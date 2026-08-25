(* In-memory queue-manager store, mirroring crates/server/src/{model,store}.rs
   from the original Rust project. Business rules (who can do what) are kept
   faithful to the original; Microsoft SSO is intentionally out of scope for
   this OCaml rewrite (password sign-in only). All mutating operations here
   run synchronously (no Lwt binds in the middle) so they are safe to call
   from any Lwt fiber without extra locking. *)

module P = Lue_shared.Protocol

let ( let* ) = Result.bind

let rejoin_cooldown_secs = 5.0

(* ---------- mutable domain types ---------- *)

type account = {
  acc_id : string;
  mutable acc_name : string;
  mutable acc_email : string;
  mutable acc_password_hash : string;
  mutable acc_role : P.account_role;
}

type session = { sess_token : string; sess_account_id : string }

type queue_entry = {
  qe_id : string;
  qe_token : string;
  qe_requester_account_id : string option;
  mutable qe_requester_label : string;
  qe_requester_email : string option;
  qe_is_guest : bool;
  qe_values : (string * string) list;
  qe_submitted_at : string;
  mutable qe_left_at : string option;
  mutable qe_status : P.entry_status;
  mutable qe_claimed_by : string option;
}

type queue = {
  q_id : string;
  mutable q_code : string;
  mutable q_name : string;
  mutable q_allow_guests : bool;
  mutable q_is_public : bool;
  mutable q_opens_at : string option;
  mutable q_weekly_schedule : P.weekly_schedule option;
  q_owner_account_id : string;
  mutable q_owner_name : string;
  mutable q_shared_account_ids : string list;
  mutable q_shared_group_ids : string list;
  mutable q_fields : P.queue_field list;
  mutable q_entries : queue_entry list;
}

type archived_queue = {
  arq_queue : queue;
  arq_closed_at : string;
  arq_closed_by_account_id : string;
  arq_closed_by_name : string;
}

type group = {
  grp_id : string;
  mutable grp_name : string;
  mutable grp_role : P.account_role;
  mutable grp_member_ids : string list;
}

type site_settings = {
  mutable ss_title : string;
  mutable ss_admin_pw : bool;
  mutable ss_admin_ms : bool;
  mutable ss_user_pw : bool;
  mutable ss_user_ms : bool;
}

let default_site_settings () =
  { ss_title = "Lue"; ss_admin_pw = true; ss_admin_ms = true; ss_user_pw = true; ss_user_ms = true }

type store = {
  mutable site_settings : site_settings;
  accounts : (string, account) Hashtbl.t;
  account_email_index : (string, string) Hashtbl.t;
  admin_sessions : (string, session) Hashtbl.t;
  user_sessions : (string, session) Hashtbl.t;
  queues : (string, queue) Hashtbl.t;
  archived_queues : (string, archived_queue) Hashtbl.t;
  queue_code_index : (string, string) Hashtbl.t;
  groups : (string, group) Hashtbl.t;
  entry_index : (string, string) Hashtbl.t;
}

let create_store () =
  {
    site_settings = default_site_settings ();
    accounts = Hashtbl.create 16;
    account_email_index = Hashtbl.create 16;
    admin_sessions = Hashtbl.create 16;
    user_sessions = Hashtbl.create 16;
    queues = Hashtbl.create 16;
    archived_queues = Hashtbl.create 16;
    queue_code_index = Hashtbl.create 16;
    groups = Hashtbl.create 16;
    entry_index = Hashtbl.create 16;
  }

let db = create_store ()

(* ---------- account role helpers ---------- *)

let is_super_admin (a : account) = a.acc_role = P.Super_admin
let can_administer (a : account) = a.acc_role = P.Super_admin || a.acc_role = P.Admin
let can_join_queues (_ : account) = true (* SuperAdmin | Admin | User can all join *)

(* ---------- queue helpers ---------- *)

let normalize_code value =
  let buf = Buffer.create (String.length value) in
  String.iter
    (fun c ->
      if (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then
        Buffer.add_char buf (Char.uppercase_ascii c))
    value;
  Buffer.contents buf

let new_code existing_codes =
  let rec go () =
    let candidate = String.uppercase_ascii (String.sub (Uid.random_hex 4) 0 6) in
    if List.mem candidate existing_codes then go () else candidate
  in
  go ()

let waiting_count (q : queue) =
  List.length (List.filter (fun (e : queue_entry) -> e.qe_status = P.Pending) q.q_entries)

let active_count (q : queue) =
  List.length
    (List.filter (fun (e : queue_entry) -> e.qe_status = P.Pending || e.qe_status = P.Claimed)
       q.q_entries)

let position_for (q : queue) entry_id =
  let rec go entries position =
    match entries with
    | [] -> None
    | (e : queue_entry) :: rest ->
        let position = if e.qe_status = P.Pending then position + 1 else position in
        if e.qe_id = entry_id then if e.qe_status = P.Pending then Some position else None
        else go rest position
  in
  go q.q_entries 0

let summary (q : queue) : P.queue_summary =
  {
    id = q.q_id; code = q.q_code; name = q.q_name; allow_guests = q.q_allow_guests;
    is_public = q.q_is_public; opens_at = q.q_opens_at; weekly_schedule = q.q_weekly_schedule;
    waiting_count = waiting_count q; active_count = active_count q;
  }

let weekday_label = function
  | 0 -> "Sunday" | 1 -> "Monday" | 2 -> "Tuesday" | 3 -> "Wednesday" | 4 -> "Thursday"
  | 5 -> "Friday" | 6 -> "Saturday" | _ -> "Unknown"

let queue_is_open (q : queue) =
  let now = Uid.now_epoch () in
  let past_one_time_open =
    match q.q_opens_at with
    | None -> true
    | Some opens_at -> ( match Uid.epoch_of_iso opens_at with Some t -> now >= t | None -> true)
  in
  if not past_one_time_open then false
  else
    match q.q_weekly_schedule with
    | None -> true
    | Some schedule ->
        let weekday, minute_of_day = Uid.now_weekday_and_minute () in
        weekday > schedule.weekday
        || (weekday = schedule.weekday && minute_of_day >= schedule.minute_of_day)

let queue_not_open_message (q : queue) =
  match q.q_opens_at with
  | Some opens_at when (match Uid.epoch_of_iso opens_at with
                        | Some t -> Uid.now_epoch () < t
                        | None -> false) ->
      Printf.sprintf "This queue opens at %s." opens_at
  | _ -> (
      match q.q_weekly_schedule with
      | Some schedule ->
          Printf.sprintf "This queue opens weekly on %s at %02d:%02d UTC."
            (weekday_label schedule.weekday)
            (schedule.minute_of_day / 60) (schedule.minute_of_day mod 60)
      | None -> "This queue is not open yet.")

(* ---------- lookups ---------- *)

let find_account id = Hashtbl.find_opt db.accounts id
let admin_account token =
  match Hashtbl.find_opt db.admin_sessions token with
  | None -> None
  | Some s -> find_account s.sess_account_id

let user_account token =
  match Hashtbl.find_opt db.user_sessions token with
  | None -> None
  | Some s -> find_account s.sess_account_id

let admin_identity token : P.admin_identity_view option =
  match Hashtbl.find_opt db.admin_sessions token with
  | None -> None
  | Some s -> (
      match find_account s.sess_account_id with
      | None -> None
      | Some a ->
          Some
            {
              ai_token = s.sess_token; account_id = a.acc_id; ai_name = a.acc_name;
              ai_email = a.acc_email; is_super_admin = is_super_admin a;
            })

let user_identity token : P.user_identity_view option =
  match Hashtbl.find_opt db.user_sessions token with
  | None -> None
  | Some s -> (
      match find_account s.sess_account_id with
      | None -> None
      | Some a -> Some { token = s.sess_token; name = a.acc_name; email = a.acc_email })

let account_can_manage_queue account_id (q : queue) =
  match find_account account_id with
  | None -> false
  | Some a ->
      is_super_admin a || q.q_owner_account_id = account_id
      || List.mem account_id q.q_shared_account_ids
      || List.exists
           (fun group_id ->
             match Hashtbl.find_opt db.groups group_id with
             | Some g -> g.grp_role = P.Admin && List.mem account_id g.grp_member_ids
             | None -> false)
           q.q_shared_group_ids

let create_admin_session account_id =
  let token = Uid.token () in
  Hashtbl.replace db.admin_sessions token { sess_token = token; sess_account_id = account_id };
  match admin_identity token with
  | Some a -> Ok a
  | None -> Error "failed to create admin session"

let create_user_session account_id =
  let token = Uid.token () in
  Hashtbl.replace db.user_sessions token { sess_token = token; sess_account_id = account_id };
  match user_identity token with
  | Some u -> Ok u
  | None -> Error "failed to create user session"

let authenticate_account email password =
  let* email = Util.normalize_email email in
  match Hashtbl.find_opt db.account_email_index email with
  | None -> Error "invalid email or password"
  | Some account_id -> (
      match find_account account_id with
      | None -> Error "invalid email or password"
      | Some a ->
          if Password.verify_password (String.trim password) a.acc_password_hash then Ok a
          else Error "invalid email or password")

(* ---------- setup / login ---------- *)

let needs_initial_setup () =
  not (Hashtbl.fold (fun _ a acc -> acc || a.acc_role = P.Super_admin) db.accounts false)

let bootstrap_super_admin name email password =
  let* email = Util.normalize_email email in
  let name = String.trim name in
  let password = String.trim password in
  if name = "" then Error "super admin name is required"
  else if password = "" then Error "super admin password is required"
  else
    let account_id =
      match Hashtbl.find_opt db.account_email_index email with
      | Some id -> id
      | None ->
          let id = Uid.uuid4 () in
          Hashtbl.replace db.account_email_index email id;
          id
    in
    let* password_hash = Password.hash_password password in
    Hashtbl.replace db.accounts account_id
      { acc_id = account_id; acc_name = name; acc_email = email; acc_password_hash = password_hash;
        acc_role = P.Super_admin };
    Ok ()

let setup_super_admin name email password =
  if not (needs_initial_setup ()) then Error "initial setup is already complete"
  else
    let* () = bootstrap_super_admin name email password in
    let* a = authenticate_account email password in
    create_admin_session a.acc_id

let login_admin email password =
  if not db.site_settings.ss_admin_pw then Error "admin password sign-in is disabled"
  else
    let* a = authenticate_account email password in
    if not (can_administer a) then Error "this account does not have admin access"
    else create_admin_session a.acc_id

let login_user email password =
  if not db.site_settings.ss_user_pw then Error "user password sign-in is disabled"
  else
    let* a = authenticate_account email password in
    if not (can_join_queues a) then Error "use a user account to join queues"
    else create_user_session a.acc_id

(* ---------- accounts ---------- *)

let cleanup_account_references () =
  let account_ids = Hashtbl.fold (fun id _ acc -> id :: acc) db.accounts [] in
  Hashtbl.iter
    (fun _ q -> q.q_shared_account_ids <- List.filter (fun id -> List.mem id account_ids) q.q_shared_account_ids)
    db.queues;
  Hashtbl.iter
    (fun _ g -> g.grp_member_ids <- List.filter (fun id -> List.mem id account_ids) g.grp_member_ids)
    db.groups;
  Hashtbl.filter_map_inplace
    (fun _ s -> if Hashtbl.mem db.accounts s.sess_account_id then Some s else None)
    db.admin_sessions;
  Hashtbl.filter_map_inplace
    (fun _ s -> if Hashtbl.mem db.accounts s.sess_account_id then Some s else None)
    db.user_sessions

let cleanup_group_references () =
  let group_ids = Hashtbl.fold (fun id _ acc -> id :: acc) db.groups [] in
  Hashtbl.iter
    (fun _ q -> q.q_shared_group_ids <- List.filter (fun id -> List.mem id group_ids) q.q_shared_group_ids)
    db.queues

let create_account admin_token name email password (role : P.account_role) =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      if not (is_super_admin admin) then Error "only the super admin can create accounts"
      else
        let normalized_name = String.trim name in
        let* normalized_email = Util.normalize_email email in
        let normalized_password = String.trim password in
        if normalized_name = "" then Error "account name is required"
        else if String.length normalized_password < 4 then
          Error "password must be at least 4 characters"
        else if Hashtbl.mem db.account_email_index normalized_email then
          Error "an account with that email already exists"
        else
          let* password_hash = Password.hash_password normalized_password in
          let id = Uid.uuid4 () in
          Hashtbl.replace db.account_email_index normalized_email id;
          Hashtbl.replace db.accounts id
            { acc_id = id; acc_name = normalized_name; acc_email = normalized_email;
              acc_password_hash = password_hash; acc_role = role };
          Ok ()

let update_account admin_token account_id name email password (role : P.account_role) =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      if not (is_super_admin admin) then Error "only the super admin can edit accounts"
      else if admin.acc_id = account_id && role <> P.Super_admin then
        Error "you cannot demote your own super admin account"
      else
        let normalized_name = String.trim name in
        let* normalized_email = Util.normalize_email email in
        if normalized_name = "" then Error "account name is required"
        else
          let email_conflict =
            match Hashtbl.find_opt db.account_email_index normalized_email with
            | Some existing_id -> existing_id <> account_id
            | None -> false
          in
          if email_conflict then Error "an account with that email already exists"
          else
            match find_account account_id with
            | None -> Error "account not found"
            | Some account ->
                Hashtbl.remove db.account_email_index account.acc_email;
                account.acc_name <- normalized_name;
                account.acc_email <- normalized_email;
                account.acc_role <- role;
                let result =
                  match password with
                  | None -> Ok ()
                  | Some password ->
                      let password = String.trim password in
                      if password = "" then Ok ()
                      else if String.length password < 4 then
                        Error "password must be at least 4 characters"
                      else
                        let* hash = Password.hash_password password in
                        account.acc_password_hash <- hash;
                        Ok ()
                in
                Hashtbl.replace db.account_email_index normalized_email account_id;
                cleanup_account_references ();
                result

let delete_account admin_token account_id =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      if not (is_super_admin admin) then Error "only the super admin can delete accounts"
      else if admin.acc_id = account_id then Error "you cannot delete your own admin account"
      else if Hashtbl.fold (fun _ q acc -> acc || q.q_owner_account_id = account_id) db.queues false
      then Error "close or reassign this account's queues before deleting it"
      else
        match find_account account_id with
        | None -> Error "account not found"
        | Some account ->
            Hashtbl.remove db.accounts account_id;
            Hashtbl.remove db.account_email_index account.acc_email;
            cleanup_account_references ();
            Ok ()

(* ---------- queues ---------- *)

let normalize_opens_at opens_at =
  match Option.map String.trim opens_at with
  | None | Some "" -> Ok None
  | Some opens_at -> (
      match Uid.epoch_of_iso opens_at with
      | None -> Error "invalid queue opening time"
      | Some epoch -> Ok (Some (Uid.iso_of_epoch epoch)))

let normalize_weekly_schedule schedule =
  match schedule with
  | None -> Ok None
  | Some (s : P.weekly_schedule) ->
      if s.weekday > 6 then Error "weekly schedule day is invalid"
      else if s.minute_of_day >= 24 * 60 then Error "weekly schedule time is invalid"
      else Ok (Some s)

let all_queue_codes () =
  Hashtbl.fold (fun _ q acc -> q.q_code :: acc) db.queues
    (Hashtbl.fold (fun _ a acc -> a.arq_queue.q_code :: acc) db.archived_queues [])

let create_queue admin_token name fields allow_guests is_public opens_at weekly_schedule =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      let normalized_name = String.trim name in
      if normalized_name = "" then Error "queue name is required"
      else
        let* fields = Util.normalize_fields fields in
        let* opens_at = normalize_opens_at opens_at in
        let* weekly_schedule = normalize_weekly_schedule weekly_schedule in
        let id = Uid.uuid4 () in
        let code = new_code (all_queue_codes ()) in
        Hashtbl.replace db.queues id
          {
            q_id = id; q_code = code; q_name = normalized_name; q_allow_guests = allow_guests;
            q_is_public = is_public; q_opens_at = opens_at; q_weekly_schedule = weekly_schedule;
            q_owner_account_id = admin.acc_id; q_owner_name = admin.acc_name;
            q_shared_account_ids = []; q_shared_group_ids = []; q_fields = fields; q_entries = [];
          };
        Hashtbl.replace db.queue_code_index code id;
        Ok id

let update_queue_settings admin_token queue_id fields allow_guests is_public opens_at weekly_schedule =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin -> (
      let* fields = Util.normalize_fields fields in
      let* opens_at = normalize_opens_at opens_at in
      let* weekly_schedule = normalize_weekly_schedule weekly_schedule in
      match Hashtbl.find_opt db.queues queue_id with
      | None -> Error "queue not found"
      | Some q ->
          if not (is_super_admin admin || q.q_owner_account_id = admin.acc_id) then
            Error "only the queue owner or super admin can edit this queue"
          else (
            q.q_fields <- fields; q.q_allow_guests <- allow_guests; q.q_is_public <- is_public;
            q.q_opens_at <- opens_at; q.q_weekly_schedule <- weekly_schedule;
            Ok ()))

let close_queue admin_token queue_id =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin -> (
      match Hashtbl.find_opt db.queues queue_id with
      | None -> Error "queue not found"
      | Some q ->
          if not (is_super_admin admin || q.q_owner_account_id = admin.acc_id) then
            Error "only the queue owner or super admin can close this queue"
          else (
            Hashtbl.remove db.queues queue_id;
            Hashtbl.remove db.queue_code_index (normalize_code q.q_code);
            List.iter (fun (e : queue_entry) -> Hashtbl.remove db.entry_index e.qe_id) q.q_entries;
            Hashtbl.replace db.archived_queues queue_id
              { arq_queue = q; arq_closed_at = Uid.now_iso (); arq_closed_by_account_id = admin.acc_id;
                arq_closed_by_name = admin.acc_name };
            Ok ()))

(* ---------- groups ---------- *)

let validated_group_members (role : P.account_role) member_ids =
  let rec go ids validated =
    match ids with
    | [] -> Ok (List.rev validated)
    | id :: rest -> (
        match find_account id with
        | None -> Error "group member account not found"
        | Some a ->
            let valid =
              match role with
              | P.Admin -> can_administer a
              | P.User -> can_join_queues a
              | P.Super_admin -> false
            in
            if not valid then Error "group members must match the group role"
            else go rest (if List.mem id validated then validated else id :: validated))
  in
  go member_ids []

let create_group admin_token name (role : P.account_role) member_ids =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      if not (can_administer admin) then Error "only admins can create groups"
      else if (not (is_super_admin admin)) && role <> P.Admin then
        Error "only the super admin can create user groups"
      else if role = P.Super_admin then Error "groups can only be created for admins or users"
      else
        let name = String.trim name in
        if name = "" then Error "group name is required"
        else
          let* member_ids = validated_group_members role member_ids in
          let id = Uid.uuid4 () in
          Hashtbl.replace db.groups id
            { grp_id = id; grp_name = name; grp_role = role; grp_member_ids = member_ids };
          Ok ()

let update_group admin_token group_id name (role : P.account_role) member_ids =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      if not (can_administer admin) then Error "only admins can edit groups"
      else
        let* () =
          if is_super_admin admin then Ok ()
          else
            match Hashtbl.find_opt db.groups group_id with
            | None -> Error "group not found"
            | Some g ->
                if g.grp_role <> P.Admin || role <> P.Admin then
                  Error "only the super admin can edit user groups"
                else Ok ()
        in
        if role = P.Super_admin then Error "groups can only be created for admins or users"
        else
          let name = String.trim name in
          if name = "" then Error "group name is required"
          else
            let* member_ids = validated_group_members role member_ids in
            match Hashtbl.find_opt db.groups group_id with
            | None -> Error "group not found"
            | Some g ->
                g.grp_name <- name; g.grp_role <- role; g.grp_member_ids <- member_ids;
                cleanup_group_references ();
                Ok ()

let delete_group admin_token group_id =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      if not (can_administer admin) then Error "only admins can delete groups"
      else
        let* () =
          if is_super_admin admin then Ok ()
          else
            match Hashtbl.find_opt db.groups group_id with
            | None -> Error "group not found"
            | Some g -> if g.grp_role <> P.Admin then Error "only the super admin can delete user groups" else Ok ()
        in
        if not (Hashtbl.mem db.groups group_id) then Error "group not found"
        else (
          Hashtbl.remove db.groups group_id;
          cleanup_group_references ();
          Ok ())

(* ---------- site settings / sharing ---------- *)

let site_settings_view () : P.site_settings_view =
  {
    site_title = db.site_settings.ss_title;
    admin_password_sign_in_enabled = db.site_settings.ss_admin_pw;
    admin_microsoft_sign_in_enabled = db.site_settings.ss_admin_ms;
    user_password_sign_in_enabled = db.site_settings.ss_user_pw;
    user_microsoft_sign_in_enabled = db.site_settings.ss_user_ms;
  }

let update_site_settings admin_token site_title admin_pw admin_ms user_pw user_ms =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      if not (is_super_admin admin) then Error "only the super admin can edit site settings"
      else
        let site_title = String.trim site_title in
        if site_title = "" then Error "site title is required"
        else if String.length site_title > 80 then Error "site title must be 80 characters or fewer"
        else (
          db.site_settings.ss_title <- site_title;
          db.site_settings.ss_admin_pw <- admin_pw;
          db.site_settings.ss_admin_ms <- admin_ms;
          db.site_settings.ss_user_pw <- user_pw;
          db.site_settings.ss_user_ms <- user_ms;
          Ok ())

let validated_share_accounts account_ids =
  let rec go ids validated =
    match ids with
    | [] -> Ok (List.rev validated)
    | id :: rest -> (
        match find_account id with
        | None -> Error "shared admin account not found"
        | Some a ->
            if not (can_administer a) then Error "queues can only be shared with admin accounts"
            else go rest (if List.mem id validated then validated else id :: validated))
  in
  go account_ids []

let validated_share_groups group_ids =
  let rec go ids validated =
    match ids with
    | [] -> Ok (List.rev validated)
    | id :: rest -> (
        match Hashtbl.find_opt db.groups id with
        | None -> Error "shared admin group not found"
        | Some g ->
            if g.grp_role <> P.Admin then Error "queues can only be shared with admin groups"
            else go rest (if List.mem id validated then validated else id :: validated))
  in
  go group_ids []

let share_queue admin_token queue_id account_ids group_ids =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin -> (
      match Hashtbl.find_opt db.queues queue_id with
      | None -> Error "queue not found"
      | Some q ->
          if not (account_can_manage_queue admin.acc_id q) then Error "you do not have access to this queue"
          else
            let* shared_account_ids = validated_share_accounts account_ids in
            let* shared_group_ids = validated_share_groups group_ids in
            q.q_shared_account_ids <- shared_account_ids;
            q.q_shared_group_ids <- shared_group_ids;
            Ok ())

(* ---------- entries: join / leave / claim / status ---------- *)

let join_queue queue_id (values : (string * string) list) user_token entry_token =
  let requester =
    match user_token with
    | Some ut -> (
        match user_account ut with
        | None -> Error "unknown user session"
        | Some a -> Ok (Some (a.acc_id, a.acc_name, a.acc_email)))
    | None -> Ok None
  in
  let* requester = requester in
  match Hashtbl.find_opt db.queues queue_id with
  | None -> Error "queue not found"
  | Some q ->
      if not (queue_is_open q) then Error (queue_not_open_message q)
      else
        let requester_account_id = Option.map (fun (id, _, _) -> id) requester in
        let active_entry =
          match requester_account_id with
          | Some account_id ->
              List.find_opt
                (fun (e : queue_entry) ->
                  e.qe_requester_account_id = Some account_id
                  && (e.qe_status = P.Pending || e.qe_status = P.Claimed))
                q.q_entries
          | None -> (
              match entry_token with
              | Some et ->
                  List.find_opt
                    (fun (e : queue_entry) ->
                      e.qe_token = et && (e.qe_status = P.Pending || e.qe_status = P.Claimed))
                    q.q_entries
              | None -> None)
        in
        match active_entry with
        | Some e -> Ok e.qe_token
        | None -> (
            (* validate / fill field values *)
            let requester_name =
              match requester with Some (_, name, _) -> Some name | None -> None
            in
            let rec build_values fields values acc =
              match fields with
              | [] -> Ok (List.rev acc)
              | (field : P.queue_field) :: rest ->
                  let value =
                    match List.assoc_opt field.key values with Some v -> String.trim v | None -> ""
                  in
                  let value =
                    if Util.is_requester_name_key field.key && value = "" then
                      match requester_name with Some n -> n | None -> value
                    else value
                  in
                  if field.required && value = "" then Error (field.label ^ " is required")
                  else if value <> "" && field.options <> [] && not (List.mem value field.options)
                  then Error (field.label ^ " must be one of the available options")
                  else build_values rest values ((field.key, value) :: acc)
            in
            let* filled_values = build_values q.q_fields values [] in
            let* requester_label, requester_email, is_guest =
              match requester with
              | Some (_, name, email) -> Ok (name, Some email, false)
              | None -> if q.q_allow_guests then Ok ("Guest", None, true) else Error "this queue requires a user account"
            in
            let requester_label =
              List.fold_left
                (fun label (field : P.queue_field) ->
                  if Util.is_requester_name_key field.key then
                    match List.assoc_opt field.key filled_values with
                    | Some v when String.trim v <> "" -> String.trim v
                    | _ -> label
                  else label)
                requester_label q.q_fields
            in
            let left_entry =
              match requester_account_id with
              | Some account_id ->
                  List.fold_left
                    (fun acc (e : queue_entry) ->
                      if e.qe_requester_account_id = Some account_id && e.qe_status = P.Left then Some e
                      else acc)
                    None q.q_entries
              | None -> (
                  match entry_token with
                  | Some et -> List.find_opt (fun (e : queue_entry) -> e.qe_token = et && e.qe_status = P.Left) q.q_entries
                  | None -> None)
            in
            let* () =
              match left_entry with
              | None -> Ok ()
              | Some e -> (
                  match e.qe_left_at with
                  | None -> Ok ()
                  | Some left_at -> (
                      match Uid.epoch_of_iso left_at with
                      | None -> Ok ()
                      | Some left_epoch ->
                          let rejoin_at = left_epoch +. rejoin_cooldown_secs in
                          let now = Uid.now_epoch () in
                          if now < rejoin_at then
                            let remaining = max 1 (int_of_float (ceil (rejoin_at -. now))) in
                            Error (Printf.sprintf "Please wait %d seconds before attempting to rejoin." remaining)
                          else Ok ()))
            in
            let id = Uid.uuid4 () in
            let token = Uid.token () in
            let entry =
              {
                qe_id = id; qe_token = token; qe_requester_account_id = requester_account_id;
                qe_requester_label = requester_label; qe_requester_email = requester_email;
                qe_is_guest = is_guest; qe_values = filled_values; qe_submitted_at = Uid.now_iso ();
                qe_left_at = None; qe_status = P.Pending; qe_claimed_by = None;
              }
            in
            q.q_entries <- q.q_entries @ [ entry ];
            Hashtbl.replace db.entry_index id queue_id;
            Ok token)

let leave_queue queue_id entry_token =
  match Hashtbl.find_opt db.queues queue_id with
  | None -> Error "queue not found"
  | Some q -> (
      match List.find_opt (fun (e : queue_entry) -> e.qe_token = entry_token) q.q_entries with
      | None -> Error "queue entry not found for leave request"
      | Some e -> (
          match e.qe_status with
          | P.Pending | P.Claimed ->
              e.qe_status <- P.Left; e.qe_claimed_by <- None; e.qe_left_at <- Some (Uid.now_iso ());
              Ok ()
          | P.Left | P.Resolved | P.Denied -> Error "queue entry is already closed"))

let find_entry_queue entry_id =
  match Hashtbl.find_opt db.entry_index entry_id with
  | None -> Error "queue entry not found"
  | Some queue_id -> (
      match Hashtbl.find_opt db.queues queue_id with
      | None -> Error "queue not found"
      | Some q -> Ok (queue_id, q))

let claim_entry admin_token entry_id =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      let* queue_id, q = find_entry_queue entry_id in
      if not (account_can_manage_queue admin.acc_id q) then Error "you do not have access to this queue"
      else
        match List.find_opt (fun (e : queue_entry) -> e.qe_id = entry_id) q.q_entries with
        | None -> Error "queue entry not found"
        | Some e -> (
            match e.qe_status with
            | P.Pending -> e.qe_status <- P.Claimed; e.qe_claimed_by <- Some admin.acc_name; Ok queue_id
            | _ -> Error "only pending requests can be claimed")

let unclaim_entry admin_token entry_id =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      let* queue_id, q = find_entry_queue entry_id in
      if not (account_can_manage_queue admin.acc_id q) then Error "you do not have access to this queue"
      else
        match List.find_opt (fun (e : queue_entry) -> e.qe_id = entry_id) q.q_entries with
        | None -> Error "queue entry not found"
        | Some e -> (
            match e.qe_status with
            | P.Claimed -> e.qe_status <- P.Pending; e.qe_claimed_by <- None; Ok queue_id
            | _ -> Error "only claimed requests can be unclaimed")

let update_entry_status admin_token entry_id (next_status : P.entry_status) =
  match admin_account admin_token with
  | None -> Error "unknown admin session"
  | Some admin ->
      let* queue_id, q = find_entry_queue entry_id in
      if not (account_can_manage_queue admin.acc_id q) then Error "you do not have access to this queue"
      else
        match List.find_opt (fun (e : queue_entry) -> e.qe_id = entry_id) q.q_entries with
        | None -> Error "queue entry not found"
        | Some e -> (
            match (e.qe_status, next_status) with
            | (P.Pending | P.Claimed), (P.Resolved | P.Denied) ->
                if e.qe_claimed_by = None then e.qe_claimed_by <- Some admin.acc_name;
                e.qe_status <- next_status;
                Ok queue_id
            | (P.Resolved | P.Denied), P.Pending when e.qe_left_at = None ->
                e.qe_status <- P.Pending; e.qe_claimed_by <- None; Ok queue_id
            | _ -> Error "invalid status transition")

(* ---------- views ---------- *)

let admin_can_see_queue admin_token queue_id =
  match (admin_account admin_token, Hashtbl.find_opt db.queues queue_id) with
  | Some admin, Some q -> account_can_manage_queue admin.acc_id q
  | _ -> false

let visible_queue_ids admin_token =
  match admin_account admin_token with
  | None -> None
  | Some admin ->
      let ids =
        Hashtbl.fold
          (fun id q acc -> if account_can_manage_queue admin.acc_id q then id :: acc else acc)
          db.queues []
      in
      let ids =
        List.sort
          (fun a b ->
            let name_of id = match Hashtbl.find_opt db.queues id with Some q -> q.q_name | None -> "" in
            compare (name_of a) (name_of b))
          ids
      in
      Some ids

let admin_entry_view (e : queue_entry) : P.admin_entry_view =
  {
    ae_id = e.qe_id; ae_status = e.qe_status; ae_submitted_at = e.qe_submitted_at;
    ae_claimed_by = e.qe_claimed_by; ae_requester_label = e.qe_requester_label;
    ae_requester_email = e.qe_requester_email; ae_is_guest = e.qe_is_guest; ae_values = e.qe_values;
  }

let admin_queue_view admin_token queue_id : P.admin_queue_view option =
  if not (admin_can_see_queue admin_token queue_id) then None
  else
    match Hashtbl.find_opt db.queues queue_id with
    | None -> None
    | Some q ->
        Some
          {
            sel_summary = summary q; sel_owner_name = q.q_owner_name;
            sel_owner_account_id = q.q_owner_account_id;
            sel_shared_account_ids = q.q_shared_account_ids; sel_shared_group_ids = q.q_shared_group_ids;
            sel_fields = q.q_fields; sel_entries = List.map admin_entry_view q.q_entries;
          }

let admin_state admin_token selected_queue_id : P.admin_state_view option =
  match admin_identity admin_token with
  | None -> None
  | Some admin -> (
      match visible_queue_ids admin_token with
      | None -> None
      | Some visible_ids ->
          let queues =
            List.filter_map
              (fun id ->
                match Hashtbl.find_opt db.queues id with
                | None -> None
                | Some q ->
                    Some
                      {
                        P.aq_summary = summary q; aq_owner_name = q.q_owner_name;
                        aq_shared_account_ids = q.q_shared_account_ids;
                        aq_shared_group_ids = q.q_shared_group_ids;
                      })
              visible_ids
          in
          let fallback_queue_id = match queues with q :: _ -> Some q.aq_summary.id | [] -> None in
          let selected_queue =
            match selected_queue_id with
            | Some qid -> (
                match admin_queue_view admin_token qid with
                | Some v -> Some v
                | None -> (
                    match fallback_queue_id with
                    | Some qid -> admin_queue_view admin_token qid
                    | None -> None))
            | None -> (
                match fallback_queue_id with
                | Some qid -> admin_queue_view admin_token qid
                | None -> None)
          in
          let archived_queues =
            Hashtbl.fold
              (fun _ (a : archived_queue) acc ->
                if account_can_manage_queue admin.account_id a.arq_queue then
                  {
                    P.arc_summary = summary a.arq_queue; arc_owner_name = a.arq_queue.q_owner_name;
                    arc_closed_at = a.arq_closed_at; arc_closed_by_name = a.arq_closed_by_name;
                    arc_entry_count = List.length a.arq_queue.q_entries; arc_fields = a.arq_queue.q_fields;
                    arc_entries = List.map admin_entry_view a.arq_queue.q_entries;
                  }
                  :: acc
                else acc)
              db.archived_queues []
          in
          let archived_queues = List.sort (fun a b -> compare b.P.arc_closed_at a.P.arc_closed_at) archived_queues in
          let accounts =
            Hashtbl.fold
              (fun _ a acc ->
                if admin.is_super_admin || can_administer a then
                  { P.id = a.acc_id; name = a.acc_name; email = a.acc_email; role = a.acc_role } :: acc
                else acc)
              db.accounts []
          in
          let accounts = List.sort (fun a b -> compare a.P.email b.P.email) accounts in
          let groups =
            Hashtbl.fold
              (fun _ g acc ->
                if admin.is_super_admin || g.grp_role = P.Admin then
                  { P.g_id = g.grp_id; g_name = g.grp_name; g_role = g.grp_role; g_member_ids = g.grp_member_ids }
                  :: acc
                else acc)
              db.groups []
          in
          let groups = List.sort (fun a b -> compare a.P.g_name b.P.g_name) groups in
          Some
            {
              P.as_admin = admin; as_site_settings = site_settings_view (); as_queues = queues;
              as_archived_queues = archived_queues; as_selected_queue = selected_queue;
              as_accounts = accounts; as_groups = groups;
            })

let rejoin_after_timestamp left_at =
  match left_at with
  | None -> None
  | Some left_at -> (
      match Uid.epoch_of_iso left_at with
      | None -> None
      | Some t -> Some (Uid.iso_of_epoch (t +. rejoin_cooldown_secs)))

let user_queue_view (q : queue) entry_token closed =
  let your_entry =
    match entry_token with
    | None -> None
    | Some token -> (
        match List.find_opt (fun (e : queue_entry) -> e.qe_token = token) q.q_entries with
        | None -> None
        | Some e ->
            Some
              {
                P.ue_id = e.qe_id; ue_token = e.qe_token; ue_status = e.qe_status;
                ue_claimed_by = e.qe_claimed_by; ue_values = e.qe_values; ue_submitted_at = e.qe_submitted_at;
                ue_left_at = e.qe_left_at; ue_rejoin_after = rejoin_after_timestamp e.qe_left_at;
                ue_position = position_for q e.qe_id; ue_requester_label = e.qe_requester_label;
                ue_is_guest = e.qe_is_guest;
              })
  in
  let closed_at, closed_by_name =
    match closed with Some (a, b) -> (Some a, Some b) | None -> (None, None)
  in
  ( {
      P.uq_id = q.q_id; uq_code = q.q_code; uq_name = q.q_name; uq_fields = q.q_fields;
      uq_allow_guests = q.q_allow_guests; uq_opens_at = q.q_opens_at; uq_weekly_schedule = q.q_weekly_schedule;
      uq_waiting_count = waiting_count q; uq_closed_at = closed_at; uq_closed_by_name = closed_by_name;
    },
    your_entry )

let user_view queue_id entry_token =
  match Hashtbl.find_opt db.queues queue_id with
  | Some q -> if not (queue_is_open q) then None else Some (user_queue_view q entry_token None)
  | None -> (
      match Hashtbl.find_opt db.archived_queues queue_id with
      | None -> None
      | Some a -> Some (user_queue_view a.arq_queue entry_token (Some (a.arq_closed_at, a.arq_closed_by_name))))

let public_queues () =
  let queues =
    Hashtbl.fold (fun _ q acc -> if q.q_is_public && queue_is_open q then summary q :: acc else acc) db.queues []
  in
  List.sort (fun (a : P.queue_summary) b -> compare a.name b.name) queues

let queue_unavailable_message queue_id =
  match Hashtbl.find_opt db.queues queue_id with
  | None -> None
  | Some q -> if queue_is_open q then None else Some (queue_not_open_message q)

let queue_id_for_code code = Hashtbl.find_opt db.queue_code_index (normalize_code code)

(* ---------- persistence ---------- *)

let mkdir_p path =
  let parts = String.split_on_char '/' path in
  let _ =
    List.fold_left
      (fun acc part ->
        let dir = if acc = "" then part else acc ^ "/" ^ part in
        if dir <> "" then (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> () | _ -> ());
        dir)
      "" parts
  in
  ()

let json_of_account (a : account) =
  `Assoc
    [
      ("id", `String a.acc_id); ("name", `String a.acc_name); ("email", `String a.acc_email);
      ("password_hash", `String a.acc_password_hash); ("role", P.json_of_role a.acc_role);
    ]

let account_of_json j =
  let open Yojson.Safe.Util in
  {
    acc_id = to_string (member "id" j); acc_name = to_string (member "name" j);
    acc_email = to_string (member "email" j); acc_password_hash = to_string (member "password_hash" j);
    acc_role = P.role_of_json (member "role" j);
  }

let json_of_session (s : session) =
  `Assoc [ ("token", `String s.sess_token); ("account_id", `String s.sess_account_id) ]

let session_of_json j =
  let open Yojson.Safe.Util in
  { sess_token = to_string (member "token" j); sess_account_id = to_string (member "account_id" j) }

let json_of_qentry (e : queue_entry) =
  `Assoc
    [
      ("id", `String e.qe_id); ("token", `String e.qe_token);
      ("requester_account_id", P.opt_json (fun s -> `String s) e.qe_requester_account_id);
      ("requester_label", `String e.qe_requester_label);
      ("requester_email", P.opt_json (fun s -> `String s) e.qe_requester_email);
      ("is_guest", `Bool e.qe_is_guest); ("values", P.json_of_values e.qe_values);
      ("submitted_at", `String e.qe_submitted_at);
      ("left_at", P.opt_json (fun s -> `String s) e.qe_left_at);
      ("status", P.json_of_status e.qe_status);
      ("claimed_by", P.opt_json (fun s -> `String s) e.qe_claimed_by);
    ]

let qentry_of_json j =
  let open Yojson.Safe.Util in
  {
    qe_id = to_string (member "id" j); qe_token = to_string (member "token" j);
    qe_requester_account_id = P.opt_field "requester_account_id" to_string j;
    qe_requester_label = to_string (member "requester_label" j);
    qe_requester_email = P.opt_field "requester_email" to_string j;
    qe_is_guest = to_bool (member "is_guest" j); qe_values = P.assoc_of_values (member "values" j);
    qe_submitted_at = to_string (member "submitted_at" j);
    qe_left_at = P.opt_field "left_at" to_string j; qe_status = P.status_of_json (member "status" j);
    qe_claimed_by = P.opt_field "claimed_by" to_string j;
  }

let json_of_queue (q : queue) =
  `Assoc
    [
      ("id", `String q.q_id); ("code", `String q.q_code); ("name", `String q.q_name);
      ("allow_guests", `Bool q.q_allow_guests); ("is_public", `Bool q.q_is_public);
      ("opens_at", P.opt_json (fun s -> `String s) q.q_opens_at);
      ("weekly_schedule", P.opt_json P.json_of_schedule q.q_weekly_schedule);
      ("owner_account_id", `String q.q_owner_account_id); ("owner_name", `String q.q_owner_name);
      ("shared_account_ids", P.json_of_list (fun s -> `String s) q.q_shared_account_ids);
      ("shared_group_ids", P.json_of_list (fun s -> `String s) q.q_shared_group_ids);
      ("fields", P.json_of_list P.json_of_field q.q_fields);
      ("entries", P.json_of_list json_of_qentry q.q_entries);
    ]

let queue_of_json j =
  let open Yojson.Safe.Util in
  {
    q_id = to_string (member "id" j); q_code = to_string (member "code" j);
    q_name = to_string (member "name" j); q_allow_guests = to_bool (member "allow_guests" j);
    q_is_public = to_bool (member "is_public" j); q_opens_at = P.opt_field "opens_at" to_string j;
    q_weekly_schedule = P.opt_field "weekly_schedule" P.schedule_of_json j;
    q_owner_account_id = to_string (member "owner_account_id" j);
    q_owner_name = to_string (member "owner_name" j);
    q_shared_account_ids = P.list_of to_string (member "shared_account_ids" j);
    q_shared_group_ids = P.list_of to_string (member "shared_group_ids" j);
    q_fields = P.list_of P.field_of_json (member "fields" j);
    q_entries = P.list_of qentry_of_json (member "entries" j);
  }

let json_of_archived (a : archived_queue) =
  `Assoc
    [
      ("queue", json_of_queue a.arq_queue); ("closed_at", `String a.arq_closed_at);
      ("closed_by_account_id", `String a.arq_closed_by_account_id);
      ("closed_by_name", `String a.arq_closed_by_name);
    ]

let archived_of_json j =
  let open Yojson.Safe.Util in
  {
    arq_queue = queue_of_json (member "queue" j); arq_closed_at = to_string (member "closed_at" j);
    arq_closed_by_account_id = to_string (member "closed_by_account_id" j);
    arq_closed_by_name = to_string (member "closed_by_name" j);
  }

let json_of_grp (g : group) =
  `Assoc
    [
      ("id", `String g.grp_id); ("name", `String g.grp_name); ("role", P.json_of_role g.grp_role);
      ("member_ids", P.json_of_list (fun s -> `String s) g.grp_member_ids);
    ]

let grp_of_json j =
  let open Yojson.Safe.Util in
  {
    grp_id = to_string (member "id" j); grp_name = to_string (member "name" j);
    grp_role = P.role_of_json (member "role" j);
    grp_member_ids = P.list_of to_string (member "member_ids" j);
  }

let json_of_site_settings_full (s : site_settings) =
  `Assoc
    [
      ("site_title", `String s.ss_title); ("admin_password_sign_in_enabled", `Bool s.ss_admin_pw);
      ("admin_microsoft_sign_in_enabled", `Bool s.ss_admin_ms);
      ("user_password_sign_in_enabled", `Bool s.ss_user_pw);
      ("user_microsoft_sign_in_enabled", `Bool s.ss_user_ms);
    ]

let site_settings_of_json_full j =
  let open Yojson.Safe.Util in
  {
    ss_title = to_string (member "site_title" j);
    ss_admin_pw = to_bool (member "admin_password_sign_in_enabled" j);
    ss_admin_ms = to_bool (member "admin_microsoft_sign_in_enabled" j);
    ss_user_pw = to_bool (member "user_password_sign_in_enabled" j);
    ss_user_ms = to_bool (member "user_microsoft_sign_in_enabled" j);
  }

let rebuild_indexes () =
  Hashtbl.reset db.account_email_index;
  Hashtbl.iter (fun id a -> Hashtbl.replace db.account_email_index a.acc_email id) db.accounts;
  Hashtbl.reset db.queue_code_index;
  Hashtbl.iter (fun id q -> Hashtbl.replace db.queue_code_index (normalize_code q.q_code) id) db.queues;
  Hashtbl.reset db.entry_index;
  Hashtbl.iter
    (fun id q -> List.iter (fun (e : queue_entry) -> Hashtbl.replace db.entry_index e.qe_id id) q.q_entries)
    db.queues

let save_to_disk path =
  mkdir_p (Filename.dirname path);
  let snapshot =
    `Assoc
      [
        ("site_settings", json_of_site_settings_full db.site_settings);
        ("accounts", `Assoc (Hashtbl.fold (fun id a acc -> (id, json_of_account a) :: acc) db.accounts []));
        ("queues", `Assoc (Hashtbl.fold (fun id q acc -> (id, json_of_queue q) :: acc) db.queues []));
        ( "archived_queues",
          `Assoc (Hashtbl.fold (fun id a acc -> (id, json_of_archived a) :: acc) db.archived_queues []) );
        ("groups", `Assoc (Hashtbl.fold (fun id g acc -> (id, json_of_grp g) :: acc) db.groups []));
        ( "admin_sessions",
          `Assoc (Hashtbl.fold (fun tok s acc -> (tok, json_of_session s) :: acc) db.admin_sessions []) );
        ( "user_sessions",
          `Assoc (Hashtbl.fold (fun tok s acc -> (tok, json_of_session s) :: acc) db.user_sessions []) );
      ]
  in
  let contents = Yojson.Safe.pretty_to_string snapshot in
  let tmp_path = path ^ ".tmp" in
  let oc = open_out tmp_path in
  output_string oc contents;
  close_out oc;
  Sys.rename tmp_path path

let load_from_disk path =
  if not (Sys.file_exists path) then ()
  else
    let ic = open_in path in
    let n = in_channel_length ic in
    let contents = really_input_string ic n in
    close_in ic;
    let open Yojson.Safe.Util in
    let j = Yojson.Safe.from_string contents in
    (match P.member_opt "site_settings" j with
    | Some s when s <> `Null -> db.site_settings <- site_settings_of_json_full s
    | _ -> ());
    (match P.member_opt "accounts" j with
    | Some (`Assoc entries) ->
        List.iter (fun (id, v) -> Hashtbl.replace db.accounts id (account_of_json v)) entries
    | _ -> ());
    (match P.member_opt "queues" j with
    | Some (`Assoc entries) -> List.iter (fun (id, v) -> Hashtbl.replace db.queues id (queue_of_json v)) entries
    | _ -> ());
    (match P.member_opt "archived_queues" j with
    | Some (`Assoc entries) ->
        List.iter (fun (id, v) -> Hashtbl.replace db.archived_queues id (archived_of_json v)) entries
    | _ -> ());
    (match P.member_opt "groups" j with
    | Some (`Assoc entries) -> List.iter (fun (id, v) -> Hashtbl.replace db.groups id (grp_of_json v)) entries
    | _ -> ());
    (match P.member_opt "admin_sessions" j with
    | Some (`Assoc entries) ->
        List.iter (fun (tok, v) -> Hashtbl.replace db.admin_sessions tok (session_of_json v)) entries
    | _ -> ());
    (match P.member_opt "user_sessions" j with
    | Some (`Assoc entries) ->
        List.iter (fun (tok, v) -> Hashtbl.replace db.user_sessions tok (session_of_json v)) entries
    | _ -> ());
    (* prune sessions pointing at deleted accounts, hash any legacy plaintext
       passwords, and rebuild derived indexes -- mirrors persistence.rs *)
    Hashtbl.filter_map_inplace
      (fun _ s -> if Hashtbl.mem db.accounts s.sess_account_id then Some s else None)
      db.admin_sessions;
    Hashtbl.filter_map_inplace
      (fun _ s -> if Hashtbl.mem db.accounts s.sess_account_id then Some s else None)
      db.user_sessions;
    Hashtbl.iter
      (fun _ a ->
        if not (Password.is_password_hash a.acc_password_hash) then
          match Password.hash_password a.acc_password_hash with
          | Ok h -> a.acc_password_hash <- h
          | Error _ -> ())
      db.accounts;
    rebuild_indexes ()

let data_path = ref "data/store.json"

let save () = try save_to_disk !data_path with e -> Printf.eprintf "failed to save store: %s\n%!" (Printexc.to_string e)
