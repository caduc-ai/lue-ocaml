(* Shared protocol types and JSON codecs used by both the Dream backend and
   the Bonsai frontend. This is a from-scratch OCaml redesign of the wire
   protocol used by the original Rust/Axum + Dioxus app; it is not
   byte-compatible with the Rust JSON shape, only internally consistent
   between our own client and server. *)

open Yojson.Safe.Util

(* ---------- small helpers ---------- *)

let j_str s = `String s
let j_bool b = `Bool b
let j_int i = `Int i
let str = to_string
let bool_ = to_bool
let int_ = to_int

let opt_json f = function None -> `Null | Some v -> f v
let opt_of_json f = function `Null -> None | j -> Some (f j)

let member_opt name j = try Some (member name j) with _ -> None

let opt_field name f j =
  match member_opt name j with
  | None | Some `Null -> None
  | Some v -> Some (f v)

let list_of f j = j |> to_list |> List.map f
let json_of_list f l = `List (List.map f l)

let assoc_of_values j =
  j |> to_assoc |> List.map (fun (k, v) -> (k, to_string v))

let json_of_values (values : (string * string) list) =
  `Assoc (List.map (fun (k, v) -> (k, `String v)) values)

(* ---------- core enums / records ---------- *)

type account_role = Super_admin | Admin | User

let account_role_to_string = function
  | Super_admin -> "super_admin"
  | Admin -> "admin"
  | User -> "user"

let account_role_of_string = function
  | "super_admin" -> Super_admin
  | "admin" -> Admin
  | "user" -> User
  | other -> failwith ("unknown account role: " ^ other)

let json_of_role r = j_str (account_role_to_string r)
let role_of_json j = account_role_of_string (str j)

type account_view = {
  id : string;
  name : string;
  email : string;
  role : account_role;
}

let json_of_account_view (a : account_view) =
  `Assoc
    [ ("id", j_str a.id); ("name", j_str a.name); ("email", j_str a.email);
      ("role", json_of_role a.role) ]

let account_view_of_json j =
  {
    id = str (member "id" j);
    name = str (member "name" j);
    email = str (member "email" j);
    role = role_of_json (member "role" j);
  }

type user_identity_view = { token : string; name : string; email : string }

let json_of_user_identity (u : user_identity_view) =
  `Assoc [ ("token", j_str u.token); ("name", j_str u.name); ("email", j_str u.email) ]

let user_identity_of_json j =
  { token = str (member "token" j); name = str (member "name" j);
    email = str (member "email" j) }

type queue_field = {
  key : string;
  label : string;
  required : bool;
  options : string list;
}

let json_of_field (f : queue_field) =
  `Assoc
    [ ("key", j_str f.key); ("label", j_str f.label); ("required", j_bool f.required);
      ("options", json_of_list j_str f.options) ]

let field_of_json j =
  {
    key = str (member "key" j);
    label = str (member "label" j);
    required = bool_ (member "required" j);
    options =
      (match member_opt "options" j with
      | Some (`List _ as l) -> list_of str l
      | _ -> []);
  }

type weekly_schedule = { weekday : int; minute_of_day : int }

let json_of_schedule (s : weekly_schedule) =
  `Assoc [ ("weekday", j_int s.weekday); ("minute_of_day", j_int s.minute_of_day) ]

let schedule_of_json j =
  { weekday = int_ (member "weekday" j); minute_of_day = int_ (member "minute_of_day" j) }

type entry_status = Pending | Claimed | Left | Resolved | Denied

let status_to_string = function
  | Pending -> "pending"
  | Claimed -> "claimed"
  | Left -> "left"
  | Resolved -> "resolved"
  | Denied -> "denied"

let status_of_string = function
  | "pending" -> Pending
  | "claimed" -> Claimed
  | "left" -> Left
  | "resolved" -> Resolved
  | "denied" -> Denied
  | other -> failwith ("unknown entry status: " ^ other)

let json_of_status s = j_str (status_to_string s)
let status_of_json j = status_of_string (str j)

type queue_summary = {
  id : string;
  code : string;
  name : string;
  allow_guests : bool;
  is_public : bool;
  opens_at : string option;
  weekly_schedule : weekly_schedule option;
  waiting_count : int;
  active_count : int;
}

let json_of_summary (s : queue_summary) =
  `Assoc
    [
      ("id", j_str s.id); ("code", j_str s.code); ("name", j_str s.name);
      ("allow_guests", j_bool s.allow_guests); ("is_public", j_bool s.is_public);
      ("opens_at", opt_json j_str s.opens_at);
      ("weekly_schedule", opt_json json_of_schedule s.weekly_schedule);
      ("waiting_count", j_int s.waiting_count); ("active_count", j_int s.active_count);
    ]

let summary_of_json j =
  {
    id = str (member "id" j); code = str (member "code" j); name = str (member "name" j);
    allow_guests = bool_ (member "allow_guests" j); is_public = bool_ (member "is_public" j);
    opens_at = opt_field "opens_at" str j;
    weekly_schedule = opt_field "weekly_schedule" schedule_of_json j;
    waiting_count = int_ (member "waiting_count" j); active_count = int_ (member "active_count" j);
  }

type user_queue_view = {
  uq_id : string;
  uq_code : string;
  uq_name : string;
  uq_fields : queue_field list;
  uq_allow_guests : bool;
  uq_opens_at : string option;
  uq_weekly_schedule : weekly_schedule option;
  uq_waiting_count : int;
  uq_closed_at : string option;
  uq_closed_by_name : string option;
}

let json_of_user_queue (q : user_queue_view) =
  `Assoc
    [
      ("id", j_str q.uq_id); ("code", j_str q.uq_code); ("name", j_str q.uq_name);
      ("fields", json_of_list json_of_field q.uq_fields);
      ("allow_guests", j_bool q.uq_allow_guests);
      ("opens_at", opt_json j_str q.uq_opens_at);
      ("weekly_schedule", opt_json json_of_schedule q.uq_weekly_schedule);
      ("waiting_count", j_int q.uq_waiting_count);
      ("closed_at", opt_json j_str q.uq_closed_at);
      ("closed_by_name", opt_json j_str q.uq_closed_by_name);
    ]

let user_queue_of_json j =
  {
    uq_id = str (member "id" j); uq_code = str (member "code" j); uq_name = str (member "name" j);
    uq_fields = list_of field_of_json (member "fields" j);
    uq_allow_guests = bool_ (member "allow_guests" j);
    uq_opens_at = opt_field "opens_at" str j;
    uq_weekly_schedule = opt_field "weekly_schedule" schedule_of_json j;
    uq_waiting_count = int_ (member "waiting_count" j);
    uq_closed_at = opt_field "closed_at" str j;
    uq_closed_by_name = opt_field "closed_by_name" str j;
  }

type user_entry_view = {
  ue_id : string;
  ue_token : string;
  ue_status : entry_status;
  ue_claimed_by : string option;
  ue_values : (string * string) list;
  ue_submitted_at : string;
  ue_left_at : string option;
  ue_rejoin_after : string option;
  ue_position : int option;
  ue_requester_label : string;
  ue_is_guest : bool;
}

let json_of_user_entry (e : user_entry_view) =
  `Assoc
    [
      ("id", j_str e.ue_id); ("token", j_str e.ue_token); ("status", json_of_status e.ue_status);
      ("claimed_by", opt_json j_str e.ue_claimed_by);
      ("values", json_of_values e.ue_values);
      ("submitted_at", j_str e.ue_submitted_at);
      ("left_at", opt_json j_str e.ue_left_at);
      ("rejoin_after", opt_json j_str e.ue_rejoin_after);
      ("position", opt_json j_int e.ue_position);
      ("requester_label", j_str e.ue_requester_label);
      ("is_guest", j_bool e.ue_is_guest);
    ]

let user_entry_of_json j =
  {
    ue_id = str (member "id" j); ue_token = str (member "token" j);
    ue_status = status_of_json (member "status" j);
    ue_claimed_by = opt_field "claimed_by" str j;
    ue_values = assoc_of_values (member "values" j);
    ue_submitted_at = str (member "submitted_at" j);
    ue_left_at = opt_field "left_at" str j;
    ue_rejoin_after = opt_field "rejoin_after" str j;
    ue_position = opt_field "position" int_ j;
    ue_requester_label = str (member "requester_label" j);
    ue_is_guest = bool_ (member "is_guest" j);
  }

type site_settings_view = {
  site_title : string;
  admin_password_sign_in_enabled : bool;
  admin_microsoft_sign_in_enabled : bool;
  user_password_sign_in_enabled : bool;
  user_microsoft_sign_in_enabled : bool;
}

let json_of_site_settings (s : site_settings_view) =
  `Assoc
    [
      ("site_title", j_str s.site_title);
      ("admin_password_sign_in_enabled", j_bool s.admin_password_sign_in_enabled);
      ("admin_microsoft_sign_in_enabled", j_bool s.admin_microsoft_sign_in_enabled);
      ("user_password_sign_in_enabled", j_bool s.user_password_sign_in_enabled);
      ("user_microsoft_sign_in_enabled", j_bool s.user_microsoft_sign_in_enabled);
    ]

let site_settings_of_json j =
  {
    site_title = str (member "site_title" j);
    admin_password_sign_in_enabled = bool_ (member "admin_password_sign_in_enabled" j);
    admin_microsoft_sign_in_enabled = bool_ (member "admin_microsoft_sign_in_enabled" j);
    user_password_sign_in_enabled = bool_ (member "user_password_sign_in_enabled" j);
    user_microsoft_sign_in_enabled = bool_ (member "user_microsoft_sign_in_enabled" j);
  }

type admin_identity_view = {
  ai_token : string;
  account_id : string;
  ai_name : string;
  ai_email : string;
  is_super_admin : bool;
}

let json_of_admin_identity (a : admin_identity_view) =
  `Assoc
    [
      ("token", j_str a.ai_token); ("account_id", j_str a.account_id);
      ("name", j_str a.ai_name); ("email", j_str a.ai_email);
      ("is_super_admin", j_bool a.is_super_admin);
    ]

let admin_identity_of_json j =
  {
    ai_token = str (member "token" j); account_id = str (member "account_id" j);
    ai_name = str (member "name" j); ai_email = str (member "email" j);
    is_super_admin = bool_ (member "is_super_admin" j);
  }

type admin_queue_list_item = {
  aq_summary : queue_summary;
  aq_owner_name : string;
  aq_shared_account_ids : string list;
  aq_shared_group_ids : string list;
}

let json_of_queue_list_item (q : admin_queue_list_item) =
  `Assoc
    [
      ("summary", json_of_summary q.aq_summary); ("owner_name", j_str q.aq_owner_name);
      ("shared_account_ids", json_of_list j_str q.aq_shared_account_ids);
      ("shared_group_ids", json_of_list j_str q.aq_shared_group_ids);
    ]

let queue_list_item_of_json j =
  {
    aq_summary = summary_of_json (member "summary" j);
    aq_owner_name = str (member "owner_name" j);
    aq_shared_account_ids = list_of str (member "shared_account_ids" j);
    aq_shared_group_ids = list_of str (member "shared_group_ids" j);
  }

type admin_entry_view = {
  ae_id : string;
  ae_status : entry_status;
  ae_submitted_at : string;
  ae_claimed_by : string option;
  ae_requester_label : string;
  ae_requester_email : string option;
  ae_is_guest : bool;
  ae_values : (string * string) list;
}

let json_of_admin_entry (e : admin_entry_view) =
  `Assoc
    [
      ("id", j_str e.ae_id); ("status", json_of_status e.ae_status);
      ("submitted_at", j_str e.ae_submitted_at);
      ("claimed_by", opt_json j_str e.ae_claimed_by);
      ("requester_label", j_str e.ae_requester_label);
      ("requester_email", opt_json j_str e.ae_requester_email);
      ("is_guest", j_bool e.ae_is_guest);
      ("values", json_of_values e.ae_values);
    ]

let admin_entry_of_json j =
  {
    ae_id = str (member "id" j); ae_status = status_of_json (member "status" j);
    ae_submitted_at = str (member "submitted_at" j);
    ae_claimed_by = opt_field "claimed_by" str j;
    ae_requester_label = str (member "requester_label" j);
    ae_requester_email = opt_field "requester_email" str j;
    ae_is_guest = bool_ (member "is_guest" j);
    ae_values = assoc_of_values (member "values" j);
  }

type archived_queue_list_item = {
  arc_summary : queue_summary;
  arc_owner_name : string;
  arc_closed_at : string;
  arc_closed_by_name : string;
  arc_entry_count : int;
  arc_fields : queue_field list;
  arc_entries : admin_entry_view list;
}

let json_of_archived (a : archived_queue_list_item) =
  `Assoc
    [
      ("summary", json_of_summary a.arc_summary); ("owner_name", j_str a.arc_owner_name);
      ("closed_at", j_str a.arc_closed_at); ("closed_by_name", j_str a.arc_closed_by_name);
      ("entry_count", j_int a.arc_entry_count);
      ("fields", json_of_list json_of_field a.arc_fields);
      ("entries", json_of_list json_of_admin_entry a.arc_entries);
    ]

let archived_of_json j =
  {
    arc_summary = summary_of_json (member "summary" j);
    arc_owner_name = str (member "owner_name" j);
    arc_closed_at = str (member "closed_at" j);
    arc_closed_by_name = str (member "closed_by_name" j);
    arc_entry_count = int_ (member "entry_count" j);
    arc_fields = list_of field_of_json (member "fields" j);
    arc_entries = list_of admin_entry_of_json (member "entries" j);
  }

type group_view = {
  g_id : string;
  g_name : string;
  g_role : account_role;
  g_member_ids : string list;
}

let json_of_group (g : group_view) =
  `Assoc
    [
      ("id", j_str g.g_id); ("name", j_str g.g_name); ("role", json_of_role g.g_role);
      ("member_ids", json_of_list j_str g.g_member_ids);
    ]

let group_of_json j =
  {
    g_id = str (member "id" j); g_name = str (member "name" j);
    g_role = role_of_json (member "role" j);
    g_member_ids = list_of str (member "member_ids" j);
  }

type admin_queue_view = {
  sel_summary : queue_summary;
  sel_owner_name : string;
  sel_owner_account_id : string;
  sel_shared_account_ids : string list;
  sel_shared_group_ids : string list;
  sel_fields : queue_field list;
  sel_entries : admin_entry_view list;
}

let json_of_admin_queue (q : admin_queue_view) =
  `Assoc
    [
      ("summary", json_of_summary q.sel_summary); ("owner_name", j_str q.sel_owner_name);
      ("owner_account_id", j_str q.sel_owner_account_id);
      ("shared_account_ids", json_of_list j_str q.sel_shared_account_ids);
      ("shared_group_ids", json_of_list j_str q.sel_shared_group_ids);
      ("fields", json_of_list json_of_field q.sel_fields);
      ("entries", json_of_list json_of_admin_entry q.sel_entries);
    ]

let admin_queue_of_json j =
  {
    sel_summary = summary_of_json (member "summary" j);
    sel_owner_name = str (member "owner_name" j);
    sel_owner_account_id = str (member "owner_account_id" j);
    sel_shared_account_ids = list_of str (member "shared_account_ids" j);
    sel_shared_group_ids = list_of str (member "shared_group_ids" j);
    sel_fields = list_of field_of_json (member "fields" j);
    sel_entries = list_of admin_entry_of_json (member "entries" j);
  }

type admin_state_view = {
  as_admin : admin_identity_view;
  as_site_settings : site_settings_view;
  as_queues : admin_queue_list_item list;
  as_archived_queues : archived_queue_list_item list;
  as_selected_queue : admin_queue_view option;
  as_accounts : account_view list;
  as_groups : group_view list;
}

let json_of_admin_state (s : admin_state_view) =
  `Assoc
    [
      ("admin", json_of_admin_identity s.as_admin);
      ("site_settings", json_of_site_settings s.as_site_settings);
      ("queues", json_of_list json_of_queue_list_item s.as_queues);
      ("archived_queues", json_of_list json_of_archived s.as_archived_queues);
      ("selected_queue", opt_json json_of_admin_queue s.as_selected_queue);
      ("accounts", json_of_list json_of_account_view s.as_accounts);
      ("groups", json_of_list json_of_group s.as_groups);
    ]

let admin_state_of_json j =
  {
    as_admin = admin_identity_of_json (member "admin" j);
    as_site_settings = site_settings_of_json (member "site_settings" j);
    as_queues = list_of queue_list_item_of_json (member "queues" j);
    as_archived_queues = list_of archived_of_json (member "archived_queues" j);
    as_selected_queue = opt_field "selected_queue" admin_queue_of_json j;
    as_accounts = list_of account_view_of_json (member "accounts" j);
    as_groups = list_of group_of_json (member "groups" j);
  }

(* ---------- client -> server messages ---------- *)

type client_message =
  | Check_setup
  | List_public_queues
  | Resolve_queue_code of { code : string }
  | Setup_super_admin of { name : string; email : string; password : string }
  | Login_admin of { email : string; password : string }
  | Login_user of { email : string; password : string }
  | Subscribe_admin of { admin_token : string; selected_queue_id : string option }
  | Create_queue of {
      admin_token : string;
      name : string;
      fields : queue_field list;
      allow_guests : bool;
      is_public : bool;
      opens_at : string option;
      weekly_schedule : weekly_schedule option;
    }
  | Update_queue_settings of {
      admin_token : string;
      queue_id : string;
      fields : queue_field list;
      allow_guests : bool;
      is_public : bool;
      opens_at : string option;
      weekly_schedule : weekly_schedule option;
    }
  | Create_account of {
      admin_token : string;
      name : string;
      email : string;
      password : string;
      role : account_role;
    }
  | Update_account of {
      admin_token : string;
      account_id : string;
      name : string;
      email : string;
      password : string option;
      role : account_role;
    }
  | Delete_account of { admin_token : string; account_id : string }
  | Create_group of {
      admin_token : string;
      name : string;
      role : account_role;
      member_ids : string list;
    }
  | Update_group of {
      admin_token : string;
      group_id : string;
      name : string;
      role : account_role;
      member_ids : string list;
    }
  | Delete_group of { admin_token : string; group_id : string }
  | Update_site_settings of {
      admin_token : string;
      site_title : string;
      admin_password_sign_in_enabled : bool;
      admin_microsoft_sign_in_enabled : bool;
      user_password_sign_in_enabled : bool;
      user_microsoft_sign_in_enabled : bool;
    }
  | Share_queue of {
      admin_token : string;
      queue_id : string;
      account_ids : string list;
      group_ids : string list;
    }
  | Close_queue of { admin_token : string; queue_id : string }
  | Claim_entry of { admin_token : string; entry_id : string }
  | Unclaim_entry of { admin_token : string; entry_id : string }
  | Resolve_entry of { admin_token : string; entry_id : string }
  | Deny_entry of { admin_token : string; entry_id : string }
  | Reopen_entry of { admin_token : string; entry_id : string }
  | Subscribe_queue of {
      queue_id : string;
      entry_token : string option;
      user_token : string option;
    }
  | Join_queue of {
      queue_id : string;
      values : (string * string) list;
      user_token : string option;
      entry_token : string option;
    }
  | Leave_queue of { queue_id : string; entry_token : string }

let tagged tag fields = `Assoc (("type", j_str tag) :: fields)

let json_of_client_message = function
  | Check_setup -> tagged "check_setup" []
  | List_public_queues -> tagged "list_public_queues" []
  | Resolve_queue_code { code } -> tagged "resolve_queue_code" [ ("code", j_str code) ]
  | Setup_super_admin { name; email; password } ->
      tagged "setup_super_admin"
        [ ("name", j_str name); ("email", j_str email); ("password", j_str password) ]
  | Login_admin { email; password } ->
      tagged "login_admin" [ ("email", j_str email); ("password", j_str password) ]
  | Login_user { email; password } ->
      tagged "login_user" [ ("email", j_str email); ("password", j_str password) ]
  | Subscribe_admin { admin_token; selected_queue_id } ->
      tagged "subscribe_admin"
        [
          ("admin_token", j_str admin_token);
          ("selected_queue_id", opt_json j_str selected_queue_id);
        ]
  | Create_queue { admin_token; name; fields; allow_guests; is_public; opens_at; weekly_schedule }
    ->
      tagged "create_queue"
        [
          ("admin_token", j_str admin_token); ("name", j_str name);
          ("fields", json_of_list json_of_field fields);
          ("allow_guests", j_bool allow_guests); ("is_public", j_bool is_public);
          ("opens_at", opt_json j_str opens_at);
          ("weekly_schedule", opt_json json_of_schedule weekly_schedule);
        ]
  | Update_queue_settings
      { admin_token; queue_id; fields; allow_guests; is_public; opens_at; weekly_schedule } ->
      tagged "update_queue_settings"
        [
          ("admin_token", j_str admin_token); ("queue_id", j_str queue_id);
          ("fields", json_of_list json_of_field fields);
          ("allow_guests", j_bool allow_guests); ("is_public", j_bool is_public);
          ("opens_at", opt_json j_str opens_at);
          ("weekly_schedule", opt_json json_of_schedule weekly_schedule);
        ]
  | Create_account { admin_token; name; email; password; role } ->
      tagged "create_account"
        [
          ("admin_token", j_str admin_token); ("name", j_str name); ("email", j_str email);
          ("password", j_str password); ("role", json_of_role role);
        ]
  | Update_account { admin_token; account_id; name; email; password; role } ->
      tagged "update_account"
        [
          ("admin_token", j_str admin_token); ("account_id", j_str account_id);
          ("name", j_str name); ("email", j_str email);
          ("password", opt_json j_str password); ("role", json_of_role role);
        ]
  | Delete_account { admin_token; account_id } ->
      tagged "delete_account" [ ("admin_token", j_str admin_token); ("account_id", j_str account_id) ]
  | Create_group { admin_token; name; role; member_ids } ->
      tagged "create_group"
        [
          ("admin_token", j_str admin_token); ("name", j_str name); ("role", json_of_role role);
          ("member_ids", json_of_list j_str member_ids);
        ]
  | Update_group { admin_token; group_id; name; role; member_ids } ->
      tagged "update_group"
        [
          ("admin_token", j_str admin_token); ("group_id", j_str group_id); ("name", j_str name);
          ("role", json_of_role role); ("member_ids", json_of_list j_str member_ids);
        ]
  | Delete_group { admin_token; group_id } ->
      tagged "delete_group" [ ("admin_token", j_str admin_token); ("group_id", j_str group_id) ]
  | Update_site_settings
      {
        admin_token; site_title; admin_password_sign_in_enabled; admin_microsoft_sign_in_enabled;
        user_password_sign_in_enabled; user_microsoft_sign_in_enabled;
      } ->
      tagged "update_site_settings"
        [
          ("admin_token", j_str admin_token); ("site_title", j_str site_title);
          ("admin_password_sign_in_enabled", j_bool admin_password_sign_in_enabled);
          ("admin_microsoft_sign_in_enabled", j_bool admin_microsoft_sign_in_enabled);
          ("user_password_sign_in_enabled", j_bool user_password_sign_in_enabled);
          ("user_microsoft_sign_in_enabled", j_bool user_microsoft_sign_in_enabled);
        ]
  | Share_queue { admin_token; queue_id; account_ids; group_ids } ->
      tagged "share_queue"
        [
          ("admin_token", j_str admin_token); ("queue_id", j_str queue_id);
          ("account_ids", json_of_list j_str account_ids);
          ("group_ids", json_of_list j_str group_ids);
        ]
  | Close_queue { admin_token; queue_id } ->
      tagged "close_queue" [ ("admin_token", j_str admin_token); ("queue_id", j_str queue_id) ]
  | Claim_entry { admin_token; entry_id } ->
      tagged "claim_entry" [ ("admin_token", j_str admin_token); ("entry_id", j_str entry_id) ]
  | Unclaim_entry { admin_token; entry_id } ->
      tagged "unclaim_entry" [ ("admin_token", j_str admin_token); ("entry_id", j_str entry_id) ]
  | Resolve_entry { admin_token; entry_id } ->
      tagged "resolve_entry" [ ("admin_token", j_str admin_token); ("entry_id", j_str entry_id) ]
  | Deny_entry { admin_token; entry_id } ->
      tagged "deny_entry" [ ("admin_token", j_str admin_token); ("entry_id", j_str entry_id) ]
  | Reopen_entry { admin_token; entry_id } ->
      tagged "reopen_entry" [ ("admin_token", j_str admin_token); ("entry_id", j_str entry_id) ]
  | Subscribe_queue { queue_id; entry_token; user_token } ->
      tagged "subscribe_queue"
        [
          ("queue_id", j_str queue_id); ("entry_token", opt_json j_str entry_token);
          ("user_token", opt_json j_str user_token);
        ]
  | Join_queue { queue_id; values; user_token; entry_token } ->
      tagged "join_queue"
        [
          ("queue_id", j_str queue_id); ("values", json_of_values values);
          ("user_token", opt_json j_str user_token);
          ("entry_token", opt_json j_str entry_token);
        ]
  | Leave_queue { queue_id; entry_token } ->
      tagged "leave_queue" [ ("queue_id", j_str queue_id); ("entry_token", j_str entry_token) ]

let client_message_of_json j =
  let tag = str (member "type" j) in
  match tag with
  | "check_setup" -> Check_setup
  | "list_public_queues" -> List_public_queues
  | "resolve_queue_code" -> Resolve_queue_code { code = str (member "code" j) }
  | "setup_super_admin" ->
      Setup_super_admin
        {
          name = str (member "name" j); email = str (member "email" j);
          password = str (member "password" j);
        }
  | "login_admin" -> Login_admin { email = str (member "email" j); password = str (member "password" j) }
  | "login_user" -> Login_user { email = str (member "email" j); password = str (member "password" j) }
  | "subscribe_admin" ->
      Subscribe_admin
        {
          admin_token = str (member "admin_token" j);
          selected_queue_id = opt_field "selected_queue_id" str j;
        }
  | "create_queue" ->
      Create_queue
        {
          admin_token = str (member "admin_token" j); name = str (member "name" j);
          fields = list_of field_of_json (member "fields" j);
          allow_guests = bool_ (member "allow_guests" j); is_public = bool_ (member "is_public" j);
          opens_at = opt_field "opens_at" str j;
          weekly_schedule = opt_field "weekly_schedule" schedule_of_json j;
        }
  | "update_queue_settings" ->
      Update_queue_settings
        {
          admin_token = str (member "admin_token" j); queue_id = str (member "queue_id" j);
          fields = list_of field_of_json (member "fields" j);
          allow_guests = bool_ (member "allow_guests" j); is_public = bool_ (member "is_public" j);
          opens_at = opt_field "opens_at" str j;
          weekly_schedule = opt_field "weekly_schedule" schedule_of_json j;
        }
  | "create_account" ->
      Create_account
        {
          admin_token = str (member "admin_token" j); name = str (member "name" j);
          email = str (member "email" j); password = str (member "password" j);
          role = role_of_json (member "role" j);
        }
  | "update_account" ->
      Update_account
        {
          admin_token = str (member "admin_token" j); account_id = str (member "account_id" j);
          name = str (member "name" j); email = str (member "email" j);
          password = opt_field "password" str j; role = role_of_json (member "role" j);
        }
  | "delete_account" ->
      Delete_account
        { admin_token = str (member "admin_token" j); account_id = str (member "account_id" j) }
  | "create_group" ->
      Create_group
        {
          admin_token = str (member "admin_token" j); name = str (member "name" j);
          role = role_of_json (member "role" j);
          member_ids = list_of str (member "member_ids" j);
        }
  | "update_group" ->
      Update_group
        {
          admin_token = str (member "admin_token" j); group_id = str (member "group_id" j);
          name = str (member "name" j); role = role_of_json (member "role" j);
          member_ids = list_of str (member "member_ids" j);
        }
  | "delete_group" ->
      Delete_group { admin_token = str (member "admin_token" j); group_id = str (member "group_id" j) }
  | "update_site_settings" ->
      Update_site_settings
        {
          admin_token = str (member "admin_token" j); site_title = str (member "site_title" j);
          admin_password_sign_in_enabled = bool_ (member "admin_password_sign_in_enabled" j);
          admin_microsoft_sign_in_enabled = bool_ (member "admin_microsoft_sign_in_enabled" j);
          user_password_sign_in_enabled = bool_ (member "user_password_sign_in_enabled" j);
          user_microsoft_sign_in_enabled = bool_ (member "user_microsoft_sign_in_enabled" j);
        }
  | "share_queue" ->
      Share_queue
        {
          admin_token = str (member "admin_token" j); queue_id = str (member "queue_id" j);
          account_ids = list_of str (member "account_ids" j);
          group_ids = list_of str (member "group_ids" j);
        }
  | "close_queue" ->
      Close_queue { admin_token = str (member "admin_token" j); queue_id = str (member "queue_id" j) }
  | "claim_entry" ->
      Claim_entry { admin_token = str (member "admin_token" j); entry_id = str (member "entry_id" j) }
  | "unclaim_entry" ->
      Unclaim_entry { admin_token = str (member "admin_token" j); entry_id = str (member "entry_id" j) }
  | "resolve_entry" ->
      Resolve_entry { admin_token = str (member "admin_token" j); entry_id = str (member "entry_id" j) }
  | "deny_entry" ->
      Deny_entry { admin_token = str (member "admin_token" j); entry_id = str (member "entry_id" j) }
  | "reopen_entry" ->
      Reopen_entry { admin_token = str (member "admin_token" j); entry_id = str (member "entry_id" j) }
  | "subscribe_queue" ->
      Subscribe_queue
        {
          queue_id = str (member "queue_id" j); entry_token = opt_field "entry_token" str j;
          user_token = opt_field "user_token" str j;
        }
  | "join_queue" ->
      Join_queue
        {
          queue_id = str (member "queue_id" j); values = assoc_of_values (member "values" j);
          user_token = opt_field "user_token" str j; entry_token = opt_field "entry_token" str j;
        }
  | "leave_queue" ->
      Leave_queue { queue_id = str (member "queue_id" j); entry_token = str (member "entry_token" j) }
  | other -> failwith ("unknown client message type: " ^ other)

(* ---------- server -> client messages ---------- *)

type server_message =
  | Setup_state of { needs_setup : bool; site_settings : site_settings_view }
  | Admin_logged_in of { admin : admin_identity_view }
  | User_logged_in of { user : user_identity_view }
  | Queue_created of { queue_id : string }
  | Queue_code_resolved of { queue_id : string }
  | Queue_settings_updated
  | Account_created
  | Account_updated
  | Account_deleted
  | Group_created
  | Group_updated
  | Group_deleted
  | Site_settings_updated
  | Queue_sharing_updated
  | Queue_closed
  | Admin_state of { state : admin_state_view }
  | Queue_state of {
      queue : user_queue_view;
      your_entry : user_entry_view option;
      site_settings : site_settings_view;
    }
  | Public_queues of { queues : queue_summary list; site_settings : site_settings_view }
  | Info of { message : string }
  | Error of { message : string }

let json_of_server_message = function
  | Setup_state { needs_setup; site_settings } ->
      tagged "setup_state"
        [ ("needs_setup", j_bool needs_setup); ("site_settings", json_of_site_settings site_settings) ]
  | Admin_logged_in { admin } -> tagged "admin_logged_in" [ ("admin", json_of_admin_identity admin) ]
  | User_logged_in { user } -> tagged "user_logged_in" [ ("user", json_of_user_identity user) ]
  | Queue_created { queue_id } -> tagged "queue_created" [ ("queue_id", j_str queue_id) ]
  | Queue_code_resolved { queue_id } -> tagged "queue_code_resolved" [ ("queue_id", j_str queue_id) ]
  | Queue_settings_updated -> tagged "queue_settings_updated" []
  | Account_created -> tagged "account_created" []
  | Account_updated -> tagged "account_updated" []
  | Account_deleted -> tagged "account_deleted" []
  | Group_created -> tagged "group_created" []
  | Group_updated -> tagged "group_updated" []
  | Group_deleted -> tagged "group_deleted" []
  | Site_settings_updated -> tagged "site_settings_updated" []
  | Queue_sharing_updated -> tagged "queue_sharing_updated" []
  | Queue_closed -> tagged "queue_closed" []
  | Admin_state { state } -> tagged "admin_state" [ ("state", json_of_admin_state state) ]
  | Queue_state { queue; your_entry; site_settings } ->
      tagged "queue_state"
        [
          ("queue", json_of_user_queue queue);
          ("your_entry", opt_json json_of_user_entry your_entry);
          ("site_settings", json_of_site_settings site_settings);
        ]
  | Public_queues { queues; site_settings } ->
      tagged "public_queues"
        [
          ("queues", json_of_list json_of_summary queues);
          ("site_settings", json_of_site_settings site_settings);
        ]
  | Info { message } -> tagged "info" [ ("message", j_str message) ]
  | Error { message } -> tagged "error" [ ("message", j_str message) ]

let server_message_of_json j =
  let tag = str (member "type" j) in
  match tag with
  | "setup_state" ->
      Setup_state
        {
          needs_setup = bool_ (member "needs_setup" j);
          site_settings = site_settings_of_json (member "site_settings" j);
        }
  | "admin_logged_in" -> Admin_logged_in { admin = admin_identity_of_json (member "admin" j) }
  | "user_logged_in" -> User_logged_in { user = user_identity_of_json (member "user" j) }
  | "queue_created" -> Queue_created { queue_id = str (member "queue_id" j) }
  | "queue_code_resolved" -> Queue_code_resolved { queue_id = str (member "queue_id" j) }
  | "queue_settings_updated" -> Queue_settings_updated
  | "account_created" -> Account_created
  | "account_updated" -> Account_updated
  | "account_deleted" -> Account_deleted
  | "group_created" -> Group_created
  | "group_updated" -> Group_updated
  | "group_deleted" -> Group_deleted
  | "site_settings_updated" -> Site_settings_updated
  | "queue_sharing_updated" -> Queue_sharing_updated
  | "queue_closed" -> Queue_closed
  | "admin_state" -> Admin_state { state = admin_state_of_json (member "state" j) }
  | "queue_state" ->
      Queue_state
        {
          queue = user_queue_of_json (member "queue" j);
          your_entry = opt_field "your_entry" user_entry_of_json j;
          site_settings = site_settings_of_json (member "site_settings" j);
        }
  | "public_queues" ->
      Public_queues
        {
          queues = list_of summary_of_json (member "queues" j);
          site_settings = site_settings_of_json (member "site_settings" j);
        }
  | "info" -> Info { message = str (member "message" j) }
  | "error" -> Error { message = str (member "message" j) }
  | other -> failwith ("unknown server message type: " ^ other)

let encode_client m = Yojson.Safe.to_string (json_of_client_message m)
let decode_client s = client_message_of_json (Yojson.Safe.from_string s)
let encode_server m = Yojson.Safe.to_string (json_of_server_message m)
let decode_server s = server_message_of_json (Yojson.Safe.from_string s)
