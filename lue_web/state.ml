module P = Lue_shared.Protocol

type page =
  | Loading
  | Setup_page
  | Home_page
  | Admin_login_page
  | User_login_page
  | Admin_page
  | Queue_page of string
  | Public_queues_page

type t = {
  page : page;
  connected : bool;
  needs_setup : bool option;
  site_settings : P.site_settings_view option;
  admin_identity : P.admin_identity_view option;
  admin_state : P.admin_state_view option;
  user_identity : P.user_identity_view option;
  queue_view : (P.user_queue_view * P.user_entry_view option) option;
  public_queues : P.queue_summary list;
  error : string option;
  info : string option;
  form : (string * string) list;
  checked : (string * bool) list;
}

let initial =
  {
    page = Loading; connected = false; needs_setup = None; site_settings = None;
    admin_identity = None; admin_state = None; user_identity = None; queue_view = None;
    public_queues = []; error = None; info = None; form = []; checked = [];
  }

(* --- localStorage keys --- *)

let admin_token_key = "lue_admin_token"
let user_token_key = "lue_user_token"
let entry_token_key entry_key = "lue_entry_token_" ^ entry_key

let saved_admin_token () = Ws_client.local_storage_get admin_token_key
let save_admin_token t = Ws_client.local_storage_set admin_token_key t
let clear_admin_token () = Ws_client.local_storage_remove admin_token_key

let saved_user_token () = Ws_client.local_storage_get user_token_key
let save_user_token t = Ws_client.local_storage_set user_token_key t
let clear_user_token () = Ws_client.local_storage_remove user_token_key

let saved_entry_token queue_id = Ws_client.local_storage_get (entry_token_key queue_id)
let save_entry_token queue_id t = Ws_client.local_storage_set (entry_token_key queue_id) t
let clear_entry_token queue_id = Ws_client.local_storage_remove (entry_token_key queue_id)

(* --- form field helpers --- *)

let field key state = Option.value (List.assoc_opt key state.form) ~default:""

let set_field key value state =
  { state with form = (key, value) :: List.remove_assoc key state.form }

let clear_fields keys state =
  { state with form = List.filter (fun (k, _) -> not (List.mem k keys)) state.form }

let is_checked key state = Option.value (List.assoc_opt key state.checked) ~default:false

let toggle_checked key state =
  { state with checked = (key, not (is_checked key state)) :: List.remove_assoc key state.checked }

let checked_keys prefix state =
  List.filter_map
    (fun (k, v) ->
      if v && String.length k > String.length prefix && String.sub k 0 (String.length prefix) = prefix then
        Some (String.sub k (String.length prefix) (String.length k - String.length prefix))
      else None)
    state.checked

(* --- reducer over incoming server messages --- *)

let apply_server_message (state : t) (msg : P.server_message) : t =
  match msg with
  | P.Setup_state { needs_setup; site_settings } ->
      let page = if state.page = Loading then (if needs_setup then Setup_page else Home_page) else state.page in
      { state with needs_setup = Some needs_setup; site_settings = Some site_settings; page }
  | P.Admin_logged_in { admin } ->
      save_admin_token admin.ai_token;
      { state with admin_identity = Some admin; page = Admin_page; error = None }
  | P.User_logged_in { user } ->
      save_user_token user.token;
      { state with user_identity = Some user; error = None; info = Some "Signed in" }
  | P.Queue_created { queue_id = _ } -> state
  | P.Queue_code_resolved { queue_id } -> { state with page = Queue_page queue_id }
  | P.Queue_settings_updated -> { state with info = Some "Queue settings updated" }
  | P.Account_created -> { state with info = Some "Account created" }
  | P.Account_updated -> { state with info = Some "Account updated" }
  | P.Account_deleted -> { state with info = Some "Account deleted" }
  | P.Group_created -> { state with info = Some "Group created" }
  | P.Group_updated -> { state with info = Some "Group updated" }
  | P.Group_deleted -> { state with info = Some "Group deleted" }
  | P.Site_settings_updated -> { state with info = Some "Site settings updated" }
  | P.Queue_sharing_updated -> { state with info = Some "Sharing updated" }
  | P.Queue_closed -> { state with info = Some "Queue closed" }
  | P.Admin_state { state = admin_state } -> { state with admin_state = Some admin_state; error = None }
  | P.Queue_state { queue; your_entry; site_settings } -> (
      (match your_entry with
      | Some entry -> save_entry_token queue.uq_id entry.ue_token
      | None -> ());
      { state with queue_view = Some (queue, your_entry); site_settings = Some site_settings; error = None })
  | P.Public_queues { queues; site_settings } ->
      { state with public_queues = queues; site_settings = Some site_settings }
  | P.Info { message } -> { state with info = Some message }
  | P.Error { message } -> { state with error = Some message }
