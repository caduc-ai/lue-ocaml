(* Dream HTTP + WebSocket server. Mirrors crates/server/src/{main,ws}.rs:
   a single `/ws` endpoint speaks the JSON protocol defined in
   Lue_shared.Protocol; every mutating command re-broadcasts fresh state to
   every connection that is currently subscribed to the affected admin
   session or queue, the same way the original's `broadcast::Sender<Uuid>`
   fan-out worked. Microsoft SSO from the original is intentionally not
   ported. *)

module P = Lue_shared.Protocol
open Lwt.Infix

type connection = {
  ws : Dream.websocket;
  mutable admin_token : string option;
  mutable admin_selected_queue_id : string option;
  mutable c_queue_id : string option;
  mutable c_entry_token : string option;
  mutable c_user_token : string option;
}

let connections : connection list ref = ref []

let send conn (msg : P.server_message) =
  Lwt.catch (fun () -> Dream.send conn.ws (P.encode_server msg)) (fun _ -> Lwt.return_unit)

let broadcast queue_id =
  Lwt_list.iter_s
    (fun conn ->
      (match conn.admin_token with
      | None -> Lwt.return_unit
      | Some tok -> (
          match Store.admin_state tok conn.admin_selected_queue_id with
          | Some state -> send conn (P.Admin_state { state })
          | None -> Lwt.return_unit))
      >>= fun () ->
      match conn.c_queue_id with
      | Some qid when qid = queue_id -> (
          match Store.user_view qid conn.c_entry_token with
          | Some (queue, your_entry) ->
              send conn (P.Queue_state { queue; your_entry; site_settings = Store.site_settings_view () })
          | None -> Lwt.return_unit)
      | _ -> Lwt.return_unit)
    !connections

let save_and_ignore () = Store.save ()

(* Sends the fresh admin state directly to [conn], used right after commands
   that also want an immediate acknowledgement message. *)
let send_admin_state conn admin_token selected_queue_id =
  match Store.admin_state admin_token selected_queue_id with
  | Some state -> send conn (P.Admin_state { state })
  | None -> Lwt.return_unit

let send_queue_state conn queue_id entry_token =
  match Store.user_view queue_id entry_token with
  | Some (queue, your_entry) ->
      send conn (P.Queue_state { queue; your_entry; site_settings = Store.site_settings_view () })
  | None -> Lwt.return_unit

let handle_command conn (msg : P.client_message) =
  match msg with
  | P.Check_setup ->
      send conn (P.Setup_state { needs_setup = Store.needs_initial_setup (); site_settings = Store.site_settings_view () })
  | P.List_public_queues ->
      send conn (P.Public_queues { queues = Store.public_queues (); site_settings = Store.site_settings_view () })
  | P.Resolve_queue_code { code } -> (
      match Store.queue_id_for_code code with
      | Some queue_id -> send conn (P.Queue_code_resolved { queue_id })
      | None -> send conn (P.Error { message = "Queue code not found" }))
  | P.Setup_super_admin { name; email; password } -> (
      match Store.setup_super_admin name email password with
      | Error message -> send conn (P.Error { message })
      | Ok admin ->
          save_and_ignore ();
          conn.admin_token <- Some admin.ai_token;
          send conn (P.Admin_logged_in { admin }) >>= fun () ->
          send_admin_state conn admin.ai_token None)
  | P.Login_admin { email; password } -> (
      match Store.login_admin email password with
      | Error message -> send conn (P.Error { message })
      | Ok admin ->
          save_and_ignore ();
          conn.admin_token <- Some admin.ai_token;
          send conn (P.Admin_logged_in { admin }) >>= fun () -> send_admin_state conn admin.ai_token None)
  | P.Login_user { email; password } -> (
      match Store.login_user email password with
      | Error message -> send conn (P.Error { message })
      | Ok user ->
          save_and_ignore ();
          conn.c_user_token <- Some user.token;
          send conn (P.User_logged_in { user }))
  | P.Subscribe_admin { admin_token; selected_queue_id } -> (
      match Store.admin_state admin_token selected_queue_id with
      | None -> send conn (P.Error { message = "unknown admin session" })
      | Some state ->
          conn.admin_token <- Some admin_token;
          conn.admin_selected_queue_id <-
            Option.map (fun q -> q.P.sel_summary.id) state.as_selected_queue;
          send conn (P.Admin_state { state }))
  | P.Create_queue { admin_token; name; fields; allow_guests; is_public; opens_at; weekly_schedule } -> (
      match Store.create_queue admin_token name fields allow_guests is_public opens_at weekly_schedule with
      | Error message -> send conn (P.Error { message })
      | Ok queue_id ->
          save_and_ignore ();
          conn.admin_token <- Some admin_token;
          conn.admin_selected_queue_id <- Some queue_id;
          send conn (P.Queue_created { queue_id }) >>= fun () ->
          send_admin_state conn admin_token (Some queue_id) >>= fun () -> broadcast queue_id)
  | P.Update_queue_settings
      { admin_token; queue_id; fields; allow_guests; is_public; opens_at; weekly_schedule } -> (
      match Store.update_queue_settings admin_token queue_id fields allow_guests is_public opens_at weekly_schedule with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Queue_settings_updated >>= fun () ->
          send_admin_state conn admin_token (Some queue_id) >>= fun () -> broadcast queue_id)
  | P.Create_account { admin_token; name; email; password; role } -> (
      match Store.create_account admin_token name email password role with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Account_created >>= fun () ->
          send_admin_state conn admin_token conn.admin_selected_queue_id)
  | P.Update_account { admin_token; account_id; name; email; password; role } -> (
      match Store.update_account admin_token account_id name email password role with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Account_updated >>= fun () ->
          send_admin_state conn admin_token conn.admin_selected_queue_id)
  | P.Delete_account { admin_token; account_id } -> (
      match Store.delete_account admin_token account_id with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Account_deleted >>= fun () ->
          send_admin_state conn admin_token conn.admin_selected_queue_id)
  | P.Create_group { admin_token; name; role; member_ids } -> (
      match Store.create_group admin_token name role member_ids with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Group_created >>= fun () ->
          send_admin_state conn admin_token conn.admin_selected_queue_id)
  | P.Update_group { admin_token; group_id; name; role; member_ids } -> (
      match Store.update_group admin_token group_id name role member_ids with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Group_updated >>= fun () ->
          send_admin_state conn admin_token conn.admin_selected_queue_id)
  | P.Delete_group { admin_token; group_id } -> (
      match Store.delete_group admin_token group_id with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Group_deleted >>= fun () ->
          send_admin_state conn admin_token conn.admin_selected_queue_id)
  | P.Update_site_settings
      {
        admin_token; site_title; admin_password_sign_in_enabled; admin_microsoft_sign_in_enabled;
        user_password_sign_in_enabled; user_microsoft_sign_in_enabled;
      } -> (
      match
        Store.update_site_settings admin_token site_title admin_password_sign_in_enabled
          admin_microsoft_sign_in_enabled user_password_sign_in_enabled user_microsoft_sign_in_enabled
      with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Site_settings_updated >>= fun () ->
          send_admin_state conn admin_token conn.admin_selected_queue_id)
  | P.Share_queue { admin_token; queue_id; account_ids; group_ids } -> (
      match Store.share_queue admin_token queue_id account_ids group_ids with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          send conn P.Queue_sharing_updated >>= fun () ->
          send_admin_state conn admin_token (Some queue_id) >>= fun () -> broadcast queue_id)
  | P.Close_queue { admin_token; queue_id } -> (
      match Store.close_queue admin_token queue_id with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          conn.admin_selected_queue_id <- None;
          send conn P.Queue_closed >>= fun () ->
          send_admin_state conn admin_token None >>= fun () -> broadcast queue_id)
  | P.Claim_entry { admin_token; entry_id } -> (
      match Store.claim_entry admin_token entry_id with
      | Error message -> send conn (P.Error { message })
      | Ok queue_id -> save_and_ignore (); broadcast queue_id)
  | P.Unclaim_entry { admin_token; entry_id } -> (
      match Store.unclaim_entry admin_token entry_id with
      | Error message -> send conn (P.Error { message })
      | Ok queue_id -> save_and_ignore (); broadcast queue_id)
  | P.Resolve_entry { admin_token; entry_id } -> (
      match Store.update_entry_status admin_token entry_id P.Resolved with
      | Error message -> send conn (P.Error { message })
      | Ok queue_id -> save_and_ignore (); broadcast queue_id)
  | P.Deny_entry { admin_token; entry_id } -> (
      match Store.update_entry_status admin_token entry_id P.Denied with
      | Error message -> send conn (P.Error { message })
      | Ok queue_id -> save_and_ignore (); broadcast queue_id)
  | P.Reopen_entry { admin_token; entry_id } -> (
      match Store.update_entry_status admin_token entry_id P.Pending with
      | Error message -> send conn (P.Error { message })
      | Ok queue_id -> save_and_ignore (); broadcast queue_id)
  | P.Subscribe_queue { queue_id; entry_token; user_token } -> (
      match Store.queue_unavailable_message queue_id with
      | Some message -> send conn (P.Error { message })
      | None -> (
          match Store.user_view queue_id entry_token with
          | None -> send conn (P.Error { message = "unknown queue" })
          | Some (queue, your_entry) ->
              conn.c_queue_id <- Some queue_id;
              conn.c_entry_token <- entry_token;
              conn.c_user_token <- user_token;
              send conn (P.Queue_state { queue; your_entry; site_settings = Store.site_settings_view () })))
  | P.Join_queue { queue_id; values; user_token; entry_token } -> (
      match Store.join_queue queue_id values user_token entry_token with
      | Error message -> send conn (P.Error { message })
      | Ok token ->
          save_and_ignore ();
          conn.c_queue_id <- Some queue_id;
          conn.c_entry_token <- Some token;
          conn.c_user_token <- user_token;
          send_queue_state conn queue_id (Some token) >>= fun () -> broadcast queue_id)
  | P.Leave_queue { queue_id; entry_token } -> (
      match Store.leave_queue queue_id entry_token with
      | Error message -> send conn (P.Error { message })
      | Ok () ->
          save_and_ignore ();
          conn.c_queue_id <- Some queue_id;
          conn.c_entry_token <- Some entry_token;
          send_queue_state conn queue_id (Some entry_token) >>= fun () -> broadcast queue_id)

let rec receive_loop conn =
  Dream.receive conn.ws >>= function
  | None -> Lwt.return_unit
  | Some text -> (
      (match P.decode_client text with
      | msg -> handle_command conn msg
      | exception exn ->
          send conn (P.Error { message = Printf.sprintf "invalid websocket message: %s" (Printexc.to_string exn) }))
      >>= fun () -> receive_loop conn)

let ws_handler _request =
  Dream.websocket (fun ws ->
      let conn =
        { ws; admin_token = None; admin_selected_queue_id = None; c_queue_id = None;
          c_entry_token = None; c_user_token = None }
      in
      connections := conn :: !connections;
      Lwt.finalize
        (fun () -> receive_loop conn)
        (fun () ->
          connections := List.filter (fun c -> c != conn) !connections;
          Lwt.return_unit))

let () =
  let data_path = try Sys.getenv "DATA_PATH" with Not_found -> "data/store.json" in
  Store.data_path := data_path;
  Store.load_from_disk data_path;
  let server_addr = try Sys.getenv "SERVER_ADDR" with Not_found -> "127.0.0.1:3000" in
  let interface, port =
    match String.rindex_opt server_addr ':' with
    | Some i -> (String.sub server_addr 0 i, int_of_string (String.sub server_addr (i + 1) (String.length server_addr - i - 1)))
    | None -> (server_addr, 3000)
  in
  Printf.printf "server listening on http://%s:%d\n%!" interface port;
  Dream.run ~interface ~port
  @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/health" (fun _ -> Dream.respond "ok");
         Dream.get "/ws" ws_handler;
         Dream.get "/**" (Dream.static "lue_web/dist");
       ]
