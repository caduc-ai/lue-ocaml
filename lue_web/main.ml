module App_view = View
open Bonsai_web
module P = Lue_shared.Protocol
module S = State

let state_var : S.t Bonsai.Var.t = Bonsai.Var.create S.initial

(* Plain, synchronous, unit-returning versions used from our own imperative
   glue code below (websocket callbacks, hash-change routing). *)
let update_now f = Bonsai.Var.update state_var ~f
let dispatch_now (msg : P.client_message) = Ws_client.send (P.encode_client msg)
let navigate_now path = Ws_client.set_hash path

(* [Vdom.Effect.t]-returning wrappers for use inside [View.ctx], since every
   vdom event handler (on_click, on_input, ...) must produce an effect
   rather than perform a side effect directly. *)
let update f = Vdom.Effect.of_sync_fun update_now f
let dispatch msg = Vdom.Effect.of_sync_fun dispatch_now msg
let navigate path = Vdom.Effect.of_sync_fun navigate_now path

let route_to path =
  let parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
  match parts with
  | [] -> update_now (fun s -> { s with S.page = S.Home_page })
  | [ "admin-login" ] -> update_now (fun s -> { s with S.page = S.Admin_login_page })
  | [ "user-login" ] -> update_now (fun s -> { s with S.page = S.User_login_page })
  | [ "public" ] ->
      update_now (fun s -> { s with S.page = S.Public_queues_page });
      dispatch_now P.List_public_queues
  | [ "admin" ] -> (
      update_now (fun s -> { s with S.page = S.Admin_page });
      match S.saved_admin_token () with
      | Some token -> dispatch_now (P.Subscribe_admin { admin_token = token; selected_queue_id = None })
      | None -> update_now (fun s -> { s with S.page = S.Admin_login_page }))
  | [ "queue"; queue_id ] ->
      update_now (fun s -> { s with S.page = S.Queue_page queue_id; queue_view = None });
      dispatch_now
        (P.Subscribe_queue
           {
             queue_id;
             entry_token = S.saved_entry_token queue_id;
             user_token = Option.map (fun (u : P.user_identity_view) -> u.token) (Bonsai.Var.get state_var).user_identity;
           })
  | _ -> update_now (fun s -> { s with S.page = S.Home_page })

let handle_incoming (raw : string) =
  match P.decode_server raw with
  | msg -> (
      update_now (fun s -> S.apply_server_message s msg);
      match msg with
      | P.Setup_state { needs_setup; _ } ->
          if not needs_setup then
            (* attempt silent resume from saved tokens once we know setup is done *)
            (match S.saved_admin_token () with
            | Some token -> dispatch_now (P.Subscribe_admin { admin_token = token; selected_queue_id = None })
            | None -> ())
      | _ -> ())
  | exception _ -> ()

let ctx_value : App_view.ctx Bonsai.Value.t =
  Bonsai.Value.map (Bonsai.Var.value state_var) ~f:(fun state -> { App_view.state; update; dispatch; navigate })

let app : Vdom.Node.t Bonsai.Computation.t = Bonsai.read (Bonsai.Value.map ctx_value ~f:App_view.render)

let () =
  Ws_client.on_hash_change route_to;
  Ws_client.connect
    ~on_open:(fun () ->
      update_now (fun s -> { s with S.connected = true });
      dispatch_now P.Check_setup;
      route_to (Ws_client.current_hash ()))
    ~on_message:handle_incoming
    ~on_close:(fun () -> update_now (fun s -> { s with S.connected = false }));
  Bonsai_web.Start.start app
