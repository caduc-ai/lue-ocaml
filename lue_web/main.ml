open Bonsai_web
module P = Lue_shared.Protocol
module S = State

let state_var : S.t Bonsai.Var.t = Bonsai.Var.create S.initial

let update f = Bonsai.Var.update state_var ~f
let dispatch (msg : P.client_message) = Ws_client.send (P.encode_client msg)

let route_to path =
  let parts = String.split_on_char '/' path |> List.filter (fun s -> s <> "") in
  match parts with
  | [] -> update (fun s -> { s with S.page = S.Home_page })
  | [ "admin-login" ] -> update (fun s -> { s with S.page = S.Admin_login_page })
  | [ "user-login" ] -> update (fun s -> { s with S.page = S.User_login_page })
  | [ "public" ] ->
      update (fun s -> { s with S.page = S.Public_queues_page });
      dispatch P.List_public_queues
  | [ "admin" ] -> (
      update (fun s -> { s with S.page = S.Admin_page });
      match S.saved_admin_token () with
      | Some token -> dispatch (P.Subscribe_admin { admin_token = token; selected_queue_id = None })
      | None -> update (fun s -> { s with S.page = S.Admin_login_page }))
  | [ "queue"; queue_id ] ->
      update (fun s -> { s with S.page = S.Queue_page queue_id; queue_view = None });
      dispatch
        (P.Subscribe_queue
           {
             queue_id;
             entry_token = S.saved_entry_token queue_id;
             user_token = Option.map (fun (u : P.user_identity_view) -> u.token) (Bonsai.Var.get state_var).user_identity;
           })
  | _ -> update (fun s -> { s with S.page = S.Home_page })

let handle_incoming (raw : string) =
  match P.decode_server raw with
  | msg -> (
      update (fun s -> S.apply_server_message s msg);
      match msg with
      | P.Setup_state { needs_setup; _ } ->
          if not needs_setup then
            (* attempt silent resume from saved tokens once we know setup is done *)
            match S.saved_admin_token () with
            | Some token -> dispatch (P.Subscribe_admin { admin_token = token; selected_queue_id = None })
            | None -> ()
      | _ -> ())
  | exception _ -> ()

let ctx_value : View.ctx Bonsai.Value.t =
  Bonsai.Value.map (Bonsai.Var.value state_var) ~f:(fun state ->
      { View.state; update; dispatch; navigate = (fun path -> Ws_client.set_hash path) })

let app : Vdom.Node.t Bonsai.Computation.t = Bonsai.read (Bonsai.Value.map ctx_value ~f:View.render)

let () =
  Ws_client.on_hash_change route_to;
  Ws_client.connect
    ~on_open:(fun () ->
      update (fun s -> { s with S.connected = true });
      dispatch P.Check_setup;
      route_to (Ws_client.current_hash ()))
    ~on_message:handle_incoming
    ~on_close:(fun () -> update (fun s -> { s with S.connected = false }));
  Bonsai_web.Start.start app
