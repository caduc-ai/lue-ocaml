open Bonsai_web
module P = Lue_shared.Protocol
module S = State

type ctx = {
  state : S.t;
  update : (S.t -> S.t) -> unit Vdom.Effect.t;
  dispatch : P.client_message -> unit Vdom.Effect.t;
  navigate : string -> unit Vdom.Effect.t;
}

let txt s = Vdom.Node.text s
let div ?(cls = "") children = Vdom.Node.div [ Vdom.Attr.class_ cls ] children
let card children = div ~cls:"card" children
let button ?(cls = "") label on_click = Vdom.Node.button [ Vdom.Attr.class_ cls; Vdom.Attr.on_click (fun _ -> on_click) ] [ txt label ]
let link label href = Vdom.Node.a [ Vdom.Attr.href href ] [ txt label ]

let text_input ~ctx ~key ~placeholder ?(kind = "text") () =
  Vdom.Node.input
    [
      Vdom.Attr.type_ kind; Vdom.Attr.placeholder placeholder;
      Vdom.Attr.string_property "value" (S.field key ctx.state);
      Vdom.Attr.on_input (fun _ value -> ctx.update (S.set_field key value));
    ]
    ()

let checkbox ~ctx ~key ~label () =
  Vdom.Node.label []
    [
      Vdom.Node.input
        [
          Vdom.Attr.type_ "checkbox"; Vdom.Attr.bool_property "checked" (S.is_checked key ctx.state);
          Vdom.Attr.on_click (fun _ -> ctx.update (S.toggle_checked key));
        ]
        ();
      txt (" " ^ label);
    ]

let banner ctx =
  div
    [
      (match ctx.state.error with
      | Some msg -> div ~cls:"card error" [ txt ("Error: " ^ msg) ]
      | None -> txt "");
      (match ctx.state.info with
      | Some msg -> div ~cls:"card info" [ txt msg ]
      | None -> txt "");
    ]

let role_of_string = function
  | "admin" -> P.Admin
  | "super_admin" -> P.Super_admin
  | _ -> P.User

let role_to_label = function P.Super_admin -> "Super Admin" | P.Admin -> "Admin" | P.User -> "User"

(* --------------------------- setup / login --------------------------- *)

let setup_view ctx =
  card
    [
      Vdom.Node.h2 [] [ txt "Initial setup" ];
      txt "Create the first super admin account.";
      div ~cls:"col"
        [
          text_input ~ctx ~key:"setup_name" ~placeholder:"Name" ();
          text_input ~ctx ~key:"setup_email" ~placeholder:"Email" ~kind:"email" ();
          text_input ~ctx ~key:"setup_password" ~placeholder:"Password" ~kind:"password" ();
          button "Create super admin"
            (ctx.dispatch
               (P.Setup_super_admin
                  {
                    name = S.field "setup_name" ctx.state; email = S.field "setup_email" ctx.state;
                    password = S.field "setup_password" ctx.state;
                  }));
        ];
    ]

let home_view ctx =
  card
    [
      Vdom.Node.h2 [] [ txt (Option.fold ~none:"Lue" ~some:(fun (s : P.site_settings_view) -> s.site_title) ctx.state.site_settings) ];
      div ~cls:"row"
        [
          button "Admin sign in" (ctx.navigate "/admin-login");
          button ~cls:"secondary" "User sign in" (ctx.navigate "/user-login");
          button ~cls:"secondary" "Browse public queues" (ctx.navigate "/public");
        ];
      div ~cls:"col"
        [
          txt "Have a queue code or link?";
          text_input ~ctx ~key:"join_code" ~placeholder:"Queue code" ();
          button "Go"
            (ctx.dispatch (P.Resolve_queue_code { code = S.field "join_code" ctx.state }));
        ];
    ]

let admin_login_view ctx =
  card
    [
      Vdom.Node.h2 [] [ txt "Admin sign in" ];
      div ~cls:"col"
        [
          text_input ~ctx ~key:"admin_email" ~placeholder:"Email" ~kind:"email" ();
          text_input ~ctx ~key:"admin_password" ~placeholder:"Password" ~kind:"password" ();
          button "Sign in"
            (ctx.dispatch
               (P.Login_admin { email = S.field "admin_email" ctx.state; password = S.field "admin_password" ctx.state }));
          button ~cls:"secondary" "Back" (ctx.navigate "/");
        ];
    ]

let user_login_view ctx =
  card
    [
      Vdom.Node.h2 [] [ txt "Sign in" ];
      div ~cls:"col"
        [
          text_input ~ctx ~key:"user_email" ~placeholder:"Email" ~kind:"email" ();
          text_input ~ctx ~key:"user_password" ~placeholder:"Password" ~kind:"password" ();
          button "Sign in"
            (ctx.dispatch
               (P.Login_user { email = S.field "user_email" ctx.state; password = S.field "user_password" ctx.state }));
          button ~cls:"secondary" "Back" (ctx.navigate "/");
        ];
    ]

let public_queues_view ctx =
  card
    [
      Vdom.Node.h2 [] [ txt "Public queues" ];
      div ~cls:"col"
        (List.map
           (fun (q : P.queue_summary) ->
             div ~cls:"row"
               [
                 txt (Printf.sprintf "%s (code %s) — waiting: %d" q.name q.code q.waiting_count);
                 button "Open" (ctx.navigate ("/queue/" ^ q.id));
               ])
           ctx.state.public_queues);
      button ~cls:"secondary" "Back" (ctx.navigate "/");
    ]

(* --------------------------- queue join / status --------------------------- *)

let field_row ctx (f : P.queue_field) =
  div ~cls:"col"
    [
      txt (f.label ^ if f.required then " *" else "");
      (if f.options = [] then text_input ~ctx ~key:("qf_" ^ f.key) ~placeholder:f.label ()
       else
         Vdom.Node.select
           [ Vdom.Attr.on_change (fun _ value -> ctx.update (S.set_field ("qf_" ^ f.key) value)) ]
           (Vdom.Node.option [] [ txt "" ]
           :: List.map (fun opt -> Vdom.Node.option [ Vdom.Attr.value opt ] [ txt opt ]) f.options));
    ]

let status_label = function
  | P.Pending -> "Waiting" | P.Claimed -> "Being helped" | P.Left -> "Left"
  | P.Resolved -> "Resolved" | P.Denied -> "Denied"

let queue_page_view ctx queue_id =
  match ctx.state.queue_view with
  | None -> card [ txt "Loading queue…" ]
  | Some (q, your_entry) ->
      let closed_banner =
        match q.uq_closed_at with
        | Some at -> div ~cls:"card error" [ txt (Printf.sprintf "This queue was closed at %s%s" at (Option.fold ~none:"" ~some:(fun n -> " by " ^ n) q.uq_closed_by_name)) ]
        | None -> txt ""
      in
      card
        [
          Vdom.Node.h2 [] [ txt q.uq_name ];
          txt (Printf.sprintf "Code: %s — waiting: %d" q.uq_code q.uq_waiting_count);
          closed_banner;
          (match your_entry with
          | Some entry ->
              div ~cls:"col"
                [
                  Vdom.Node.h3 [] [ txt "Your status" ];
                  div ~cls:"pill" [ txt (status_label entry.ue_status) ];
                  (match entry.ue_position with
                  | Some p -> txt (Printf.sprintf "Position in line: %d" p)
                  | None -> txt "");
                  (match entry.ue_claimed_by with
                  | Some name -> txt ("Being helped by " ^ name)
                  | None -> txt "");
                  (if entry.ue_status = P.Pending || entry.ue_status = P.Claimed then
                     button ~cls:"danger" "Leave queue"
                       (ctx.dispatch (P.Leave_queue { queue_id; entry_token = entry.ue_token }))
                   else txt "");
                ]
          | None ->
              div ~cls:"col"
                (List.map (field_row ctx) q.uq_fields
                @ [
                    button "Join queue"
                      (ctx.dispatch
                         (P.Join_queue
                            {
                              queue_id;
                              values = List.map (fun (f : P.queue_field) -> (f.key, S.field ("qf_" ^ f.key) ctx.state)) q.uq_fields;
                              user_token = ctx.state.user_identity |> Option.map (fun (u : P.user_identity_view) -> u.token);
                              entry_token = S.saved_entry_token queue_id;
                            }));
                  ]));
          button ~cls:"secondary" "Back" (ctx.navigate "/");
        ]

(* --------------------------- admin dashboard --------------------------- *)

let entry_row ctx (e : P.admin_entry_view) =
  Vdom.Node.tr []
    [
      Vdom.Node.td [] [ txt e.ae_requester_label; (if e.ae_is_guest then div ~cls:"pill" [ txt "guest" ] else txt "") ];
      Vdom.Node.td [] [ txt (status_label e.ae_status) ];
      Vdom.Node.td [] [ txt (Option.value e.ae_claimed_by ~default:"—") ];
      Vdom.Node.td []
        [
          txt (String.concat ", " (List.map (fun (k, v) -> k ^ "=" ^ v) e.ae_values));
        ];
      Vdom.Node.td [] [ txt e.ae_submitted_at ];
      Vdom.Node.td ~key:e.ae_id []
        [
          div ~cls:"row"
            [
              (match e.ae_status with
              | P.Pending ->
                  div []
                    [
                      button "Claim" (ctx.dispatch (P.Claim_entry { admin_token = Option.value (Option.map (fun (a: P.admin_identity_view) -> a.token) ctx.state.admin_identity) ~default:""; entry_id = e.ae_id }));
                      button "Resolve" (ctx.dispatch (P.Resolve_entry { admin_token = Option.value (Option.map (fun (a: P.admin_identity_view) -> a.token) ctx.state.admin_identity) ~default:""; entry_id = e.ae_id }));
                      button ~cls:"danger" "Deny" (ctx.dispatch (P.Deny_entry { admin_token = Option.value (Option.map (fun (a: P.admin_identity_view) -> a.token) ctx.state.admin_identity) ~default:""; entry_id = e.ae_id }));
                    ]
              | P.Claimed ->
                  div []
                    [
                      button "Unclaim" (ctx.dispatch (P.Unclaim_entry { admin_token = Option.value (Option.map (fun (a: P.admin_identity_view) -> a.token) ctx.state.admin_identity) ~default:""; entry_id = e.ae_id }));
                      button "Resolve" (ctx.dispatch (P.Resolve_entry { admin_token = Option.value (Option.map (fun (a: P.admin_identity_view) -> a.token) ctx.state.admin_identity) ~default:""; entry_id = e.ae_id }));
                      button ~cls:"danger" "Deny" (ctx.dispatch (P.Deny_entry { admin_token = Option.value (Option.map (fun (a: P.admin_identity_view) -> a.token) ctx.state.admin_identity) ~default:""; entry_id = e.ae_id }));
                    ]
              | P.Resolved | P.Denied ->
                  button ~cls:"secondary" "Reopen" (ctx.dispatch (P.Reopen_entry { admin_token = Option.value (Option.map (fun (a: P.admin_identity_view) -> a.token) ctx.state.admin_identity) ~default:""; entry_id = e.ae_id }))
              | P.Left -> txt "—");
            ];
        ];
    ]

let admin_token ctx = Option.value (Option.map (fun (a : P.admin_identity_view) -> a.token) ctx.state.admin_identity) ~default:""

let create_queue_form ctx =
  card
    [
      Vdom.Node.h3 [] [ txt "Create a queue" ];
      div ~cls:"col"
        [
          text_input ~ctx ~key:"cq_name" ~placeholder:"Queue name" ();
          checkbox ~ctx ~key:"cq_allow_guests" ~label:"Allow guests" ();
          checkbox ~ctx ~key:"cq_is_public" ~label:"Public (listed for anyone)" ();
          txt "Fields, one per line as: label|required(0/1)|comma,separated,options";
          Vdom.Node.textarea
            [
              Vdom.Attr.string_property "value" (S.field "cq_fields" ctx.state);
              Vdom.Attr.on_input (fun _ v -> ctx.update (S.set_field "cq_fields" v));
            ]
            [];
          button "Create"
            (let lines = String.split_on_char '\n' (S.field "cq_fields" ctx.state) in
             let fields =
               List.filter_map
                 (fun line ->
                   let line = String.trim line in
                   if line = "" then None
                   else
                     match String.split_on_char '|' line with
                     | label :: rest ->
                         let required = match rest with r :: _ -> String.trim r = "1" | [] -> false in
                         let options =
                           match rest with
                           | _ :: opts :: _ when String.trim opts <> "" ->
                               List.map String.trim (String.split_on_char ',' opts)
                           | _ -> []
                         in
                         Some { P.key = ""; label = String.trim label; required; options }
                     | [] -> None)
                 lines
             in
             ctx.dispatch
               (P.Create_queue
                  {
                    admin_token = admin_token ctx; name = S.field "cq_name" ctx.state; fields;
                    allow_guests = S.is_checked "cq_allow_guests" ctx.state;
                    is_public = S.is_checked "cq_is_public" ctx.state; opens_at = None; weekly_schedule = None;
                  }));
        ];
    ]

let selected_queue_panel ctx (q : P.admin_queue_view) =
  card
    [
      Vdom.Node.h3 [] [ txt (q.sel_summary.name ^ " (" ^ q.sel_summary.code ^ ")") ];
      div ~cls:"row"
        [
          txt (Printf.sprintf "Waiting: %d — Active: %d" q.sel_summary.waiting_count q.sel_summary.active_count);
          button ~cls:"danger" "Close queue" (ctx.dispatch (P.Close_queue { admin_token = admin_token ctx; queue_id = q.sel_summary.id }));
        ];
      Vdom.Node.table []
        [
          Vdom.Node.thead []
            [
              Vdom.Node.tr []
                (List.map (fun h -> Vdom.Node.th [] [ txt h ]) [ "Requester"; "Status"; "Claimed by"; "Values"; "Submitted"; "Actions" ]);
            ];
          Vdom.Node.tbody [] (List.map (entry_row ctx) q.sel_entries);
        ];
    ]

let accounts_panel ctx (state : P.admin_state_view) =
  card
    [
      Vdom.Node.h3 [] [ txt "Accounts" ];
      Vdom.Node.table []
        [
          Vdom.Node.tbody []
            (List.map
               (fun (a : P.account_view) ->
                 Vdom.Node.tr []
                   [
                     Vdom.Node.td [] [ txt a.name ]; Vdom.Node.td [] [ txt a.email ];
                     Vdom.Node.td [] [ txt (role_to_label a.role) ];
                     Vdom.Node.td []
                       [
                         (if a.id <> state.as_admin.account_id then
                            button ~cls:"danger" "Delete"
                              (ctx.dispatch (P.Delete_account { admin_token = admin_token ctx; account_id = a.id }))
                          else txt "you");
                       ];
                   ])
               state.as_accounts);
        ];
      Vdom.Node.h3 [] [ txt "Add account" ];
      div ~cls:"col"
        [
          text_input ~ctx ~key:"acc_name" ~placeholder:"Name" ();
          text_input ~ctx ~key:"acc_email" ~placeholder:"Email" ();
          text_input ~ctx ~key:"acc_password" ~placeholder:"Password" ~kind:"password" ();
          Vdom.Node.select
            [ Vdom.Attr.on_change (fun _ v -> ctx.update (S.set_field "acc_role" v)) ]
            [
              Vdom.Node.option [ Vdom.Attr.value "user" ] [ txt "User" ];
              Vdom.Node.option [ Vdom.Attr.value "admin" ] [ txt "Admin" ];
            ];
          button "Create account"
            (ctx.dispatch
               (P.Create_account
                  {
                    admin_token = admin_token ctx; name = S.field "acc_name" ctx.state;
                    email = S.field "acc_email" ctx.state; password = S.field "acc_password" ctx.state;
                    role = role_of_string (S.field "acc_role" ctx.state);
                  }));
        ];
    ]

let site_settings_panel ctx (settings : P.site_settings_view) =
  card
    [
      Vdom.Node.h3 [] [ txt "Site settings" ];
      div ~cls:"col"
        [
          text_input ~ctx ~key:"site_title" ~placeholder:settings.site_title ();
          button "Save"
            (ctx.dispatch
               (P.Update_site_settings
                  {
                    admin_token = admin_token ctx;
                    site_title =
                      (let v = S.field "site_title" ctx.state in
                       if v = "" then settings.site_title else v);
                    admin_password_sign_in_enabled = settings.admin_password_sign_in_enabled;
                    admin_microsoft_sign_in_enabled = settings.admin_microsoft_sign_in_enabled;
                    user_password_sign_in_enabled = settings.user_password_sign_in_enabled;
                    user_microsoft_sign_in_enabled = settings.user_microsoft_sign_in_enabled;
                  }));
        ];
    ]

let archived_panel (state : P.admin_state_view) =
  card
    [
      Vdom.Node.h3 [] [ txt "Archived queues" ];
      div ~cls:"col"
        (List.map
           (fun (a : P.archived_queue_list_item) ->
             div ~cls:"row"
               [
                 txt (Printf.sprintf "%s — closed %s by %s (%d entries)" a.arc_summary.name a.arc_closed_at a.arc_closed_by_name a.arc_entry_count);
               ])
           state.as_archived_queues);
    ]

let admin_dashboard ctx =
  match ctx.state.admin_state with
  | None -> card [ txt "Loading admin dashboard…" ]
  | Some state ->
      div
        [
          Vdom.Node.nav [ Vdom.Attr.class_ "top" ]
            [
              txt (Printf.sprintf "Signed in as %s (%s)" state.as_admin.ai_name (if state.as_admin.is_super_admin then "super admin" else "admin"));
              button ~cls:"secondary" "Sign out" (ctx.update (fun s -> S.clear_admin_token (); { s with admin_identity = None; admin_state = None; page = S.Home_page }));
            ];
          div ~cls:"sidebar-layout"
            [
              card
                (Vdom.Node.h3 [] [ txt "Queues" ]
                :: List.map
                     (fun (q : P.admin_queue_list_item) ->
                       div ~cls:"row"
                         [
                           button ~cls:"secondary"
                             (Printf.sprintf "%s (%d)" q.aq_summary.name q.aq_summary.waiting_count)
                             (ctx.dispatch (P.Subscribe_admin { admin_token = admin_token ctx; selected_queue_id = Some q.aq_summary.id }));
                         ])
                     state.as_queues);
              div
                [
                  (match state.as_selected_queue with Some q -> selected_queue_panel ctx q | None -> card [ txt "No queues yet — create one below." ]);
                  create_queue_form ctx;
                  (if state.as_admin.is_super_admin then accounts_panel ctx state else txt "");
                  (if state.as_admin.is_super_admin then site_settings_panel ctx state.as_site_settings else txt "");
                  archived_panel state;
                ];
            ];
        ]

(* --------------------------- top-level --------------------------- *)

let render ctx =
  div ~cls:"app-shell"
    [
      banner ctx;
      (match ctx.state.page with
      | S.Loading -> card [ txt "Connecting…" ]
      | S.Setup_page -> setup_view ctx
      | S.Home_page -> home_view ctx
      | S.Admin_login_page -> admin_login_view ctx
      | S.User_login_page -> user_login_view ctx
      | S.Admin_page -> admin_dashboard ctx
      | S.Queue_page queue_id -> queue_page_view ctx queue_id
      | S.Public_queues_page -> public_queues_view ctx);
    ]
