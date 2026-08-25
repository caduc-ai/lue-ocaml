open Bonsai_web
module P = Lue_shared.Protocol
module S = State

type ctx = {
  state : S.t;
  update : (S.t -> S.t) -> unit Vdom.Effect.t;
  dispatch : P.client_message -> unit Vdom.Effect.t;
  navigate : string -> unit Vdom.Effect.t;
}

(* --------------------------- small DOM helpers --------------------------- *)

let txt s = Vdom.Node.text s
let none_ = Vdom.Node.none
let div ?(cls = "") children = Vdom.Node.div ~attrs:[ Vdom.Attr.class_ cls ] children
let card ?(cls = "") children = div ~cls:("card " ^ cls) children
let h1 s = Vdom.Node.h1 ~attrs:[] [ txt s ]
let h2 s = Vdom.Node.h2 ~attrs:[] [ txt s ]
let h3 s = Vdom.Node.h3 ~attrs:[] [ txt s ]
let h4 s = Vdom.Node.h4 ~attrs:[] [ txt s ]
let lede s = Vdom.Node.p ~attrs:[ Vdom.Attr.class_ "lede" ] [ txt s ]

let button ?(cls = "") ?(disabled = false) label on_click =
  Vdom.Node.button
    ~attrs:[
      Vdom.Attr.class_ cls; Vdom.Attr.on_click (fun _ -> on_click);
      (if disabled then Vdom.Attr.disabled else Vdom.Attr.empty);
    ]
    [ txt label ]

let site_title ctx =
  Option.fold ctx.state.S.site_settings ~none:"Lue" ~some:(fun (s : P.site_settings_view) -> s.site_title)

let brand =
  div ~cls:"brand" [ div ~cls:"logo-mark" [ txt "L" ]; txt "Lue" ]

(* --------------------------- form helpers --------------------------- *)

let field_label label = Vdom.Node.label ~attrs:[ Vdom.Attr.class_ "field-label" ] [ txt label ]

let text_input ~ctx ~key ~placeholder ?(kind = "text") () =
  Vdom.Node.input
    ~attrs:[
      Vdom.Attr.type_ kind; Vdom.Attr.placeholder placeholder;
      Vdom.Attr.string_property "value" (S.field key ctx.state);
      Vdom.Attr.on_input (fun _ value -> ctx.update (S.set_field key value));
    ]
    ()

let labeled_input ~ctx ~key ~label ~placeholder ?(kind = "text") () =
  div ~cls:"field" [ field_label label; text_input ~ctx ~key ~placeholder ~kind () ]

let checkbox ~ctx ~key ~label () =
  Vdom.Node.label ~attrs:[ Vdom.Attr.class_ "check" ]
    [
      Vdom.Node.input
        ~attrs:[
          Vdom.Attr.type_ "checkbox"; Vdom.Attr.bool_property "checked" (S.is_checked key ctx.state);
          Vdom.Attr.on_click (fun _ -> ctx.update (S.toggle_checked key));
        ]
        ();
      txt label;
    ]

let banner ctx =
  div
    [
      (match ctx.state.error with
      | Some msg -> div ~cls:"banner error" [ txt "⚠ "; txt msg ]
      | None -> none_);
      (match ctx.state.info with
      | Some msg -> div ~cls:"banner info" [ txt "✓ "; txt msg ]
      | None -> none_);
    ]

let empty_state ~icon ~text_ ?action () =
  div ~cls:"empty-state"
    (div ~cls:"icon" [ txt icon ] :: txt text_ :: (match action with Some a -> [ Vdom.Node.div ~attrs:[] [ a ] ] | None -> []))

let stat label value =
  div ~cls:"stat" [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "value" ] [ txt value ]; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "label" ] [ txt label ] ]

let role_of_string = function
  | "admin" -> P.Admin
  | "super_admin" -> P.Super_admin
  | _ -> P.User

let role_to_label = function P.Super_admin -> "Super admin" | P.Admin -> "Admin" | P.User -> "User"

let status_label = function
  | P.Pending -> "Waiting" | P.Claimed -> "Being helped" | P.Left -> "Left"
  | P.Resolved -> "Resolved" | P.Denied -> "Denied"

let status_class = function
  | P.Pending -> "status-pending" | P.Claimed -> "status-claimed" | P.Left -> "status-left"
  | P.Resolved -> "status-resolved" | P.Denied -> "status-denied"

let status_pill status =
  Vdom.Node.span ~attrs:[ Vdom.Attr.class_ ("pill " ^ status_class status) ]
    [ Vdom.Node.span ~attrs:[ Vdom.Attr.class_ ("dot " ^ status_class status) ] []; txt (status_label status) ]

let admin_token ctx = Option.fold ctx.state.S.admin_identity ~none:"" ~some:(fun (a : P.admin_identity_view) -> a.ai_token)

(* --------------------------- shared chrome --------------------------- *)

let top_nav ctx ~right =
  Vdom.Node.create "nav" ~attrs:[ Vdom.Attr.class_ "top" ]
    [
      Vdom.Node.div ~attrs:[ Vdom.Attr.on_click (fun _ -> ctx.navigate "/") ] [ brand ];
      div ~cls:"row" right;
    ]

let centered card_children = div ~cls:"center-page" [ div ~cls:"auth-card" card_children ]

(* --------------------------- setup / login --------------------------- *)

let setup_view ctx =
  centered
    [
      div ~cls:"row" [ brand ];
      card ~cls:"hero"
        [
          h1 "Welcome to Lue";
          lede "This is a fresh install — create the first super admin account to get started.";
          div ~cls:"stack"
            [
              labeled_input ~ctx ~key:"setup_name" ~label:"Name" ~placeholder:"Ada Lovelace" ();
              labeled_input ~ctx ~key:"setup_email" ~label:"Email" ~placeholder:"you@example.com" ~kind:"email" ();
              labeled_input ~ctx ~key:"setup_password" ~label:"Password" ~placeholder:"••••••••" ~kind:"password" ();
              button ~cls:"block" "Create super admin"
                (ctx.dispatch
                   (P.Setup_super_admin
                      {
                        name = S.field "setup_name" ctx.state; email = S.field "setup_email" ctx.state;
                        password = S.field "setup_password" ctx.state;
                      }));
            ];
        ];
    ]

let home_view ctx =
  centered
    [
      div ~cls:"row" [ brand ];
      card ~cls:"hero"
        [
          h1 (site_title ctx); lede "A live queue manager. Sign in, or jump straight to a queue.";
          div ~cls:"stack"
            [
              div ~cls:"button-row"
                [
                  button "Admin sign in" (ctx.navigate "/admin-login");
                  button ~cls:"secondary" "User sign in" (ctx.navigate "/user-login");
                  button ~cls:"secondary" "Browse public queues" (ctx.navigate "/public");
                ];
              Vdom.Node.hr ~attrs:[ Vdom.Attr.class_ "divider" ] ();
              div ~cls:"field"
                [
                  field_label "Have a queue code?";
                  div ~cls:"row"
                    [
                      div [ text_input ~ctx ~key:"join_code" ~placeholder:"ABC123" () ];
                      button "Go" (ctx.dispatch (P.Resolve_queue_code { code = S.field "join_code" ctx.state }));
                    ];
                ];
            ];
        ];
    ]

let admin_login_view ctx =
  centered
    [
      div ~cls:"row" [ brand ];
      card ~cls:"hero"
        [
          h2 "Admin sign in"; lede "Manage queues, accounts, and settings.";
          div ~cls:"stack"
            [
              labeled_input ~ctx ~key:"admin_email" ~label:"Email" ~placeholder:"you@example.com" ~kind:"email" ();
              labeled_input ~ctx ~key:"admin_password" ~label:"Password" ~placeholder:"••••••••" ~kind:"password" ();
              div ~cls:"button-row"
                [
                  button "Sign in"
                    (ctx.dispatch
                       (P.Login_admin { email = S.field "admin_email" ctx.state; password = S.field "admin_password" ctx.state }));
                  button ~cls:"ghost" "Back" (ctx.navigate "/");
                ];
            ];
        ];
    ]

let user_login_view ctx =
  centered
    [
      div ~cls:"row" [ brand ];
      card ~cls:"hero"
        [
          h2 "Sign in"; lede "Sign in to join queues from your account.";
          div ~cls:"stack"
            [
              labeled_input ~ctx ~key:"user_email" ~label:"Email" ~placeholder:"you@example.com" ~kind:"email" ();
              labeled_input ~ctx ~key:"user_password" ~label:"Password" ~placeholder:"••••••••" ~kind:"password" ();
              div ~cls:"button-row"
                [
                  button "Sign in"
                    (ctx.dispatch
                       (P.Login_user { email = S.field "user_email" ctx.state; password = S.field "user_password" ctx.state }));
                  button ~cls:"ghost" "Back" (ctx.navigate "/");
                ];
            ];
        ];
    ]

let public_queues_view ctx =
  div ~cls:"stack"
    [
      top_nav ctx ~right:[ button ~cls:"secondary" "Home" (ctx.navigate "/") ];
      card
        [
          h2 "Public queues"; lede "Anyone can join these without signing in.";
          (match ctx.state.public_queues with
          | [] -> empty_state ~icon:"🪄" ~text_:"No public queues are open right now." ()
          | queues ->
              div ~cls:"queue-list"
                (List.map
                   (fun (q : P.queue_summary) ->
                     Vdom.Node.button
                       ~attrs:[ Vdom.Attr.class_ "queue-item"; Vdom.Attr.on_click (fun _ -> ctx.navigate ("/queue/" ^ q.id)) ]
                       [
                         div ~cls:"row between"
                           [
                             div
                               [
                                 div ~cls:"name" [ txt q.name ];
                                 div ~cls:"meta" [ txt (Printf.sprintf "code %s" q.code) ];
                               ];
                             Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "pill" ] [ txt (Printf.sprintf "%d waiting" q.waiting_count) ];
                           ];
                       ])
                   queues));
        ];
    ]

(* --------------------------- queue join / status --------------------------- *)

let field_row ctx (f : P.queue_field) =
  div ~cls:"field"
    [
      field_label (f.label ^ if f.required then " *" else "");
      (if f.options = [] then text_input ~ctx ~key:("qf_" ^ f.key) ~placeholder:f.label ()
       else
         Vdom.Node.select
           ~attrs:[ Vdom.Attr.on_change (fun _ value -> ctx.update (S.set_field ("qf_" ^ f.key) value)) ]
           (Vdom.Node.option ~attrs:[] [ txt "Select…" ]
           :: List.map (fun opt -> Vdom.Node.option ~attrs:[ Vdom.Attr.value opt ] [ txt opt ]) f.options));
    ]

let queue_page_view ctx queue_id =
  div ~cls:"stack"
    [
      top_nav ctx ~right:[ button ~cls:"secondary" "Home" (ctx.navigate "/") ];
      (match ctx.state.queue_view with
      | None -> card [ empty_state ~icon:"⏳" ~text_:"Loading queue…" () ]
      | Some (q, your_entry) ->
          div ~cls:"stack"
            [
              (match q.uq_closed_at with
              | Some at ->
                  div ~cls:"banner error"
                    [ txt (Printf.sprintf "This queue was closed at %s%s." at (Option.fold q.uq_closed_by_name ~none:"" ~some:(fun n -> " by " ^ n))) ]
              | None -> none_);
              card
                [
                  div ~cls:"row between"
                    [
                      div [ h2 q.uq_name; Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "mono muted" ] [ txt ("Code " ^ q.uq_code) ] ];
                      stat "Waiting" (string_of_int q.uq_waiting_count);
                    ];
                  Vdom.Node.hr ~attrs:[ Vdom.Attr.class_ "divider" ] ();
                  (match your_entry with
                  | Some entry ->
                      div ~cls:"stack"
                        [
                          h4 "Your status";
                          div ~cls:"row" [ status_pill entry.ue_status ];
                          (match entry.ue_position with
                          | Some p -> div ~cls:"muted" [ txt (Printf.sprintf "Position in line: #%d" p) ]
                          | None -> none_);
                          (match entry.ue_claimed_by with
                          | Some name -> div ~cls:"muted" [ txt ("Being helped by " ^ name) ]
                          | None -> none_);
                          (if entry.ue_status = P.Pending || entry.ue_status = P.Claimed then
                             button ~cls:"danger" "Leave queue"
                               (ctx.dispatch (P.Leave_queue { queue_id; entry_token = entry.ue_token }))
                           else none_);
                        ]
                  | None ->
                      div ~cls:"stack"
                        (h4 "Join this queue"
                        :: List.map (field_row ctx) q.uq_fields
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
                ];
            ]);
    ]

(* --------------------------- admin dashboard --------------------------- *)

let entry_action_buttons ctx (e : P.admin_entry_view) =
  let token = admin_token ctx in
  match e.ae_status with
  | P.Pending ->
      div ~cls:"button-row"
        [
          button ~cls:"small" "Claim" (ctx.dispatch (P.Claim_entry { admin_token = token; entry_id = e.ae_id }));
          button ~cls:"small success" "Resolve" (ctx.dispatch (P.Resolve_entry { admin_token = token; entry_id = e.ae_id }));
          button ~cls:"small danger" "Deny" (ctx.dispatch (P.Deny_entry { admin_token = token; entry_id = e.ae_id }));
        ]
  | P.Claimed ->
      div ~cls:"button-row"
        [
          button ~cls:"small secondary" "Unclaim" (ctx.dispatch (P.Unclaim_entry { admin_token = token; entry_id = e.ae_id }));
          button ~cls:"small success" "Resolve" (ctx.dispatch (P.Resolve_entry { admin_token = token; entry_id = e.ae_id }));
          button ~cls:"small danger" "Deny" (ctx.dispatch (P.Deny_entry { admin_token = token; entry_id = e.ae_id }));
        ]
  | P.Resolved | P.Denied ->
      button ~cls:"small secondary" "Reopen" (ctx.dispatch (P.Reopen_entry { admin_token = token; entry_id = e.ae_id }))
  | P.Left -> Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "faint" ] [ txt "—" ]

let entry_row ctx (e : P.admin_entry_view) =
  Vdom.Node.tr ~attrs:[]
    [
      Vdom.Node.td ~attrs:[]
        [
          div ~cls:"row"
            [ txt e.ae_requester_label; (if e.ae_is_guest then Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "pill guest" ] [ txt "guest" ] else none_) ];
        ];
      Vdom.Node.td ~attrs:[] [ status_pill e.ae_status ];
      Vdom.Node.td ~attrs:[] [ Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "muted" ] [ txt (Option.value e.ae_claimed_by ~default:"—") ] ];
      Vdom.Node.td ~attrs:[]
        [ Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "faint" ] [ txt (String.concat " · " (List.map (fun (k, v) -> k ^ ": " ^ v) e.ae_values)) ] ];
      Vdom.Node.td ~attrs:[] [ Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "faint" ] [ txt e.ae_submitted_at ] ];
      Vdom.Node.td ~key:e.ae_id ~attrs:[] [ entry_action_buttons ctx e ];
    ]

let entries_table ctx entries =
  match entries with
  | [] -> empty_state ~icon:"📭" ~text_:"No one has joined this queue yet." ()
  | entries ->
      div ~cls:"table-wrap"
        [
          Vdom.Node.table ~attrs:[]
            [
              Vdom.Node.thead ~attrs:[]
                [
                  Vdom.Node.tr ~attrs:[]
                    (List.map (fun h -> Vdom.Node.th ~attrs:[] [ txt h ]) [ "Requester"; "Status"; "Claimed by"; "Values"; "Submitted"; "" ]);
                ];
              Vdom.Node.tbody ~attrs:[] (List.map (entry_row ctx) entries);
            ];
        ]

let create_queue_form ctx =
  card
    [
      h3 "Create a queue"; lede "Add optional fields people fill out when joining.";
      div ~cls:"stack"
        [
          labeled_input ~ctx ~key:"cq_name" ~label:"Queue name" ~placeholder:"Office hours" ();
          div ~cls:"row"
            [ checkbox ~ctx ~key:"cq_allow_guests" ~label:"Allow guests" (); checkbox ~ctx ~key:"cq_is_public" ~label:"Public listing" () ];
          div ~cls:"field"
            [
              field_label "Fields";
              Vdom.Node.textarea
                ~attrs:[
                  Vdom.Attr.string_property "value" (S.field "cq_fields" ctx.state);
                  Vdom.Attr.placeholder "Name|1|\nQuestion topic|0|Homework,Exam,Other";
                  Vdom.Attr.on_input (fun _ v -> ctx.update (S.set_field "cq_fields" v));
                ]
                [];
              Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hint" ] [ txt "One per line: label | required (1/0) | comma,separated,options (optional)" ];
            ];
          button "Create queue"
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
                           | _ :: opts :: _ when String.trim opts <> "" -> List.map String.trim (String.split_on_char ',' opts)
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
      div ~cls:"row between"
        [
          div [ h3 q.sel_summary.name; Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "mono muted" ] [ txt ("Code " ^ q.sel_summary.code) ] ];
          div ~cls:"row"
            [
              stat "Waiting" (string_of_int q.sel_summary.waiting_count);
              stat "Active" (string_of_int q.sel_summary.active_count);
            ];
        ];
      div ~cls:"row end" [ button ~cls:"danger small" "Close queue" (ctx.dispatch (P.Close_queue { admin_token = admin_token ctx; queue_id = q.sel_summary.id })) ];
      Vdom.Node.hr ~attrs:[ Vdom.Attr.class_ "divider" ] ();
      entries_table ctx q.sel_entries;
    ]

let accounts_panel ctx (state : P.admin_state_view) =
  card
    [
      h3 "Accounts";
      (match state.as_accounts with
      | [] -> empty_state ~icon:"👤" ~text_:"No accounts yet." ()
      | accounts ->
          div ~cls:"table-wrap"
            [
              Vdom.Node.table ~attrs:[]
                [
                  Vdom.Node.tbody ~attrs:[]
                    (List.map
                       (fun (a : P.account_view) ->
                         Vdom.Node.tr ~attrs:[]
                           [
                             Vdom.Node.td ~attrs:[] [ txt a.name ];
                             Vdom.Node.td ~attrs:[] [ Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "muted" ] [ txt a.email ] ];
                             Vdom.Node.td ~attrs:[] [ Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "pill role" ] [ txt (role_to_label a.role) ] ];
                             Vdom.Node.td ~attrs:[]
                               [
                                 (if a.id <> state.as_admin.account_id then
                                    button ~cls:"small danger"
                                      "Delete" (ctx.dispatch (P.Delete_account { admin_token = admin_token ctx; account_id = a.id }))
                                  else Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "faint" ] [ txt "you" ]);
                               ];
                           ])
                       accounts);
                ];
            ]);
      Vdom.Node.hr ~attrs:[ Vdom.Attr.class_ "divider" ] ();
      h4 "Add account";
      div ~cls:"grid-2"
        [
          labeled_input ~ctx ~key:"acc_name" ~label:"Name" ~placeholder:"Name" ();
          labeled_input ~ctx ~key:"acc_email" ~label:"Email" ~placeholder:"Email" ();
          labeled_input ~ctx ~key:"acc_password" ~label:"Password" ~placeholder:"Password" ~kind:"password" ();
          div ~cls:"field"
            [
              field_label "Role";
              Vdom.Node.select
                ~attrs:[ Vdom.Attr.on_change (fun _ v -> ctx.update (S.set_field "acc_role" v)) ]
                [
                  Vdom.Node.option ~attrs:[ Vdom.Attr.value "user" ] [ txt "User" ];
                  Vdom.Node.option ~attrs:[ Vdom.Attr.value "admin" ] [ txt "Admin" ];
                ];
            ];
        ];
      div ~cls:"row end"
        [
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
      h3 "Site settings";
      div ~cls:"row end"
        [
          div ~cls:"field"
            [ field_label "Site title"; text_input ~ctx ~key:"site_title" ~placeholder:settings.site_title () ];
          button "Save"
            (ctx.dispatch
               (P.Update_site_settings
                  {
                    admin_token = admin_token ctx;
                    site_title = (let v = S.field "site_title" ctx.state in if v = "" then settings.site_title else v);
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
      h3 "Archived queues";
      (match state.as_archived_queues with
      | [] -> empty_state ~icon:"🗄" ~text_:"Nothing archived yet." ()
      | archived ->
          div ~cls:"col"
            (List.map
               (fun (a : P.archived_queue_list_item) ->
                 div ~cls:"row between"
                   [
                     div [ Vdom.Node.strong ~attrs:[] [ txt a.arc_summary.name ]; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "faint" ] [ txt (Printf.sprintf "closed %s by %s" a.arc_closed_at a.arc_closed_by_name) ] ];
                     Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "pill" ] [ txt (Printf.sprintf "%d entries" a.arc_entry_count) ];
                   ])
               archived));
    ]

let queue_sidebar ctx (state : P.admin_state_view) =
  card ~cls:"flush tight"
    [
      div ~cls:"row between" [ h4 "Queues" ];
      (match state.as_queues with
      | [] -> empty_state ~icon:"📋" ~text_:"No queues yet." ()
      | queues ->
          div ~cls:"queue-list"
            (List.map
               (fun (q : P.admin_queue_list_item) ->
                 let active =
                   match state.as_selected_queue with Some s -> s.sel_summary.id = q.aq_summary.id | None -> false
                 in
                 Vdom.Node.button
                   ~attrs:[
                     Vdom.Attr.class_ ("queue-item" ^ if active then " active" else "");
                     Vdom.Attr.on_click (fun _ ->
                         ctx.dispatch (P.Subscribe_admin { admin_token = admin_token ctx; selected_queue_id = Some q.aq_summary.id }));
                   ]
                   [
                     div ~cls:"row between"
                       [
                         div [ div ~cls:"name" [ txt q.aq_summary.name ]; div ~cls:"meta" [ txt (q.aq_owner_name ^ " · " ^ q.aq_summary.code) ] ];
                         Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "pill status-pending" ] [ txt (string_of_int q.aq_summary.waiting_count) ];
                       ];
                   ])
               queues));
    ]

let admin_dashboard ctx =
  match ctx.state.admin_state with
  | None -> card [ empty_state ~icon:"⏳" ~text_:"Loading admin dashboard…" () ]
  | Some state ->
      div ~cls:"stack"
        [
          top_nav ctx
            ~right:
              [
                Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "who" ]
                  [
                    txt "Signed in as "; Vdom.Node.strong ~attrs:[] [ txt state.as_admin.ai_name ];
                    (if state.as_admin.is_super_admin then Vdom.Node.span ~attrs:[ Vdom.Attr.class_ "pill role" ] [ txt "Super admin" ] else none_);
                  ];
                button ~cls:"ghost" "Sign out"
                  (ctx.update (fun s ->
                       S.clear_admin_token ();
                       { s with admin_identity = None; admin_state = None; page = S.Home_page }));
              ];
          div ~cls:"sidebar-layout"
            [
              queue_sidebar ctx state;
              div ~cls:"stack"
                [
                  (match state.as_selected_queue with
                  | Some q -> selected_queue_panel ctx q
                  | None -> card [ empty_state ~icon:"✨" ~text_:"No queue selected yet — create your first one below." () ]);
                  create_queue_form ctx;
                  (if state.as_admin.is_super_admin then accounts_panel ctx state else none_);
                  (if state.as_admin.is_super_admin then site_settings_panel ctx state.as_site_settings else none_);
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
      | S.Loading -> div ~cls:"loading-shell" [ txt "Connecting to Lue…" ]
      | S.Setup_page -> setup_view ctx
      | S.Home_page -> home_view ctx
      | S.Admin_login_page -> admin_login_view ctx
      | S.User_login_page -> user_login_view ctx
      | S.Admin_page -> admin_dashboard ctx
      | S.Queue_page queue_id -> queue_page_view ctx queue_id
      | S.Public_queues_page -> public_queues_view ctx);
    ]
