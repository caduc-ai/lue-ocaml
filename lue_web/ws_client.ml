(* Minimal WebSocket + localStorage bindings built on [Js_of_ocaml.Js.Unsafe]
   rather than a specific versioned browser-API module, to stay resilient to
   js_of_ocaml API drift across versions. *)

open Js_of_ocaml

let ws_url () =
  let loc = Dom_html.window##.location in
  let protocol = if Js.to_string loc##.protocol = "https:" then "wss:" else "ws:" in
  Printf.sprintf "%s//%s/ws" protocol (Js.to_string loc##.host)

let socket : Js.Unsafe.any option ref = ref None
let queue : string Queue.t = Queue.create ()

let raw_send (ws : Js.Unsafe.any) (data : string) =
  ignore (Js.Unsafe.meth_call ws "send" [| Js.Unsafe.inject (Js.string data) |])

let send (data : string) =
  match !socket with
  | Some ws when Js.Unsafe.get ws "readyState" = 1 -> raw_send ws data
  | _ -> Queue.push data queue

let flush_queue (ws : Js.Unsafe.any) =
  while not (Queue.is_empty queue) do
    raw_send ws (Queue.pop queue)
  done

let rec connect ~on_message ~on_open ~on_close =
  let ctor = Js.Unsafe.global##._WebSocket in
  let ws = Js.Unsafe.new_obj ctor [| Js.Unsafe.inject (Js.string (ws_url ())) |] in
  Js.Unsafe.set ws "onopen"
    (Js.wrap_callback (fun _ev ->
         socket := Some ws;
         flush_queue ws;
         on_open ()));
  Js.Unsafe.set ws "onmessage"
    (Js.wrap_callback (fun ev ->
         let data = Js.Unsafe.get ev "data" in
         on_message (Js.to_string data)));
  Js.Unsafe.set ws "onclose"
    (Js.wrap_callback (fun _ev ->
         socket := None;
         on_close ();
         (* naive auto-reconnect after 3s *)
         ignore
           (Dom_html.window##setTimeout
              (Js.wrap_callback (fun () -> connect ~on_message ~on_open ~on_close))
              3000.)));
  ()

(* --- localStorage --- *)

let local_storage_get key =
  try
    let v = Js.Unsafe.meth_call Dom_html.window##.localStorage "getItem" [| Js.Unsafe.inject (Js.string key) |] in
    if Js.Opt.test (Js.some v) && not (Js.Unsafe.equals v Js.null) && not (Js.Unsafe.equals v Js.undefined) then
      Some (Js.to_string v)
    else None
  with _ -> None

let local_storage_set key value =
  try
    ignore
      (Js.Unsafe.meth_call Dom_html.window##.localStorage "setItem"
         [| Js.Unsafe.inject (Js.string key); Js.Unsafe.inject (Js.string value) |])
  with _ -> ()

let local_storage_remove key =
  try ignore (Js.Unsafe.meth_call Dom_html.window##.localStorage "removeItem" [| Js.Unsafe.inject (Js.string key) |])
  with _ -> ()

(* --- location hash routing --- *)

let current_hash () =
  let h = Js.to_string Dom_html.window##.location##.hash in
  if String.length h > 0 && h.[0] = '#' then String.sub h 1 (String.length h - 1) else h

let set_hash h = Dom_html.window##.location##.hash := Js.string h

let on_hash_change f =
  Dom_html.window##.onhashchange :=
    Dom_html.handler (fun _ ->
        f (current_hash ());
        Js._true)
