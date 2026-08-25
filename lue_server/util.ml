let is_blank s = String.trim s = ""

let slugify value =
  let buf = Buffer.create (String.length value) in
  String.iter
    (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') then Buffer.add_char buf c
      else if c >= 'A' && c <= 'Z' then Buffer.add_char buf (Char.lowercase_ascii c)
      else Buffer.add_char buf '_')
    value;
  let s = Buffer.contents buf in
  (* collapse consecutive underscores, then trim leading/trailing ones,
     mirroring the Rust slugify helper closely enough for our own keys *)
  let collapsed = Buffer.create (String.length s) in
  let prev_underscore = ref false in
  String.iter
    (fun c ->
      if c = '_' then (
        if not !prev_underscore then Buffer.add_char collapsed '_';
        prev_underscore := true)
      else (
        Buffer.add_char collapsed c;
        prev_underscore := false))
    s;
  let s = Buffer.contents collapsed in
  let len = String.length s in
  let start = ref 0 in
  while !start < len && s.[!start] = '_' do
    incr start
  done;
  let stop = ref (len - 1) in
  while !stop >= !start && s.[!stop] = '_' do
    decr stop
  done;
  if !stop < !start then "" else String.sub s !start (!stop - !start + 1)

let normalize_options options =
  List.fold_left
    (fun acc opt ->
      let opt = String.trim opt in
      if opt = "" || List.mem opt acc then acc else acc @ [ opt ])
    [] options

let normalize_fields (fields : Lue_shared.Protocol.queue_field list) :
    (Lue_shared.Protocol.queue_field list, string) result =
  let open Lue_shared.Protocol in
  let seen = Hashtbl.create 8 in
  try
    let normalized =
      List.map
        (fun (field : queue_field) ->
          let label = String.trim field.label in
          if label = "" then failwith "field labels cannot be empty";
          let key = if is_blank field.key then slugify label else slugify (String.trim field.key) in
          if key = "" then failwith (Printf.sprintf "field label '%s' produced an empty key" label);
          if Hashtbl.mem seen key then failwith (Printf.sprintf "duplicate field key '%s'" key);
          Hashtbl.add seen key true;
          { key; label; required = field.required; options = normalize_options field.options })
        fields
    in
    Ok normalized
  with Failure msg -> Error msg

let normalize_email value =
  let normalized = String.trim value |> String.lowercase_ascii in
  if normalized = "" || not (String.contains normalized '@') then
    Error "a valid email is required"
  else Ok normalized

let is_requester_name_key key = key = "name" || key = "full_name"
