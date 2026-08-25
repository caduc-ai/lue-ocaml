(* UUID / token generation and RFC3339-ish timestamp helpers.
   Intentionally dependency-free (stdlib [Random] + [Unix] only) since this
   is a from-scratch OCaml rewrite and not expected to interop byte-for-byte
   with any other implementation. *)

let () = Random.self_init ()

let hex_of_bytes bytes =
  let buf = Buffer.create (Bytes.length bytes * 2) in
  Bytes.iter (fun c -> Buffer.add_string buf (Printf.sprintf "%02x" (Char.code c))) bytes;
  Buffer.contents buf

let random_bytes n =
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    Bytes.set b i (Char.chr (Random.int 256))
  done;
  b

let random_hex n = hex_of_bytes (random_bytes n)

(* UUID v4-shaped random id; not used for anything requiring RFC compliance,
   just uniqueness and a familiar shape. *)
let uuid4 () =
  let b = random_bytes 16 in
  Bytes.set b 6 (Char.chr ((Char.code (Bytes.get b 6) land 0x0f) lor 0x40));
  Bytes.set b 8 (Char.chr ((Char.code (Bytes.get b 8) land 0x3f) lor 0x80));
  let h = hex_of_bytes b in
  Printf.sprintf "%s-%s-%s-%s-%s"
    (String.sub h 0 8) (String.sub h 8 4) (String.sub h 12 4) (String.sub h 16 4)
    (String.sub h 20 12)

let token () = random_hex 24

(* --- timestamps, treated as UTC throughout --- *)

let tz_offset =
  let epoch0_utc_tm = Unix.gmtime 0.0 in
  let t, _ = Unix.mktime epoch0_utc_tm in
  t

let iso_of_epoch epoch =
  let tm = Unix.gmtime epoch in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec

let now_epoch () = Unix.gettimeofday ()
let now_iso () = iso_of_epoch (now_epoch ())

let epoch_of_iso s =
  try
    Scanf.sscanf s "%d-%d-%dT%d:%d:%d" (fun y mo d h mi se ->
        let tm =
          Unix.
            {
              tm_year = y - 1900; tm_mon = mo - 1; tm_mday = d; tm_hour = h; tm_min = mi;
              tm_sec = se; tm_wday = 0; tm_yday = 0; tm_isdst = false;
            }
        in
        let t, _ = Unix.mktime tm in
        Some (t -. tz_offset))
  with _ -> None

let now_weekday_and_minute () =
  let tm = Unix.gmtime (now_epoch ()) in
  (tm.Unix.tm_wday, (tm.Unix.tm_hour * 60) + tm.Unix.tm_min)
