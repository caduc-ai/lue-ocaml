(* Salted, iterated-hash password storage. This intentionally avoids any
   opam dependency beyond the stdlib [Digest] (MD5) module so the project
   builds with a minimal opam switch; it is not a drop-in replacement for
   Argon2 and should be swapped for a real KDF (argon2 / bcrypt / scrypt)
   before any production use. *)

let prefix = "lue1$"
let iterations = 100_000

let iterate salt password =
  let seed = Digest.string (salt ^ ":" ^ password) in
  let rec go acc n = if n <= 0 then acc else go (Digest.string (acc ^ salt ^ password)) (n - 1) in
  Digest.to_hex (go seed iterations)

let hash_password password =
  let salt = Uid.random_hex 16 in
  Ok (Printf.sprintf "%s%s$%s" prefix salt (iterate salt password))

let is_password_hash value =
  String.length value > String.length prefix
  && String.sub value 0 (String.length prefix) = prefix

let verify_password password hash =
  if not (is_password_hash hash) then false
  else
    match String.split_on_char '$' hash with
    | [ tagged_prefix; salt; digest ] when tagged_prefix ^ "$" = prefix -> (
        try iterate salt password = digest with _ -> false)
    | _ -> false
