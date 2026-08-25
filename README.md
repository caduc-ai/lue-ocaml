# Lue (OCaml port)

This is a from-scratch OCaml rewrite of [lloyd-g-w/lue](https://github.com/lloyd-g-w/lue), a
live queue manager. The original is Rust: an Axum + WebSocket backend and a Dioxus frontend,
sharing protocol types via a `shared` crate. This port keeps the same shape:

- `lue_shared/` — protocol types (`Protocol.client_message` / `Protocol.server_message`) and
  hand-written Yojson codecs, used by both sides. This is a clean-room JSON protocol (tagged
  `{"type": "...", ...}` objects); it is **not** wire-compatible with the original Rust JSON
  shape, only internally consistent between our own OCaml client and server.
- `lue_server/` — a [Dream](https://github.com/aantron/dream) backend: `/health`, a `/ws`
  WebSocket endpoint that speaks the protocol above, an in-memory `Store` mirroring
  `crates/server/src/store.rs`'s business rules (accounts, sessions, queues, entries,
  sharing/groups, site settings), and JSON-file persistence (`data/store.json` by default,
  override with `DATA_PATH`), matching `crates/server/src/persistence.rs`.
- `lue_web/` — a [Bonsai](https://github.com/janestreet/bonsai) + `js_of_ocaml` frontend:
  setup/login pages, an admin dashboard (queues, entries with claim/unclaim/resolve/deny/reopen,
  accounts, site settings, archived queues), a queue join/status page, and a public-queues
  listing, mirroring `crates/web`.

## Important: this was authored without a working OCaml toolchain

The sandbox this port was written in has **no `opam`/`dune`/`ocaml` installed**, so none of this
code has been compiled or run. It was written carefully against the documented/common APIs of
Dream, Yojson, and Bonsai, but you should expect to fix a handful of small API-signature
mismatches (argument order/labels, module paths) when you first `dune build` against your pinned
package versions — especially in `lue_web/view.ml` and `lue_web/main.ml`, since `Bonsai`/
`virtual_dom` APIs have shifted across versions more than `Dream`/`Yojson` have. Treat this as a
complete, structurally-faithful first draft rather than a tested build.

The frontend intentionally uses a simple architecture to minimize API risk: all application state
lives in one `Bonsai.Var.t` (`lue_web/state.ml`), updated either by incoming WebSocket messages or
by UI event handlers, and the whole page is a pure `render : State.t -> Vdom.Node.t` function
(`lue_web/view.ml`) wrapped once with `Bonsai.read`. That's a legitimate, if minimal, Bonsai
program (it uses `Bonsai.Var`, `Bonsai.Value.map`, `Bonsai.read`, and `Bonsai_web.Start.start`);
a more idiomatic Bonsai app would push per-page/per-form state into local `Bonsai.state`
components via `let%sub`, which we avoided here to reduce the ppx/API surface we couldn't verify.

## Scope differences from the Rust original

- **No Microsoft SSO.** The original's Azure AD login flow (`crates/server/src/auth.rs`,
  `MicrosoftAuthConfig`, `/auth/microsoft/*`) is not ported. Only email/password sign-in exists.
  `admin_microsoft_sign_in_enabled` / `user_microsoft_sign_in_enabled` fields still round-trip
  through the protocol/site-settings for parity but have no effect.
- **Password hashing** uses a dependency-free salted/iterated MD5 KDF
  (`lue_server/password.ml`) instead of Argon2, specifically to avoid needing a C-stub opam
  package in an unverified build. Swap in `argon2`/`bcrypt`/`scrypt-kdf` before any real
  deployment.
- **UUIDs/tokens** are locally generated random hex strings (`lue_server/uid.ml`) rather than the
  `uuid` crate; format-compatible enough (`8-4-4-4-12` hex) but not RFC 4122 strict.
- **Queue "create" field editor** in the admin UI is a simple textarea
  (`label|required(0/1)|opt1,opt2` per line) instead of a dynamic field-row editor, to avoid
  hand-rolling a complex list-of-Bonsai-components UI without a build/test loop.
- Weekly/one-time queue scheduling (`opens_at`, `weekly_schedule`) is implemented in the backend
  store exactly as in the Rust version, but there's no dedicated scheduler UI in the frontend yet.

## Building (once you have opam)

```bash
opam switch create . 5.1.1   # or reuse an existing >=4.14 switch
opam install dune dream yojson bonsai js_of_ocaml js_of_ocaml-ppx core
dune build
```

Run the backend:

```bash
dune exec lue_server/main.exe
# or: DATA_PATH=data/store.json SERVER_ADDR=127.0.0.1:3000 dune exec lue_server/main.exe
```

Build the frontend to static JS, then either serve it with any static file server or let the
backend serve it (it already falls back to `lue_web/dist` for unmatched GET routes):

```bash
dune build lue_web/main.bc.js
mkdir -p lue_web/dist
cp _build/default/lue_web/main.bc.js lue_web/dist/main.bc.js
cp lue_web/index.html lue_web/style.css lue_web/dist/
```

Then open `http://127.0.0.1:3000/`.

## Protocol

See `lue_shared/protocol.ml` for the full `client_message` / `server_message` variant types —
they cover the same set of actions as the original's `shared::{ClientMessage, ServerMessage}`
(setup, admin/user login, subscribe-admin, create/update queue, accounts, groups, site settings,
sharing, close queue, claim/unclaim/resolve/deny/reopen entry, subscribe-queue, join/leave queue),
minus the Microsoft OAuth messages.

## Persistence format

`lue_server/store.ml`'s `save_to_disk`/`load_from_disk` write a JSON snapshot (site settings,
accounts incl. password hashes, queues, archived queues, groups, admin/user sessions) to
`DATA_PATH` (default `data/store.json`), mirroring `crates/server/src/persistence.rs`, including
pruning sessions for deleted accounts and re-hashing any legacy plaintext passwords on load.
