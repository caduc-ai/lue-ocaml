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

## Status: it builds and runs

This has now actually been compiled and exercised end-to-end (setup -> admin login -> create
queue -> guest join -> claim -> resolve, verified over raw WebSocket frames with correct state
broadcast to every subscribed connection at each step), on `ocaml-base-compiler.5.2.0` /
`dune.3.24.2` / `dream.1.0.0~alpha8` / `bonsai.v0.16.0` / `js_of_ocaml.5.9.1`. The sandbox this
was built in has no root access, so system libraries (`m4`, `pkgconf`, `libgmp`, `libev`,
`libffi`, `zlib`) were extracted from `.deb` packages into `~/local` rather than installed via
`apt`; see the opam install commands below for the package list, and adjust for a normal
root-capable machine (just `apt install m4 pkg-config libgmp-dev libev-dev libffi-dev
zlib1g-dev` first).

**Known upstream issue:** writing to a WebSocket whose peer has already disconnected can make
dream's TCP layer (`gluten-lwt`) spin at ~100% CPU retrying a `writev()` that keeps returning
`EPIPE` instead of raising, wedging the whole single-threaded server
(<https://github.com/camlworks/dream/issues/411>, still open/unreleased as of writing). This
isn't fixable from application code. `scripts/supervise.sh` works around it by restarting the
server whenever `/health` stops responding (recovers within ~5s in testing); run the server
through that script rather than the bare binary.

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

## Building

```bash
# System packages (Debian/Ubuntu names shown; root machine):
sudo apt install m4 pkg-config libgmp-dev libev-dev libffi-dev zlib1g-dev

opam switch create . 5.2.0   # or reuse an existing >=4.14 switch
opam install dune yojson dream js_of_ocaml-compiler js_of_ocaml js_of_ocaml-ppx bonsai
dune build
```

Build the frontend to static JS and assemble `lue_web/dist` (the backend falls back to serving
that directory for `/` and any other unmatched GET route):

```bash
dune build lue_web/main.bc.js
mkdir -p lue_web/dist
cp _build/default/lue_web/main.bc.js lue_web/dist/main.bc.js
cp lue_web/index.html lue_web/style.css lue_web/dist/
```

Run the backend through the supervisor (recommended, see the known upstream issue above) or
directly:

```bash
DATA_PATH=data/store.json SERVER_ADDR=0.0.0.0:3000 scripts/supervise.sh
# or, without the auto-restart safety net:
DATA_PATH=data/store.json SERVER_ADDR=0.0.0.0:3000 dune exec lue_server/main.exe
```

Then open `http://127.0.0.1:3000/` (or whatever host/port you bound `SERVER_ADDR` to).

First visit shows the initial-setup form (create the first super admin), then the normal
sign-in flow.

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
