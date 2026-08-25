#!/bin/bash
# Supervises lue_server/main.exe and restarts it whenever it stops responding
# to /health. This exists specifically to work around a known upstream bug
# in dream's TCP layer (gluten-lwt does not check the writev() result when a
# WebSocket peer has gone away, so it can spin at ~100% CPU retrying a write
# that keeps returning EPIPE instead of raising -- see
# https://github.com/camlworks/dream/issues/411). A single stuck connection
# can otherwise wedge the whole single-threaded Lwt server. Until that fix
# lands upstream, auto-restarting on a failed health check is the practical
# mitigation: worst case is a ~1-2s interruption while it's detected and
# restarted, instead of a permanent hang requiring manual intervention.
#
# Usage: scripts/supervise.sh
# Env:   DATA_PATH, SERVER_ADDR, HEALTH_URL, CHECK_INTERVAL_SECS,
#        FAILS_BEFORE_RESTART (all optional, see defaults below).

# Pull in the locally-installed (non-root, extracted-from-.deb) runtime
# libraries this build links against (see README for why). Sourced before
# `set -u` since it unconditionally appends to possibly-unset PATH-like vars.
if [ -f "$HOME/.bashrc.local_toolchain" ]; then
  # shellcheck disable=SC1090
  source "$HOME/.bashrc.local_toolchain"
fi

set -u
cd "$(dirname "$0")/.."

BIN="_build/default/lue_server/main.exe"
SERVER_ADDR="${SERVER_ADDR:-0.0.0.0:3000}"
HEALTH_URL="${HEALTH_URL:-http://127.0.0.1:${SERVER_ADDR##*:}/health}"
DATA_PATH="${DATA_PATH:-$(pwd)/data/store.json}"
CHECK_INTERVAL_SECS="${CHECK_INTERVAL_SECS:-3}"
FAILS_BEFORE_RESTART="${FAILS_BEFORE_RESTART:-2}"

export DATA_PATH SERVER_ADDR

log() { echo "[supervise $(date '+%Y-%m-%dT%H:%M:%S%z')] $*"; }

start_server() {
  "$BIN" &
  SERVER_PID=$!
  log "started $BIN (pid $SERVER_PID) on $SERVER_ADDR, data at $DATA_PATH"
}

stop_server() {
  if kill -0 "$SERVER_PID" 2>/dev/null; then
    log "stopping stuck/old server (pid $SERVER_PID)"
    kill -9 "$SERVER_PID" 2>/dev/null
    wait "$SERVER_PID" 2>/dev/null
  fi
}

trap 'stop_server; exit 0' TERM INT

start_server
fails=0

while true; do
  sleep "$CHECK_INTERVAL_SECS"

  if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    log "server process exited unexpectedly, restarting"
    start_server
    fails=0
    continue
  fi

  if curl -s -o /dev/null -m 2 -w '' "$HEALTH_URL"; then
    fails=0
  else
    fails=$((fails + 1))
    log "health check failed ($fails/$FAILS_BEFORE_RESTART)"
    if [ "$fails" -ge "$FAILS_BEFORE_RESTART" ]; then
      log "restarting server after $fails consecutive failed health checks"
      stop_server
      start_server
      fails=0
    fi
  fi
done
