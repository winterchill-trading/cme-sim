#!/usr/bin/env bash
# End-to-end run: start cme-sim with noise traders on free high ports, log on, read the
# MDP feed, place, modify, cancel and fill orders over iLink 3, look at the JSON side,
# halt a product, then stop everything.
#
#   scripts/e2e.sh                 # builds with dune first
#   CME_SIM=... CME_SIM_CLI=... scripts/e2e.sh   # use prebuilt binaries
set -euo pipefail

cd "$(dirname "$0")/.."
BUILD_DIR=${DUNE_BUILD_DIR:-_build}
if [[ -z "${CME_SIM:-}" || -z "${CME_SIM_CLI:-}" ]]; then
  dune build @install 2>&1 | grep -v '^Alert\|lossy\|^ *|\|^File \|^[0-9]* |\|^ *\^' || true
  CME_SIM=${CME_SIM:-$BUILD_DIR/install/default/bin/cme-sim}
  CME_SIM_CLI=${CME_SIM_CLI:-$BUILD_DIR/install/default/bin/cme-sim-cli}
fi

port_free() { ! lsof -iTCP:"$1" -sTCP:LISTEN >/dev/null 2>&1; }
BASE=${E2E_BASE_PORT:-4300}
while ! (port_free "$BASE" && port_free $((BASE + 1)) && port_free $((BASE + 2))); do
  BASE=$((BASE + 10))
done
GW=$BASE FEED=$((BASE + 1)) HTTP=$((BASE + 2))

LOG=$(mktemp -t cme-sim-e2e.XXXXXX)
"$CME_SIM" -gateway-port "$GW" -feed-port "$FEED" -http-port "$HTTP" -noise -seed 7 \
  >"$LOG" 2>&1 &
SERVER=$!
cleanup() {
  kill "$SERVER" 2>/dev/null || true
  wait "$SERVER" 2>/dev/null || true
  rm -f "$LOG"
}
trap cleanup EXIT

for _ in $(seq 50); do
  port_free "$GW" || break
  sleep 0.1
done
sleep 1 # let the noise traders build a book

step() { printf '\n== %s\n' "$*"; }

step "server"
cat "$LOG"

step "logon (Negotiate, Establish, Terminate)"
"$CME_SIM_CLI" logon -port "$GW" -firm 001 -session E2E

step "feed: definitions, status and snapshots, then incrementals"
"$CME_SIM_CLI" feed -port "$FEED" -count 16 | cut -c1-160

step "ESZ6 book rebuilt from the feed"
"$CME_SIM_CLI" book -port "$FEED" -symbol ESZ6 -seconds 1

step "one session: rest, modify, cancel, iceberg, market with protection, rejects"
"$CME_SIM_CLI" session -port "$GW" -firm 001 -session E2E <<'SCRIPT'
order -symbol ESZ6 -side buy -qty 2 -price 6600 -clordid rest-1
sleep 0.3
replace -id last -price 6600.25 -qty 3
sleep 0.3
cancel -id last
sleep 0.3
order -symbol CLZ6 -side sell -qty 10 -price 70 -display 2 -tif gtc -clordid iceberg
sleep 0.3
order -symbol ESZ6 -side buy -qty 2 -type market -tif fak -clordid mkt-fak
sleep 0.3
order -symbol NQZ6 -side sell -qty 1 -price 24400 -tif fok -clordid fok
sleep 0.3
order -symbol ESZ6 -side buy -qty 1 -price 6600.10 -clordid off-tick
order -symbol ESZ6 -side buy -qty 1 -price 9000 -clordid over-limit
order -symbol GCZ6 -side buy -qty 1 -price 4000 -tif gtd -clordid gtd
sleep 0.5
SCRIPT

step "one-shot order: FAK sell that crosses the GCZ6 bid"
"$CME_SIM_CLI" order -port "$GW" -firm 002 -session ONE -symbol GCZ6 -side sell -qty 3 \
  -price 3900 -tif fak -wait 0.5

step "JSON: instrument list and the /ws websocket"
curl -s "http://127.0.0.1:$HTTP/" | cut -c1-300
echo
curl -s -N --max-time 1 --http1.1 -H "Connection: Upgrade" -H "Upgrade: websocket" \
  -H "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==" -H "Sec-WebSocket-Version: 13" \
  "http://127.0.0.1:$HTTP/ws" | strings | grep -m 3 '"type":"\(trade\|book\)"' | cut -c1-220 || true

step "halt GCZ6 for 2 s: status goes halt, pre-open, open (reopening auction)"
FEEDLOG=$(mktemp -t cme-sim-feed.XXXXXX)
"$CME_SIM_CLI" feed -port "$FEED" -seconds 9 >"$FEEDLOG" &
FEEDER=$!
sleep 0.5
curl -s "http://127.0.0.1:$HTTP/admin/halt?symbol=GCZ6&seconds=2"
echo
wait "$FEEDER"
grep 'SecurityStatus GCZ6' "$FEEDLOG" | tail -n 3
rm -f "$FEEDLOG"

step "server log"
cat "$LOG"

step "stop"
kill "$SERVER"
wait "$SERVER" 2>/dev/null || true
if port_free "$GW" && port_free "$FEED" && port_free "$HTTP"; then
  echo "server stopped, ports $GW-$HTTP free"
else
  echo "ports still in use" >&2
  exit 1
fi
