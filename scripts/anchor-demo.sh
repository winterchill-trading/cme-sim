#!/usr/bin/env bash
# Price anchor demo: ESZ6 follows a reference series (6700 to 6900 in 10 s, then back to
# 6800), GCZ6 a fixed 4100 with a drift, CLZ6 stays a free random walk. Polls /anchors
# every second, halts ESZ6 for 3 s on the way (the anchor waits, then resumes after the
# reopening auction), then stops the server.
#
#   scripts/anchor-demo.sh                 # builds with dune first
#   CME_SIM=... scripts/anchor-demo.sh     # use a prebuilt binary
set -euo pipefail

cd "$(dirname "$0")/.."
BUILD_DIR=${DUNE_BUILD_DIR:-_build}
if [[ -z "${CME_SIM:-}" ]]; then
  dune build @install 2>&1 | grep -v '^Alert\|lossy\|^ *|\|^File \|^[0-9]* |\|^ *\^' || true
  CME_SIM=$BUILD_DIR/install/default/bin/cme-sim
fi

port_free() { ! lsof -iTCP:"$1" -sTCP:LISTEN >/dev/null 2>&1; }
BASE=${DEMO_BASE_PORT:-4350}
while ! (port_free "$BASE" && port_free $((BASE + 1)) && port_free $((BASE + 2))); do
  BASE=$((BASE + 10))
done
HTTP=$((BASE + 2))

WORK=$(mktemp -d -t cme-sim-anchor.XXXXXX)
SERIES=$WORK/es.txt
{
  echo "# time price: the first point is mapped to the server start"
  for i in $(seq 0 10); do
    printf '2026-10-12T13:30:%02dZ %d\n' "$i" $((6700 + 20 * i))
  done
  echo "2026-10-12T13:30:11Z 6800"
} >"$SERIES"

"$CME_SIM" -gateway-port "$BASE" -feed-port $((BASE + 1)) -http-port "$HTTP" \
  -noise -seed 7 -symbols CLZ6 \
  -anchor-series "$SERIES=ESZ6" -anchor-fixed 4100,50=GCZ6 -anchor-half-life-ms 1000 \
  >"$WORK/server.log" 2>&1 &
SERVER=$!
cleanup() {
  kill "$SERVER" 2>/dev/null || true
  wait "$SERVER" 2>/dev/null || true
  rm -rf "$WORK"
}
trap cleanup EXIT

for _ in $(seq 50); do
  port_free "$HTTP" || break
  sleep 0.1
done

show() {
  curl -s "localhost:$HTTP/anchors" | python3 -c '
import json, sys
for a in json.load(sys.stdin):
    print("  {symbol} {phase:<14} target {target:>8} mark {mark:>8} last {last:>8} "
          "pulls {pulls:>3} waits {waits:>3} restarts {restarts} clamped {clamped}".format(
              **{k: ("-" if v is None else v) for k, v in a.items()}))'
}

echo "== server"
cat "$WORK/server.log"
for t in $(seq 1 14); do
  sleep 1
  echo "== t=${t}s"
  show
  if [[ $t == 5 ]]; then
    echo "== halt ESZ6 for 3 s"
    curl -s "localhost:$HTTP/admin/halt?symbol=ESZ6&seconds=3"
    echo
  fi
done
echo "== stop"
