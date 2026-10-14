# AGENTS.md: cme-sim/scripts

- `e2e.sh`: starts the server and drives it with `cme-sim-cli` (login, orders, fills,
  feed read-back), then stops it.
- `anchor-demo.sh`: runs anchored products and prints the top of the MDP book.

Rules:
- Keep `set -euo pipefail` and clean up the started processes on exit.
- Do not pipe a script into `head`; SIGPIPE skips the cleanup.
