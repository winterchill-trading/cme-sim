# AGENTS.md: cme-sim/cli

`cme-sim-cli` (`main.ml`): a manual iLink 3 client for a running `cme-sim` server. It logs
in, sends orders and prints the execution reports.

Build: `opam exec --switch 5.2.0+ox -- dune build --root .. @cme-sim/all`.

Rules:
- Reuse `Cme_sim_wire` codecs; do not hand-encode a message in the CLI.
- It is a debugging tool, not a second gateway. Keep it small.
