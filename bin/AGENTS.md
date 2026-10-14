# AGENTS.md: cme-sim/bin

The `cme-sim` server executable (`main.ml`). Flags, ports and clock modes are in
`../README.md`: the iLink 3 gateway, the MDP 3.0 feed, the HTTP port with the JSON
websocket, and the clock and anchor options.

Run: `opam exec -- dune exec cme-sim`. Build from the workspace with
`opam exec --switch 5.2.0+ox -- dune build --root .. @cme-sim/all`.

Rules:
- Keep the executable thin: parsing flags and wiring `Server`; logic belongs in `../lib`.
- A new flag gets a line in `../README.md` in the same commit.
