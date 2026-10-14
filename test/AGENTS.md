# AGENTS.md: cme-sim/test

Expect tests and property tests: byte-exact SBE fixtures, iLink 3 session and order flows,
MDP 3.0 book rebuilds, the anchoring phases, the session clock and the venue. Run:
`opam exec --switch 5.2.0+ox -- dune build --root .. @cme-sim/runtest`.

Rules:
- `venue_helpers.ml` is the shared setup; use it rather than copying venue construction.
- Promote expect output only after reading the diff, and only after a successful
  `@runtest` build.
- Clock tests use the manual or accelerated clocks, never wall time.
- Under `open! Async`, expect tests return `unit Deferred.t`; keep helpers that need
  `Async` in a submodule.
