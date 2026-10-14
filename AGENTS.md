# AGENTS.md: cme-sim

## Purpose

A local simulator of a CME Globex futures venue. It speaks iLink 3 (FIXP over SBE) order
entry and MDP 3.0 market data (SBE over TCP) on top of the venue-neutral
`exchange-core-engine`. Prices are plausible, not real: nothing here comes from CME.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | The venue: `Products`, `Venue`, the FIXP session handler, `Feed` (MDP publisher), JSON feed, session clock, `Anchoring`, `Server` (see `lib/AGENTS.md`) |
| `wire/` | SBE and SOFH codecs, iLink 3 and MDP 3.0 templates, the client-side MBP book (see `wire/AGENTS.md`) |
| `bin/` | The `cme-sim` server (see `bin/AGENTS.md`) |
| `cli/` | The `cme-sim-cli` client for manual sessions |
| `test/` | Expect tests and Quickcheck properties (see `test/AGENTS.md`) |
| `scripts/` | `e2e.sh` and `anchor-demo.sh` (see `scripts/AGENTS.md`) |
| `.github/` | CI workflow and the shared obj-magic check (see `.github/AGENTS.md`) |
| `README.md` | Ports, wire formats, fidelity table, session clock, limitations |
| `dune-project` | Package metadata; edit this, not the generated `cme-sim.opam` |

## Build, test

Run from this repository. The workspace root is one level up, so every command takes
`--root ..`.

```sh
opam exec --switch 5.2.0+ox -- dune build --root .. @cme-sim/all
opam exec --switch 5.2.0+ox -- dune build --root .. @cme-sim/runtest
```

Parallel work with another agent: prefix both with `DUNE_BUILD_DIR=_build-cme-sim`.

Run the server: `opam exec -- dune exec cme-sim`. The ports and clock modes are in
`README.md` (`always-open` is the default; `real` follows the Globex week; `accelerated`
cycles pre-open, open and closed).

Standalone (outside the workspace): the org opam repository is the target for published
packages (see `../specs/repos/opam-repository.md`). Until a package is there, pin the
siblings from the CI list below in order, then `opam install . --deps-only --with-test`
and `dune build`.

## Conventions

- Format only this repository: `dune build --root .. @cme-sim/fmt --auto-promote`. Never
  run `dune fmt` at the workspace root.
- Every module has an `.mli`. Consume siblings by public opam name.
- Byte-exact tests for every in-scope SBE template; a template change needs its test
  fixture updated in the same commit.
- Session semantics follow the "Simulator scope" section of
  `../equities-core/docs/cme-ilink3-mdp3.md`. Anything outside that scope is listed as
  not simulated in the README, not silently approximated.
- Comments for humans at non-obvious choices (offsets, timers, sequence rules).

## Dependencies and dependents

Sibling checkouts: `fixed-decimal`, `exchange-core` (`exchange-core-engine`), `order-book`,
`equities-core`, `parquet`, `market-data`, `price-anchor`. CI pins them in that order
(`.github/workflows/ci.yml`). Shared TradFi semantics live in `../equities-core`; the
sibling TradFi simulators are `../cboe-sim` and `../nyse-sim`.

## CI

`.github/workflows/ci.yml`: checkout, the shared `ox-setup` action (opam switch cache keyed
on the pins), `dune build`, `dune test`, then the reusable obj-magic workflow from
`../infra`. Pushes cancel the running job; push once per finished change.

To check a run: one `gh run list --limit 1`, then at most one `gh run watch <id>
--exit-status`. Never poll in a loop.

## Pitfalls

- MDP 3.0 group headers carry `numInGroup` at offset 7; check the template before reading
  a repeating group.
- Feed writers over TCP must set `raise_when_consumer_leaves` to false and watch
  `consumer_left`, or a disconnected client raises inside the server.
- 64-bit SBE fields go through `Int64` (`get_u64` and `set_u64` in `wire/sbe.ml`);
  `Int64.to_int_exn` raises on a value that does not fit an OCaml int.
- Anchored listings clamp to the daily price limit; the anchor must not move a price
  outside it.
- The iLink 3 schema reference is a public third-party copy and the MDP 3.0 templates
  come from CME's public FTP. Record the version used in a comment next to the codec.

## Where to read more

- `README.md`: fidelity table, session clock, price anchor, not-simulated list, end to end.
- `../specs/repos/cme-sim.md`: key modules and status.
- `../equities-core/docs/cme-ilink3-mdp3.md`: the scope the fidelity table is checked against.
- `../oxcaml-claude-skills/skills/oxcaml-performance/` for hot paths.
- `../oxcaml-claude-skills/AGENTS.md`: workspace-wide guide.
- `../oxcaml-claude-skills/GOTCHAS.md`: the TradFi simulators section.

## Never do

- Install opam packages that change the compiler or `core`, `base`, `async`, `ppx_jane`
  on the shared `5.2.0+ox` switch. Dry-run first: `opam install --switch 5.2.0+ox --dry-run <pkg>`.
- Publish, tag or release.
- Commit credentials or print secrets.
- Poll CI in loops.
