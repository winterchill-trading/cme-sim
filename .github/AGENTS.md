# AGENTS.md: cme-sim/.github

- `workflows/ci.yml`: checkout, the shared `ox-setup` action (opam switch cache keyed on
  the pins list in the workflow), `dune build`, `dune test`, then the reusable obj-magic
  workflow from `winterchill-trading/infra`.
- The pins list must match the sibling dependencies in `dune-project`. Add a pin in the
  same commit as the dependency, in dependency order.
- Concurrency is `cancel-in-progress: true`; push once per finished change.
- To check a run: one `gh run list --limit 1`, then at most one `gh run watch <id>
  --exit-status`. Never poll.
