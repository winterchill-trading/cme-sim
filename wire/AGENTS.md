# AGENTS.md: cme-sim/wire

The `cme-sim.wire` library (module `Cme_sim_wire`): SBE codecs, no I/O.

| Module | Holds |
| --- | --- |
| `Sbe` | SOFH and SBE header, little-endian fixed fields, 64-bit helpers |
| `Ilink3` | iLink 3 templates: Negotiate, Establish, orders, execution reports, retransmission |
| `Mdp3` | MDP 3.0 templates: incremental refresh, snapshot, security definitions, group headers |
| `Mbp_book` | Client-side market-by-price book rebuilt from MDP 3.0 |

Build and test: `opam exec --switch 5.2.0+ox -- dune build --root .. @cme-sim/all`.

Rules:
- Every in-scope template has a byte-exact test. Change the template and its fixture in one
  commit.
- Group headers: `numInGroup` is at offset 7 in MDP 3.0. Read the template before adding
  a repeating group.
- Keep the module free of `Async`.
