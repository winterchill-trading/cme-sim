# AGENTS.md: cme-sim/lib

The `cme-sim` library (module `Cme_sim`): the venue on top of `exchange-core-engine`.

| Module | Holds |
| --- | --- |
| `Products` | Futures contracts: tick sizes, multipliers, daily price limits, expiries |
| `Venue` | CME order semantics over the engine: limit, market with protection, Day, GTC, FAK, FOK, iceberg |
| `Session_handler` | The FIXP state machine: negotiate, establish, sequences, heartbeats, retransmission |
| `Feed` | MDP 3.0 publisher, incremental and snapshot messages, settlement |
| `Json_feed` | The same events as JSON for the HTTP websocket |
| `Session_clock` | `always-open`, `real` (Globex week) and `accelerated` clocks |
| `Anchoring` | Wrapper over `Price_anchor.Anchor` for each anchored product |
| `Ilink_client`, `Frame_io` | Client-side iLink 3 session and length-prefixed frame reader and writer |
| `Server` | TCP gateway, MDP feed and HTTP port |

Build and test: `opam exec --switch 5.2.0+ox -- dune build --root .. @cme-sim/all`.

Rules:
- Session state changes go through `Session_handler`; `Server` only moves bytes.
- Retransmission keeps the reports produced while a client is disconnected; do not drop
  them on disconnect (cancel-on-disconnect is a separate, documented behaviour).
- Daily price limits are enforced by the venue and by the anchor clamp; a new product
  needs both.
- Name new modules so they do not collide with `Equities_core.Session`; `Session_clock`
  is the existing name for the clock.
