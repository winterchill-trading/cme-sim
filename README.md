# cme-sim

A free, local stand-in for CME Globex that speaks the venue's real public binary
protocols over TCP: **iLink 3** order entry (SOFH + Simple Binary Encoding, FIXP session
layer) and **MDP 3.0** market data (SBE packets). Connect with the included CLI (or your
own SBE code), send orders, watch the feed.

There is no real-time real data behind it. CME market data is licensed and nothing is
replayed here: the book is driven by noise traders from
[exchange-core](https://github.com/winterchill-trading/exchange-core) (`-noise`), optionally
pulled towards a reference price by the price anchor (`-anchor-*`, see
[Price anchor](#price-anchor)). Prices, volumes and depth are synthetic.

Instruments (December 2026 contracts; ticks and multipliers from
[equities-core](https://github.com/winterchill-trading/equities-core)):

| Symbol | SecurityID | Product | Tick | Multiplier | Tick value | Start price | Daily limit | Market protection |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| ESZ6 | 1001 | E-mini S&P 500 | 0.25 | 50 | $12.50 | 6700 | 7% | 6.00 |
| NQZ6 | 1002 | E-mini Nasdaq-100 | 0.25 | 20 | $5.00 | 24500 | 7% | 25.00 |
| CLZ6 | 1003 | WTI crude oil | 0.01 | 1000 | $10.00 | 62.50 | 15% | 0.50 |
| GCZ6 | 1004 | Gold | 0.10 | 100 | $10.00 | 4000 | 10% | 5.00 |

Security ids, start prices, limits and protection ranges are the simulator's own.

## Play

```sh
dune build
dune exec cme-sim -- -noise                     # gateway 4300, feed 4301, http 4302
dune exec cme-sim-cli -- book -symbol ESZ6      # the book, rebuilt from the MDP feed
dune exec cme-sim-cli -- feed -seconds 5        # every decoded MDP message
dune exec cme-sim-cli -- order -symbol ESZ6 -side buy -qty 1 -type market -tif fak
dune exec cme-sim-cli -- session <<'EOF'
order -symbol ESZ6 -side buy -qty 2 -price 6650 -tif gtc
sleep 1
replace -id last -price 6650.25 -qty 3
cancel -id last
EOF
curl -s localhost:4302/                          # instrument definitions as JSON
curl -s 'localhost:4302/admin/halt?symbol=GCZ6&seconds=10'
```

Server flags: `-gateway-port`, `-feed-port`, `-http-port`, `-noise`,
`-symbols ESZ6,CLZ6` (what the noise traders trade), `-clock real|accelerated|always-open`,
`-seed`, `-no-cancel-on-disconnect`, `-log-level`, and the anchor flags
`-anchor-series`, `-anchor-fixed`, `-anchor-scaled`, `-anchor-norm`, `-anchor-from`,
`-anchor-to`, `-anchor-half-life-ms`.

CLI commands: `logon`, `order` (`-symbol -side -qty [-price] [-tif day|gtc|fak|fok]
[-type limit|market] [-display N] [-min-qty N] [-wait S]`), `cancel -id`, `session`
(commands from stdin over one connection: `order`, `replace`, `cancel -id N|last`,
`sleep`), `feed`, `book -symbol`. Globex cancels a session's orders when it disconnects
and so does the simulator, so one-shot `order` and `cancel` calls only make sense
together with `-no-cancel-on-disconnect`; `session` keeps the connection open.

`scripts/e2e.sh` starts the server on free high ports, runs all of the above and stops
everything.

## Ports and wire formats

| Port | Protocol | Framing |
| --- | --- | --- |
| gateway (4300) | iLink 3, schema id 8, version 5 | SOFH (`MessageSize u16`, `0xCAFE`) + SBE header + body |
| feed (4301) | MDP 3.0, schema id 1, version 13 | SOFH + packet (`MsgSeqNum u32`, `SendingTime u64`, one message) |
| http (4302) | `/ws` websocket: the same feed as JSON, one object per MDP message; `/` instrument list; `/anchors` anchor state; `/admin/halt?symbol=S&seconds=N` | HTTP/1.1 |

The MDP feed is the multicast packet format carried over one TCP stream; the SOFH in
front of each packet is the simulator's way to split the stream (CME's own TCP recovery
also frames with a SOFH). A new feed connection first receives the four instrument
definitions, the security status and a SnapshotFullRefresh per instrument, all in packets
numbered with the current `MsgSeqNum` (the snapshots' `LastMsgSeqNumProcessed`);
incrementals continue from the next number. A heartbeat is sent after 5 s without
traffic.

SBE codecs are hand-written in the `cme-sim.wire` sub-library (`Sbe`, `Ilink3`, `Mdp3`,
`Mbp_book`), which depends only on `core` and `exchange-core` so a client can reuse it.
MDP template ids, block lengths and field offsets were checked against CME's public
`templates_FixBinary.xml` (version 13, 2023-04-11); iLink 3 layouts against a copy of
`ilinkbinary.xml` version 5 (the current schema needs CME SFTP credentials). Decoders use
the header's `BlockLength` to skip appended fields, so newer schema versions still decode.

## Fidelity

Against the "Simulator scope" of
[cme-ilink3-mdp3.md](https://github.com/winterchill-trading/equities-core/blob/main/docs/cme-ilink3-mdp3.md):

| Scope item | Status | Notes |
| --- | --- | --- |
| SOFH + SBE header, little endian | done | byte-exact tests for every in-scope template |
| Negotiate / NegotiationResponse | done | HMAC and credentials accepted unchecked; empty session or firm is rejected |
| Establish / EstablishmentAck | done | must follow Negotiate within 60 s; keep-alive clamped to 1 to 60 s |
| Sequence heartbeats, keep-alive lapse | done | exchange heartbeats when silent; client silent one interval gets `KeepAliveIntervalLapsed`, two intervals terminate |
| Terminate | done | both directions; protocol errors terminate with a reason |
| NotApplied on inbound gaps | done | gap reported, message applied; a lower `SeqNum` terminates |
| RetransmitRequest / Retransmission | done | up to 2500 messages, `PossRetransFlag` set; RetransmitReject otherwise |
| UUID reuse continues sequences | done | per firm/session; reports produced while disconnected are stored for replay |
| Cancel on disconnect | done | `ExecRestatementReason` 100; `-no-cancel-on-disconnect` to turn off |
| NewOrderSingle limit | done | tick, daily price limits, quantity 1 to 2000 |
| Market with protection | done | limit at best opposite ± protection, inside the limits; remainder rests |
| Day / GTC / FAK / FOK | done | day orders expire at the close with ExecutionReportElimination; FOK and MinQty check available quantity first |
| DisplayQty (iceberg) | done | each refreshed slice joins the back of the queue |
| MinQty | done | FAK only, as on Globex |
| OrderCancelRequest / OrderCancelReplaceRequest | done | OrderID stays the same across replaces |
| ExecutionReportNew / Reject / TradeOutright / Cancel / Modify / Elimination | done | TradeOutright carries `MDTradeEntryID` of the matching TradeSummary entry |
| OrderCancelReject / OrderCancelReplaceReject | done | unknown order, too late, invalid price |
| BusinessReject | codec only | the gateway answers malformed sessions with Terminate instead |
| MDP ChannelReset, AdminHeartbeat | codec; heartbeat sent | ChannelReset is never needed: there is no loss on TCP |
| MDInstrumentDefinitionFuture | done | events (activation, last trade), GBX depth 10, lot rule, limits |
| SecurityStatus: pre-open, open, halt, close, maintenance | done | 21 / 17 / 2 / 4; maintenance is published as close |
| MDIncrementalRefreshBook, MBP 10 levels | done | diff of the engine book; `RptSeq` per instrument |
| TradeSummary | done | one entry per aggressor and price, with order id entries; auction trades have no aggressor |
| SnapshotFullRefresh on connect | done | bids, offers, last trade, settlement |
| DailyStatistics settlement | done | at each close; the settlement resets the price limits |

Beyond the scope list: the opening (and halt reopening) is a call auction over the
pre-open orders and the resting book, using equities-core `Auction` (maximum volume,
minimum imbalance, closest to the last settlement); a trade at a daily limit halts the
product for 30 s, followed by a 5 s pre-open.

Reject codes are FIX values: `OrdRejReason` 1 unknown security, 2 market closed or
halted, 3 outside the price limits, 11 unsupported order characteristic, 13 bad quantity,
18 off tick, 99 other; `CxlRejReason` 0 too late, 1 unknown order. CME's own numeric
error codes and texts are not reproduced.

## Session clock

- `always-open` (default): continuous trading.
- `real`: the Globex week from equities-core `Session.Cme_globex` (Sunday 17:00 to Friday
  16:00 Chicago time, daily 16:00 to 17:00 maintenance), with the last 15 minutes before
  each open as pre-open. Holidays are not modelled.
- `accelerated`: 20 s pre-open, 120 s open, 20 s closed, repeated from the server start.

At each close the noise traders' orders are pulled, day orders expire and a settlement is
published; customer GTC orders stay and take part in the next opening auction.

## Price anchor

There is no free real-time CME data, so nothing is shadowed or replayed: the noise
traders stay the book's only liquidity, and the
[price anchor](https://github.com/winterchill-trading/price-anchor) design's TradFi path
makes their random walk mean-revert around a reference price
(`Market_sim.set_reference ~half_life`; the gap halves every half-life, 1 s by default).

```sh
dune exec cme-sim -- -anchor-series es.txt=ESZ6                  # <time> <price> lines
dune exec cme-sim -- -anchor-fixed 4100,50=GCZ6                  # 4100, +50 bps a day
dune exec cme-sim -- -anchor-scaled binance:BTCUSDT=GCZ6,0.04 \
  -anchor-norm data/norm -anchor-from 2026-10-09                 # a recorded instrument
curl -s localhost:4302/anchors                                   # target, mark, stats
```

| Flag | Reference |
| --- | --- |
| `-anchor-series FILE=SYMBOL` | a file of `<time> <price>` lines (nanoseconds or ISO 8601 with a zone, `#` comments), read with price-anchor's `Source.of_series`; the first point is mapped to the server start and the series is followed in real time, holding the last point |
| `-anchor-fixed PRICE[,DRIFT_BPS_PER_DAY]=SYMBOL` | a fixed price, compounded by the drift |
| `-anchor-scaled VENUE:SYMBOL=SIMSYM,FACTOR` | trades, BBO mids and L2 book mids of a recorded instrument from normalized event logs (`-anchor-norm DIR -anchor-from DAY [-anchor-to DAY]`), times `FACTOR` |
| `-anchor-half-life-ms MS` | mean reversion half-life (default 1000) |

Each flag is repeatable, one product per flag; anchored products always get noise
traders, with or without `-noise`. The reference is rounded to the product's tick.

- Open: before each noise tick (100 ms) the walk is pulled towards the reference; the
  mark follows on the tick.
- Pre-open, limit halts, maintenance: the venue owns the price; the anchor neither pulls
  nor ticks and counts `waits`. At the open the walk restarts from the venue's last price
  (the opening auction's, if it traded) and is pulled back from there (`restarts`).
- Daily price limits keep coming from the settlement, i.e. the simulator's own last trade,
  which now follows the reference.
- Shadow levels, trade replay and staleness do not apply: there is no depth or trade flow
  behind the reference.

`scripts/anchor-demo.sh` runs ESZ6 on a 6700 to 6900 series, GCZ6 on a fixed 4100 and
halts ESZ6 on the way (excerpt):

```
== t=5s
  ESZ6 Open           target     6800 mark  6766.25 last  6735.75 pulls  51 waits   0 restarts 0
  GCZ6 Open           target     4100 mark   4097.6 last   4095.1 pulls  51 waits   0 restarts 0
== halt ESZ6 for 3 s
== t=10s
  ESZ6 Pre_open       target     6900 mark  6766.25 last  6735.75 pulls  51 waits  50 restarts 0
  GCZ6 Open           target     4100 mark     4100 last   4099.4 pulls 101 waits   0 restarts 0
== t=14s
  ESZ6 Open           target     6800 mark   6772.5 last   6755.5 pulls  63 waits  77 restarts 1
  GCZ6 Open           target     4100 mark   4096.5 last   4098.4 pulls 140 waits   0 restarts 0
```

Not done: reading a recorded crypto series from raw minute files (`Raw_reader` plus a
venue normalizer) would link a venue client and its network stack into the simulator, so
`-anchor-scaled` reads normalized event logs instead (`market-recorder` writes both, and
`rebuild-norm` turns raw files into event logs).

## Not simulated

- Real prices, volumes or any CME data; the start prices are fixed unless anchored.
- Spreads and implied prices, options, mass quotes, party details, RFQ, order status and
  mass action requests, stop orders, good till date.
- MBOFD/MBOLD order-level books, the recovery and instrument multicast channels, the MDP
  TCP replay session messages, fault tolerance (primary/backup), BrokerTec and EBS.
- HMAC authentication, credit controls, self-match prevention IDs (the engine's own
  self-trade check applies to orders of the same session), velocity logic and dynamic
  circuit breakers beyond the daily limit halt.
- Replaces always lose queue priority (Globex keeps it when only the quantity goes down).
- Auction trades are reported to both sides and on the feed, but the engine's clearing
  (positions, margin) only sees continuous trades. Margin is not enforced in practice:
  every session gets a large deposit.
- Exchange holidays, trade date rolls and contract expiry.

## End-to-end run

Excerpt of `scripts/e2e.sh` (seed 7; prices vary with timing):

```
== feed: definitions, status and snapshots, then incrementals
    98 Definition ESZ6 id=1001 tick=0.25 multiplier=50 limits=6231..7169
    98 Definition NQZ6 id=1002 tick=0.25 multiplier=20 limits=22785..26215
    98 Definition CLZ6 id=1003 tick=0.01 multiplier=1000 limits=53.13..71.87
    98 Definition GCZ6 id=1004 tick=0.1 multiplier=100 limits=3600..4400
    98 SecurityStatus ESZ6 Ready_to_trade
    ...
    98 Snapshot ESZ6 rpt=94 last_seq=98 0L1:6696.25x4 0L2:6696x1 0L3:6694.5x3 ...
    99 Trade NQZ6 1 @ 24487.5 aggressor=sell id=22
   101 Trade GCZ6 2 @ 4000 aggressor=buy id=24
   102 Book ESZ6 bid delete L7 6692.25, ESZ6 bid new L10 6686x1, ESZ6 ask delete L10 6708.5, ...

== ESZ6 book rebuilt from the feed
ESZ6 book (10 levels each side), last trade 2 @ 6694.75
   bid qty          bid | ask          ask qty
     2 (1)      6700.25 | 6702.5       2 (1)
     4 (1)      6698.25 | 6702.75      3 (1)
     3 (1)      6692.75 | 6703.25      2 (1)
     ...

== one session: rest, modify, cancel, iceberg, market with protection, rejects
< EstablishmentAck uuid=1791814258377617 next_seq=1 keep_alive=30000ms
> order -symbol ESZ6 -side buy -qty 2 -price 6600 -clordid rest-1
< ExecutionReportNew #1 order_id=700000000001 clordid=rest-1 buy ESZ6 2 @ 6600 day
> replace -id last -price 6600.25 -qty 3
< ExecutionReportModify #2 order_id=700000000001 clordid=S-3 3 @ 6600.25 leaves=3
> cancel -id last
< ExecutionReportCancel #3 order_id=700000000001 clordid=S-5 cum=0
> order -symbol CLZ6 -side sell -qty 10 -price 70 -display 2 -tif gtc -clordid iceberg
< ExecutionReportNew #4 order_id=700000000002 clordid=iceberg sell CLZ6 10 @ 70 gtc
> order -symbol ESZ6 -side buy -qty 2 -type market -tif fak -clordid mkt-fak
< ExecutionReportNew #5 order_id=700000000003 clordid=mkt-fak buy ESZ6 2 @ 6706.75 fak
< ExecutionReportTradeOutright #6 order_id=700000000003 clordid=mkt-fak buy ESZ6 1 @ 6700.75 cum=1 leaves=1 aggressor trade_id=52
< ExecutionReportTradeOutright #7 order_id=700000000003 clordid=mkt-fak buy ESZ6 1 @ 6701.25 cum=2 leaves=0 aggressor trade_id=53
> order -symbol NQZ6 -side sell -qty 1 -price 24400 -tif fok -clordid fok
< ExecutionReportNew #8 order_id=700000000004 clordid=fok sell NQZ6 1 @ 24400 fok
< ExecutionReportTradeOutright #9 order_id=700000000004 clordid=fok sell NQZ6 1 @ 24471.5 cum=1 leaves=0 aggressor trade_id=56
< ExecutionReportReject #10 clordid=off-tick reason=18 "price 6600.1 is not a multiple of the tick 0.25"
< ExecutionReportReject #11 clordid=over-limit reason=3 "price 9000 outside the price limits 6231 to 7169"
< ExecutionReportReject #12 clordid=gtd reason=11 "good till date is not supported"

== one-shot order: FAK sell that crosses the GCZ6 bid
< ExecutionReportNew #1 order_id=700000000005 clordid=CLI-1 sell GCZ6 3 @ 3900 fak
< ExecutionReportTradeOutright #2 order_id=700000000005 clordid=CLI-1 sell GCZ6 2 @ 3998.4 cum=2 leaves=1 aggressor trade_id=76
< ExecutionReportTradeOutright #3 order_id=700000000005 clordid=CLI-1 sell GCZ6 1 @ 3997.6 cum=3 leaves=0 aggressor trade_id=77

== JSON: instrument list and the /ws websocket
{"seq":332,"time":1791814261738670000,"type":"book","transact_time":1791814261738670000,"entries":[{"symbol":"NQZ6","side":"bid","level":8,"action":"delete","price":"24462.75",...

== halt GCZ6 for 2 s: status goes halt, pre-open, open (reopening auction)
{"halted":"GCZ6","seconds":2.0}
   404 SecurityStatus GCZ6 Trading_halt
   493 SecurityStatus GCZ6 Pre_open
   693 SecurityStatus GCZ6 Ready_to_trade

== stop
server stopped, ports 4300-4302 free
```

## Layout

- `wire/`: `cme-sim.wire`, the SBE codecs and the client-side MBP book.
- `lib/`: `Products`, `Venue` (CME order semantics over `exchange-core-engine`),
  `Session_handler` (FIXP state machine), `Feed` (MDP publisher), `Json_feed`,
  `Session_clock`, `Anchoring` (the price anchor), `Server`, `Ilink_client`, `Frame_io`.
- `bin/`: the `cme-sim` server; `cli/`: `cme-sim-cli`; `scripts/e2e.sh`,
  `scripts/anchor-demo.sh`.

CI pins fixed-decimal, exchange-core, order-book, exchange-core-engine, equities-core,
parquet, market-data and price-anchor from GitHub.

CI installs the pinned siblings and the opam dependencies through the shared
[ox-setup](https://github.com/winterchill-trading/infra/blob/main/docs/ci.md) action, which
caches the built opam switch between runs.
