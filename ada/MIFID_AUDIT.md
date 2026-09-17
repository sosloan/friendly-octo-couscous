# MiFID II Best Execution and MiFIR RTS 25 Evidence

## Scope and status

This Ada implementation is limited to best-execution evidence and clock
synchronization evidence for NYSE cash-equity and CME futures workflows. It
does not claim affiliation with, endorsement by, or use of proprietary
technology from Jump Trading, IMC Trading, Fisher Investments, NYSE, or CME
Group.

The software validates evidence completeness against configured controls. It
is not legal advice, regulatory certification, an exchange adapter, or proof
that a deployment complies with MiFID II or MiFIR. Applicability, system
classification, thresholds, retention, and operating procedures require
approval by the regulated firm's compliance and legal functions.

## Implemented controls

### Best execution

`HFT_MiFID.Execution_Evidence` separates the investment signal from execution
and records:

- parent, child, and correlation identifiers;
- lifecycle stage from market-data receipt through correction;
- instrument class, instrument, related hedge, venue, session, order type,
  futures expiry, and roll decision;
- client mandate, strategy constraints, hedge objective, routing rationale,
  overrides, policy version, and build identifier;
- bid/ask, depth, fees, liquidity, latency, fill probability, feed sequence,
  exchange timestamp, receipt timestamp, and stale-data state;
- arrival, execution, and benchmark prices; fill; fees; implementation
  shortfall; spread capture; slippage; market impact; opportunity cost; and
  execution latency;
- exchange, drop-copy, and clearing reconciliation identifiers.

`Economically_Equivalent` requires the same asset class, instrument, currency,
and quantity. A cash equity and a futures hedge therefore cannot be presented
as interchangeable execution alternatives.

`Venue_Policy` provides approved-venue flags, a version, and factor weights for
price, cost, speed, fill likelihood, and size/nature. The default policy allows
NYSE cash equities and CME futures. Production policy must be loaded from an
approved, version-controlled configuration process.

### Clock synchronization

`HFT_Engine.UTC_Timestamp_NS` represents UTC nanoseconds since the Unix epoch.
`HFT_Engine.Monotonic_Timestamp_NS` is a distinct elapsed-time type.
`HFT_Time_Util` exposes both clocks without converting through floating point.

The configured RTS 25 tiers are:

| Tier | Maximum UTC divergence | Maximum recorded granularity |
|---|---:|---:|
| High-frequency electronic | 100 µs | 1 µs |
| Standard electronic | 1 ms | 1 ms |
| Non-electronic | 1 s | 1 s |

`Clock_Evidence` records tier, synchronization state, PTP/NTP/holdover source,
hardware/kernel/application origin, UTC offset, uncertainty, granularity, and
last synchronization time. Compliance requires `abs(offset) + uncertainty` to
remain within the tier threshold. A deployment must classify each system from
its actual activity and gateway-to-gateway latency before relying on a tier.

Explicit events exist for loss of synchronization, excessive drift, rollback,
source failover, stale market data, and reconciliation failure. The application
must connect these events to monitoring, escalation, and controlled-trading
procedures.

## Audit integrity and replay

`HFT_Audit` serializes each complete evidence record to an append-only text log.
Every record includes its predecessor and a SHA-256 digest of the predecessor
plus canonical record. Event identifiers and the chain head are recovered when
the process restarts. In-memory operations are serialized by a protected
object, and capacity or persistence failures raise explicit exceptions rather
than silently dropping records.

This supplies cryptographic change detection, not immutable storage. Production
deployments must place the log on access-controlled WORM or equivalent storage,
protect independent signed checkpoints, manage retention and legal holds, and
monitor persistence failures. `Clear_Audit_History` is intended only for tests
or separately authorized maintenance.

The canonical record retains the inputs required for deterministic decision
replay. Exchange acknowledgements, drop copies, clearing records, and native
market-data archives remain external evidence and must be retained and
reconciled by their adapters.

## NYSE/CME evidence matrix

| Workflow | Required evidence |
|---|---|
| NYSE continuous | Native quote time, local receipt, sequence, session, order type, routing rationale, acknowledgements and fills |
| NYSE auction | Auction session, imbalance/reference inputs, constraints, decision time, auction result |
| CME outright | Contract and expiry, Globex session, market depth, fees, acknowledgements, drop copy, clearing ID |
| CME spread/roll | Legs or related instrument, expiry, spread order type, roll rationale, fill and clearing reconciliation |
| Equity plus futures hedge | Separate execution assessments linked by correlation and hedge objective |
| Feed or venue outage | Stale/outage event, affected orders, fallback decision, approval, and reconciliation |

Feed handlers must document normalization, sequence-gap recovery, gateway and
colocation boundaries, timestamp origin, and every timestamp transformation.

## Audit deliverables

Before an external review, supply:

1. approved best-execution policy, factor hierarchy, venue/instrument matrix,
   and change approvals;
2. RTS 25 classification for each host and gateway, clock topology, UTC source,
   PTP/NTP configuration, monitoring, and failover evidence;
3. sampled NYSE and CME lifecycle records with replay and transaction-cost
   analysis;
4. exchange acknowledgement, drop-copy, fill, and clearing reconciliation;
5. drift, rollback, source-loss, stale-feed, venue-outage, partial-fill,
   auction, spread, and roll test evidence;
6. immutable log storage, independent checkpoints, access reviews, retention,
   incident response, and unresolved-risk records.

## Build and test

```bash
cd ada
gprbuild -P hft.gpr
./obj/hft_mifid_test
```

The focused suite covers SHA-256 integrity, RTS 25 boundaries, clock loss,
clock rollback, lifecycle stages, stale quotes, reconciliation failures,
NYSE/CME evidence, cash/futures non-equivalence, and chain recovery.

## Known deployment boundaries

- Clock quality is supplied by infrastructure; this repository does not
  configure PTP hardware or independently measure UTC.
- NYSE/CME feeds, gateways, drop copies, and clearing connections are not
  implemented here.
- Policy and market inputs are trusted caller data until authenticated adapter
  boundaries are added.
- The local file is not WORM storage and has no key-backed signature.
- Formal proof and performance claims require retained tool output from the
  exact reviewed build.
