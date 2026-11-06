# Stage 0: deployed business capability and data baseline

Inspection: 2026-11-06, 20:33 UTC. Backend contract reference:
`fc4f559dfb187a368bf2a8cd4053d8e833348835`.
Context: existing Acme Corporation demo tenant, Acme Corporation Plc party
`b29db53d-325b-407f-9c47-8d229e4a111b`, signed in as the tenant administrator.
This proves that account's reads, not least-privilege or cross-tenant policy.

## Reproduce

From `projects/ores.agent`, with the existing backend running:

```text
npm run build
node scripts/inspect-business.mjs <agent-config.json> <private-demo-credentials.json> <party-id> <private-output.json>
```

The operator probe uses mTLS and the same per-user NATS session as the agent.
It performs only allowlisted reads. It does not expose new MCP/chat tools,
trigger reports, book trades, import data or start feeds. Store the output
privately: it contains scoped business records. No tokens/passwords are written.
The local raw evidence is in ignored `build/output/agent-business-baseline.json`.

Validation: 28 component tests pass on Windows and in the pinned Linux Node
container, including denied/malformed reads, selected-party isolation, partial
run health and mismatched pricing dates. Linux output is retained locally in
`build/output/agent-business-linux-check.log`; fixtures do not close live gates.

Each resource is validated and scanned with a 2,000-row bound. Result reads use
a backend party filter; all output additionally checks tenant and selected
party. Successful empty, denied, backend-refused, invalid and incomplete reads
remain distinguishable. The scan checks up to 100 run-health records and 50 IR
series. A limited scan cannot establish global absence or reporting readiness.
The report includes limits, IDs, dates, measures, health and unmet gates.

## Live findings

| Resource | Selected-party records | Read status |
| --- | ---: | --- |
| Market series | 71 | Successful, complete within ceiling |
| Trades | 73 | Successful; Bond and FxForward headers |
| Books | 2 | Successful |
| Portfolios | 3 | Successful |
| Report definitions | 25 | Successful |
| Report instances | 61 | Successful |
| Stored run-result rows | 584 | Successful, party-filtered |

Run health: 4 completed clean, 2 completed with errors, 25 failed, 30 running.
These are point-in-time observations; the existing scheduler may change them.
Stored measures include `npv`, `npv_base`, `notional`, `Notional(Base)`.
They do not establish supported Greeks, units or safe aggregation conventions.

Concrete trade available for discovery: `049da6ed-e746-5cf4-af7a-399e045e9849`.
Concrete clean run: System Health, `7b072cd2-07ac-47dd-a2a1-44a1f955a832`;
it has no stored valuation result rows and is not an NPV demonstration.
An NPV candidate exists for trade `05dcc4f0-4af9-597f-8e20-94c3018c6b4c`,
run `bdfd79f3-0089-4512-8fb6-fd8a1e783291`, but that run completed with errors.
No clean, completed run with a matching selected-party trade NPV and pricing
date was verified. `reportingReady` is therefore **false**. A result row alone
is not proof of a successful valuation. Do not label partial results clean or
invent an NPV demonstration from fixtures.

All 32 selected-party IR series were checked; none had a stored observation.
The existing historical EURUSD data remains the market-data example from 0.1.1.
To demonstrate populated IR, import a labelled point observation for an existing
selected-party IR series through supported ingestion, or explicitly enable and
start its existing synthetic IR feed after checking its configuration. Neither
action was performed by this read-only stage. Generated data must be labelled.

## Dependencies and next actions

No server source/build, new backend handlers, broker ACL/configuration changes
or new NATS subjects were required. Only the agent adapter's fixed read
allowlist was extended for trades, books, portfolios, report definitions,
instances, results and run health. No new agent application release is needed
to use this operator probe; deployed chat/MCP behaviour is unchanged.

The environment was initially stopped. Existing Docker/PostgreSQL, configured
NATS and the already-built native fleet were started for validation. No rebuild,
database recreation or queue purge occurred. Normal existing scheduler/service
activity is separate from the probe; database immutability is not claimed.

Stage 0's inspection is delivered; its clean-NPV data-readiness gate remains
open. Stage 1 discovery can proceed with real trades/books/portfolios. Before
claiming the Stage 2 clean-NPV acceptance, inspect a failed/partial intended
valuation run and its prerequisites, then explicitly prepare or run one approved
valuation for a known trade/book/date. A required backend bug fix must be called
out before implementation. Do not start report execution implicitly from reads.

Least-privilege identities, cross-party integration tests, hosted authentication,
broker subject restrictions and Linux NVIDIA execution remain separate open
gates. A successful administrator probe does not close them.
