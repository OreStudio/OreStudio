# Stage 1: trade and reference-data discovery

Agent version 0.1.2 adds `find_trades`, `read_trade`, `find_books`,
`find_portfolios` and `find_counterparties`. Internal chat and external MCP use
the same typed execution, per-user NATS session, limits and validation. No
mutation tools are added. No OreStudio server source/build or broker
configuration changes are required; only existing read subjects are added to
the adapter allowlist. The standalone agent is released independently.

Try:

- “Show my books.”
- “Show trades in book Group.” Then reply “Group FX Hedges”.
- “Show trades in portfolio Group Treasury Portfolio.”
- “Read trade 049da6ed-e746-5cf4-af7a-399e045e9849.”
- “Find counterparty CARECR9.”

## Search and scope

Trade queries match UUID, origin reference or trade type, case-insensitively.
They do not search currency-pair economics. Optional book/portfolio names or IDs
are resolved against the selected party. Exact names/IDs take precedence over
substring matches. Multiple matches require a choice; no arbitrary entity is
selected. A pending scoped membership choice lasts five minutes in that session
only. A different question, logout or restart discards it.

Book/portfolio membership queries use `trading.v1.ops.export_portfolio`, the
existing read-authorized inline export. The server resolves taxonomy membership;
the agent never derives membership from similar names. The storage-export and
booking commands are not allowlisted. Returned trade identities must match the
selected tenant/party. Book and portfolio catalogues also restrict that party.
Counterparties are an entitled **tenant** catalogue without a party ownership
field; the UI/evidence labels that distinction rather than inventing one.

Reference/trade catalogue scans are complete within a 2,000-row ceiling. They
use a shared as-of snapshot per request and validate page counts and duplicates.
Inline membership export is limited to 1,000 trades and must reach a short final
page. A ceiling or inconsistent read produces an error, not a false global
absence or complete total. Output pages contain at most 25 records and expose
offset/total matches. Business results are rendered from validated evidence.

The real counterparty catalogue contains 2,454 records. Exact short-code reads
and UUID-filtered reads avoid scanning it. Name/substring searches operate on
one explicitly labelled catalogue page; `nextOffset` allows continuation. A
partial page never proves global absence or uniqueness, including the last
page when earlier pages were not included. An efficient global name-search
filter would be a future backend enhancement; it is not implemented here.

## Trade details

`read_trade` first validates the trade header's tenant and selected party before
requesting related data. It reads booking, direct portfolio links and typed
instrument data. The booked book must be in the selected party's catalogue.
Direct portfolio assignments are separate from the book's parent hierarchy;
all linked portfolios must be in scope and the hierarchy must be acyclic.
Counterparty details use an exact ID lookup. The trade header is read again to
detect changes while assembling the view; this is not a server-wide atomic
snapshot across every related table.

FX-forward economics include bought/sold currency and precise decimal amounts,
value date and settlement. Amounts are not converted through floating-point
display arithmetic. Bond instruments expose issue UUID and notional only,
labelled `issue_reference_only`; coupon, schedules and full issue economics are
not inferred. Missing instruments and unsupported products have explicit status.
No NPV, valuation, draft creation or booking is performed by these tools.

Only tools relevant to the question domain are sent to the small local model:
business discovery or market-data tools, reducing irrelevant tool choices.
External MCP still advertises the full catalogue. This selection is a context
budget mechanism; authorization remains in the shared executor/backend. The
initial 4096-token inference window was expanded to 262144 on 2026-11-07;
see [inference deployment](inference.md). Domain selection remains useful.

## Validation

Fixtures cover scoped searches, denied/malformed pages, duplicate-name
resolution, exact membership requests, precise FX economics, hierarchy versus
direct assignments, cross-party identity rejection and changed versions.
The independent MCP client executes discovery reads. Actual CUDA LFM3 questions
exercise book listing, ambiguous-book clarification, trade details and exact
counterparty lookup. Existing FX/IR/currency regression checks remain required.

Run the read-only Acme integration check:

```text
node scripts/smoke-discovery.mjs <agent-url> <private-credentials.json> <Acme-Plc-party-id> <optional-other-party-id>
```

The optional second-party check logs in separately and confirms the first
party's known trade cannot be read through that second agent context. It does
not establish low-privilege-role coverage or cross-tenant broker isolation.
Keep the previously documented hosted-auth/broker and Linux GPU gates open.

Live data verified during implementation: two books, three portfolios, 73
selected-party trades (63 FX forwards and 10 bonds). “Group FX Hedges” contains
63 trades. The sampled FX-forward read includes booking, counterparty and
economics; its direct portfolio-link list is empty while its booking hierarchy
contains Group Treasury Portfolio. These distinct facts must not be conflated.
