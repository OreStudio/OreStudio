# Business use cases: staged implementation plan

Date: 2026-11-06. Status: proposed implementation sequence; stages below are
not implemented merely by being listed. Baseline: agent 0.1.1, code commit
`7cc8511292a2416da54c25fe8bd54530326b37c7`; validation commit `61b26b7dbb`.
Scope: the market-data, reporting/Greeks, trade-booking, quotation and workflow
use cases discussed in OreStudio issue #4295, using the architecture agreed
with the user. This extends the initial infrastructure plan in `plan.md`.

## Fixed architectural decisions

- Keep one separately released TypeScript agent/MCP application. Internal agent
  calls and external MCP calls use the same validated tool execution layer.
- Use NATS only for OreStudio business operations. No HTTP fallback, direct SQL,
  imported moving backend source, or agent business logic inside OreStudio.
- Keep GPU llama.cpp inference and its model independently deployed. Retain
  native Windows and Linux packaging; Linux GPU validation remains open.
- Keep all reads and commands tied to the authenticated tenant, selected party
  and backend permissions. Model-supplied identity is never authoritative.
- Report required OreStudio server or NATS changes before making them. Adding a
  subject to the agent's allowlist is distinct from changing a broker ACL or
  implementing a new server handler. Identify all three explicitly at each gate.
- Reuse this fork/worktree. Do not duplicate or rebuild the main CMake tree for
  agent-only changes. If backend work is required, isolate its scope and rebuild
  only what it needs. Never recreate the database or purge queues as a shortcut.

## Release sequence

| Stage | Runnable business outcome | Expected dependency |
| --- | --- | --- |
| 0 | Known usable data and capability baseline | Read-only backend verification |
| 1 | Discover trades, books, portfolios and counterparties | Existing NATS reads, subject allowlist additions |
| 2 | Explain one trade's stored NPV/results | Existing reporting reads and completed report data |
| 3 | Summarise a book/portfolio and available Greeks | Reliable scoped results and aggregation semantics |
| 4 | Start a requested report and follow its progress | Verified report command and durable status semantics |
| 5 | Turn trading shorthand into a reviewable booking draft | Supported product/economics validation and review UI |
| 6 | Submit one explicitly reviewed booking exactly once | Backend transactional booking and idempotency capability |
| 7 | Broaden product and market-data coverage | Product-specific backend support and data feeds |
| 8 | Return genuine two-way quotations | Backend bid/offer, spreads, volatility and pricing support |
| 9 | Initiate and monitor supported business workflows | Backend durable workflow definitions and permissions |

Stages 0–3 are the next delivery tranche. Stages 4 and 6 are separate command
releases, not an automatic consequence of enabling reads. Stage 5 can begin
after discovery while reporting work proceeds. Stages 8–9 depend on backend
business capabilities and cannot be completed solely by adding prompts/tools.

## Stage 0 — capability and data baseline

Inspection delivered: see [live Stage 0 findings](business-baseline.md). Discovery
data is available. The clean trade-NPV gate passed on 2026-11-07 through the
separately prepared [live validation run](valuation-validation.md); the
populated-IR gate remains open.

Inspect the deployed NATS contracts, permissions and actual selected-party data.
Record available trade types, books/portfolios, report definitions, runs, result
measures, pricing dates and health. Pin the adapter contract revision. Distinguish
missing permissions, unsupported operations, empty data and failed processing.

Use existing historical/demo market data with its dates and provenance. Identify
a populated IR point for a live lookup demonstration; otherwise record the
specific import/feed action needed. Identify at least one completed valuation
run for the selected party. Preparing data or triggering a run is a separately
declared action, not part of the read-only inspection. Fixtures remain labelled.

Acceptance: a reproducible capability/data report, a concrete trade and report
run to test, and a server/NATS dependency list. Do not claim a business use case
passes because a fixture passed while the real deployment has no data.

## Stage 1 — trade and reference-data discovery

Delivered in 0.1.2; see [implementation and limits](discovery.md) and
[current release evidence](validation.md). Global counterparty-name search is
limited to explicit catalogue pages; full Bond issue economics remain unsupported.

Questions: “Show the trades in book X”; “Find this trade”; “Which portfolios
contain it?”; “Find counterparty Acme.” Proposed tools: `find_trades`,
`read_trade`, `find_books`, `find_portfolios`, `find_counterparties`.

Resolve names to immutable IDs with bounded searches and explicit ambiguity
choices. Read product/economics through the supported domain operations, not
only the trade header. Verify booking and portfolio membership relationships;
never infer membership from a matching name. Retain narrowly scoped, expiring
clarification state, not an unrestricted session-wide data dump.

Acceptance: real selected-party discovery and drill-down through both agent
and MCP; duplicate names, absent entities, pagination, unsupported products and
permission failures behave explicitly. A second user/party cannot reuse the
first user's resolution or results. No mutation subjects are enabled.

## Stage 2 — single-trade valuation results

Read-only code slice delivered in 0.1.3; see [reporting behaviour](reporting.md)
and [validation](validation.md). The clean live trade-NPV acceptance gate passed
on 2026-11-07 after an isolated NPV-only run and the separately declared backend
scope-copy prerequisite; see [live evidence](valuation-validation.md).

Question: “What is the NPV of trade X?” Proposed tools: `find_report_runs`,
`read_run_status`, `read_trade_results`.

Use `reporting.v1.run_results.list` and verified report-instance/run-health
reads. Select a named run or a clearly defined eligible completed run covering
the requested trade. Make date/currency/run ambiguity visible. Distinguish
requested date, actual `priced_at`, market cut, completion time and result
`as_of`; display any date adjustment note. Do not turn completion timestamps
into valuation dates. Treat completed-with-errors as partial, not clean success.

Render numerical facts from backend results with trade ID, run ID, measure,
currency, source and pricing date. Do not calculate a fresh price in the LLM or
silently reuse an older failed/partial result. Missing results are not zero.

Acceptance: one real stored NPV matches the existing report UI/backend record;
tests cover absent results, failed/running/partial runs, stale runs, duplicate
measures and currency ambiguity. Measure existence alone does not prove a run
completed successfully. Unsupported Greeks are explicitly unavailable.

## Stage 3 — book/portfolio summaries and Greeks

Questions: “What is this book's NPV?”; “Show available Greeks for portfolio X”;
“Which trades contribute most to this result?”

Prefer authoritative backend aggregate rows. If client-side aggregation is
necessary, document which measures are additive, use appropriate exact decimal
handling, retrieve the complete authorised population and prevent double counting.
Do not mix currencies, valuation dates, market cuts, scenarios, risk-factor
dimensions or report runs. A delta/gamma label alone is insufficient: preserve
units, bump conventions and dimensions. Never invent those from the measure name.

Show included/expected trade counts, missing/failed counts and completeness.
Provide bounded drill-down to contributing trades with the same run identity.
If needed dimensions are absent from the API, leave that aggregate unavailable
and record the required backend extension instead of approximating it.

Acceptance: a real aggregate reconciles with its report and drill-down; partial
coverage, mixed currencies, non-additive Greeks and pagination cannot produce a
misleading total. Populated fixtures test edge cases in addition to real data.

## Stage 4 — requested report execution

Question: “Run the approved NPV report for book X at date Y.” First verify the
existing report trigger contract, prerequisites, report scope, run health and
supported cancellation. The current trigger DTO includes operational fields;
the agent must not invent tenant/job/grant IDs to satisfy it.

Present the exact definition, scope, valuation date and material resource use
for confirmation. Revalidate permission and configuration when submitting.
Track an accepted durable run ID through running, partial, failed or completed
states; a transport acknowledgement is not a result. Explain timeouts as an
unknown submission outcome until reconciled. Do not blindly retry submission.

Acceptance: a confirmed request creates one discoverable run, survives agent
restart, and exposes truthful progress/results. Verify backend idempotency or
add a supported command contract before enabling retries. If cancellation is
unsupported, say so; closing chat must not pretend to cancel backend work.

## Stage 5 — booking draft and review

Start with one verified supported product, preferably an FX forward if the
backend supports its full booking flow. Question: “Buy 20M EURUSD six-month
forward with counterparty X.” Resolve what “buy”, currency/notional, tenor,
dates, book, counterparty and settlement mean. Ask for missing terms; never
invent the forward rate, dates or counterparty. Preserve original customer text.

Extract typed economics and use backend validation/conventions where available.
Keep drafts separate from booked trades. If a server draft is required, label
its persistence and verify it cannot trigger booking or downstream processing.
Open/populate the existing booking review flow only through a supported handoff;
do not bypass its normal validation with generated XML or field-level CRUD.
If no handoff exists, propose the smallest explicit backend/UI change.

Acceptance: the user can inspect every economically material field, correct it,
cancel or discard. Draft versions and clarification state are scoped/expiring.
No booking occurs, including when the input says “mine”, “done” or “book it”.
At least one real end-to-end review flow is demonstrated for the chosen product.

## Stage 6 — explicit confirmed booking

Enable only after validating the backend's complete transactional booking
operation. The existence of trade and trade-booking CRUD does not establish
that a multi-entity booking is atomic, validated or idempotent.

Bind confirmation to an immutable draft version/economic digest, authenticated
user and party, with an expiry. Edits invalidate confirmation. An LLM statement
or tool-result text cannot grant approval. External MCP callers need the same
confirmed workflow or a narrowly documented client approval mechanism; they
cannot bypass review by calling a lower-level tool.

Use backend-enforced idempotency and concurrency preconditions. After submission
read back the trade and booking receipt. Reconcile uncertain outcomes before
retrying; return the original result for a duplicate confirmation. Record an
audit trail without passwords, tokens or unnecessary trade content.

Acceptance: one reviewed draft produces exactly one complete booking; duplicate
clicks, retries, agent restart, expired approvals, changed drafts, revoked
permissions and timeout-after-commit are tested. If the backend lacks the
required guarantees, this stage remains unavailable until domain API work lands.

## Stage 7 — product and market-data expansion

Extend the proven read/draft/confirmed-booking pipeline one supported product
at a time: vanilla rates, then supported options/credit/equity structures as
their backend flows permit. Do not assume the issue's examples are all bookable
today. Each product gets its own conventions, validation and reference examples.

Expand FX/IR data coverage first; add volatility only with actual backend
surface/expiry/strike/delta conventions and entitled data. Source selection,
historical dates and stale/missing-data behaviour remain explicit. Obtain any
needed feed access separately; do not present generated demo data as live data.

Acceptance: a per-product support matrix identifies extraction, validation,
data, pricing and booking status. An unsupported request yields a useful
explanation, never a superficially plausible ticket or quote.

## Stage 8 — genuine market quotations

Example: the issue's EURUSD risk-reversal request. This is a backend dependency
stage: volatility, bid/offer/spread modelling, product/strategy pricing and
applicable limits must exist. A single NPV or spot observation is not a two-way
quote. A mock may test protocol/UI but must be visibly non-executable.

The agent resolves intent and calls authoritative pricing operations. A quote
identifies economics, notional, side, units, valuation/market snapshot, bid/offer,
validity and quote ID. Execution, if supported, references that exact quote and
still uses the reviewed booking flow. Expired quotes cannot be silently reused.

Acceptance: reconcile supported quotes with backend pricing; verify sides,
units, stale-market/expired-quote rejection, entitlement and booking linkage.
Do not enable this stage until the underlying quant/data work passes its tests.

## Stage 9 — workflows and processes

Begin with one backend-defined workflow such as funding or wash, once its
business implementation is available. Expose explicit domain commands and
status reads, never arbitrary shell commands, SQL, subjects or model-generated
step sequences. Identify inputs, permissions, approvals, idempotency and durable
state transitions. Backend orchestration owns recovery and compensation.

Acceptance: one authorised process runs with a durable ID, visible progress and
audited outcome. Test partial failure, duplicate initiation, agent downtime,
recovery and supported cancellation. Do not claim workflows exist merely because
the agent can describe them or publish a NATS message.

## Cross-cutting gates and delivery discipline

For every stage: implement typed tools and deterministic fixture tests, exercise
an independent MCP client, evaluate real LFM3 tool selection, and prove one
real backend business scenario. Record limitations and reconcile numbers with
backend evidence. Add only necessary subjects to the adapter allowlist. Keep
the growing model-visible catalogue relevant to the request to reduce irrelevant
tool choices; do not silently truncate tool contracts. The initial 4096-token
test context was expanded to the model's full 262144 tokens on 2026-11-07;
the agent's separate 512-token generated-output limit remains in place.

Before enabling business commands or remote hosting, finish the applicable
security gate: real low-privilege/cross-party tests, restricted broker subject
ACLs, client enrollment/resource-scoped auth for hosted MCP, approval integrity,
audit handling and revocation. The existing local bearer prototype does not
prove these controls. Apply the same command protections locally as remotely.

Verify independent Windows/Linux builds and packaging at each release. Keep
Linux NVIDIA verification explicitly outstanding until run on actual hardware.
Measure model/tool success and latency separately from direct NATS/MCP overhead;
include side-by-side and LAN tests and concurrency appropriate to the deployment.
Operational rollout includes configuration preservation, stop/restart isolation
and rollback. No main-server rebuild is part of an agent-only release.

Finish each runnable checkpoint with evidence, a reviewable commit and a push
to `fork/feature/local-ai-assistant`. Keep backend prerequisites in separate
commits/tasks when needed and flag them before work begins. Do not open a PR
unless requested. Update this plan and the agile story/task records as gates
pass; an unverified or backend-blocked stage is not complete.

## Immediate next checkpoint

Stages 0/1 inspection and discovery, and Stage 2's clean single-trade NPV
demonstration, are delivered with the limitations above. Next is Stage 3:
reconcile one book/portfolio's complete population and additive, consistently
denominated results within one run. Verify available Greek dimensions/units
before exposing them; do not infer them from measure names. Report editing,
execution and trading writes remain separate approval/security stages.
