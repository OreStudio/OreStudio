# Business use cases: staged implementation plan

Created: 2026-11-06. Architecture revision: 2026-11-07. Stages 0–3 have the
delivered scope and limitations recorded below. Stages 4–14 are planned, not
implemented by being listed. Current runtime: agent 0.1.5, implementation
`753a54b4550b9b73a602fc19a7b42b9393e773ad`, validated at `212f77b63d`.
Original planning baseline: agent 0.1.1 at `7cc8511292a2416da54c25fe8bd54530326b37c7`.
Scope: the market-data, reporting/Greeks, trade-booking, quotation and workflow
use cases discussed in OreStudio issue #4295, using the architecture agreed
with the user. This extends the initial infrastructure plan in `plan.md`.

## Fixed architectural decisions

- Keep one separately released TypeScript agent/MCP application. Internal agent
  calls and external MCP calls use the same validated tool execution layer.
- Use NATS only for OreStudio business operations. No HTTP fallback, direct SQL,
  or imports from moving backend implementation code. Prompts, MCP handlers
  and conversational orchestration stay outside business services.
- Authoritative financial rules belong to domain-owned services: pricing,
  instrument/convention interpretation, historical membership, result eligibility,
  completeness, units and aggregation. Current agent-side reporting composition
  is a transitional implementation to migrate, not a permanent second source
  of financial truth. The agent retains intent interpretation, clarification,
  bounded tool orchestration, defensive validation and presentation.
- Logical ownership and deployment are separate decisions. Prefer extending
  the existing reporting/domain component; use a separately deployed query or
  read-model service only where deployment, scale or ownership warrants it.
  Keep MCP and inference independently released; prompts and MCP stay outside
  the business services. Do not add a gateway merely to rename RPC calls.
- Give the agent a small versioned business-operation contract, with capability
  checks and explicit compatibility behavior. Keep it transport-neutral, with
  NATS as the only implementation now. A future HTTP adapter is a separate
  decision and does not move business rules by itself.
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
| 4 | Read a trade valuation through a stable business contract | Rule ownership, version/capability contract and first backend query |
| 5 | Obtain authoritative historical scope and NPV summaries | Domain-owned membership, completeness, units and aggregation |
| 6 | Obtain authoritative Greek buckets through thin agent/MCP tools | Backend risk semantics and reconciled reporting cutover |
| 7 | Handle read conversations through explicit workflows and repeatable AI evaluations | Typed intent, clarification state and versioned evaluation corpus |
| 8 | Prove the intended deployment and command-readiness profile | Identity, authorisation, observability, failure and latency gates |
| 9 | Start a requested report and follow its progress | Stages 4–8 plus verified commands and durable status/idempotency |
| 10 | Turn trading shorthand into a reviewable booking draft | Backend product/economics validation and review UI |
| 11 | Submit one explicitly reviewed booking exactly once | Backend transactional booking, approval binding and idempotency |
| 12 | Broaden product and market-data coverage | Product-specific backend support and real entitled data |
| 13 | Return genuine two-way quotations | Backend bid/offer, spreads, volatility and pricing support |
| 14 | Initiate and monitor supported business workflows | Backend durable workflow definitions and permissions |

Stages 0–3 remain delivered read-only prototype checkpoints; their success does
not establish ownership migration or production readiness. Stages 4–6 migrate
one vertical slice at a time while keeping existing reads runnable. Evaluation
starts in Stage 4 and is consolidated in Stage 7; Stage 8 work can proceed in
parallel. Later backend-contract inspection can proceed, but report-execution
and booking tools stay disabled until their applicable gates pass.

This revision inserts five checkpoints before commands. Historical records keep
their original numbering: old Stage 4 report execution is now 9; old Stage 5
drafts is 10; old Stage 6 booking is 11; old Stages 7/8/9 are now 12/13/14.
The original six infrastructure checkpoints are a separate sequence.

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

**NPV delivered in 0.1.4; available raw Greek buckets delivered in 0.1.5.** Historical
booking populations, consistently denominated totals and every contributor
reconcile for two real books and their parent portfolio. Independent MCP,
GPU-backed chat, run selection, party isolation and partial-run withholding
pass. See [scope behavior and limitations](scope-reporting.md) and
[validation evidence](validation.md). Only existing NATS read subjects were
added to the agent allowlist; no server handler or broker change was needed.
Greek reads preserve raw Delta/Gamma, full factors, exact target shifts,
scenario and currency; only a bucket with one figure per expected trade has
a full-scope total. Vega selects cash volatility Delta at its stated bump;
DV01 selects signed ORE rate Delta at an exact 1bp target bump. No per-unit
derivative, inferred zero, par conversion or cross-factor grand total is
invented. Every available live Greek figure reconciles to the actual engine
CSV; the complete FX-book GBP 1Y bucket and sparse parent-portfolio behavior
pass external MCP and GPU checks. Positive live vega is not demonstrated by
this non-option dataset. See [Greek contracts](greeks.md) and
[operator setup and remaining automatic-execution dependencies](greek-validation.md).

Questions: “What is this book's NPV?”; “Show available Greeks for portfolio X”;
“Which trades contribute most to this result?”

The delivered prototype composes these reads and aggregates in the agent.
Stages 4–6 migrate the authoritative rules to domain-owned business queries.
Retain verified additive contracts, exact decimal handling, complete authorised
populations and double-counting checks throughout that migration.
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

## Stage 4 — business ownership and the first stable query

Inventory every current tool's rules and assign owners. Financial semantics
belong to reporting/trading/market-data services; permissions are enforced by
the owning service. The agent retains conversational selection, clarification
expiry, execution budgets, presentation and defensive validation. Include market
point matching, instrument conventions and discovery relationships in this
inventory, distinguishing client preferences from authoritative domain rules.

Define a small versioned business-query contract: eligible-run discovery,
trade valuation, scope valuation and scope risk are candidate operations, not
yet implemented APIs. Deliver one stored-trade valuation query in the existing
reporting component and consume it through the current agent/MCP tool adapter.
Specify explicit run/date selection, decimal-string amounts, units, provenance,
request IDs, bounds and unavailable/ambiguous/stale/partial/unsupported statuses.
Define trusted caller context and identity delegation; model-supplied identity
cannot grant authority. Pin generated/shared contracts independently of moving
BFF implementation code. Unsupported versions/capabilities fail clearly, with
no SQL/HTTP fallback or reinterpretation of malformed data.

Start a versioned evaluation corpus from the current live examples and hard
negative cases. Capture baseline tool choice, argument fidelity, clarification,
unsupported behavior and latency before changing the model/tool surface.

Acceptance: the first authoritative query serves the same authorised result to
the adapter and an independent non-AI client. Contract tests cover pinned and
candidate backend versions, denied/expired identities, malformed responses and
missing capabilities. Record the rule-ownership map and declare exact server,
subject and ACL changes before implementation. No command tools are introduced.

## Stage 5 — authoritative read queries and scope valuations

Move historical booking membership, portfolio-subtree interpretation, eligible
run selection, actual pricing-date/cut provenance, currency resolution,
completeness and exact stored-value aggregation to domain-owned queries.
Preserve booking hierarchy vs direct trade assignments. Return whole-scope
totals and bounded contributors from the same identified result set; paging
the drill-down must not alter its total or coverage.

Prefer the existing reporting component unless evidence warrants a separately
deployed query/read-model service. Cross-domain facts come through authorised
domain interfaces, never cross-service SQL grants or unrestricted service
accounts. Batch reads and aggregate near the owning data. Provide a result-set/
version token or detect/refuse changes across pages; do not claim an atomic
snapshot without providing one. Migrate authoritative market/instrument and
relationship rules identified in Stage 4 to their owning operations where
needed. Name clarification stays in the agent. Positive live IR data remains
a separate data gate; missing observations are not repaired by a refactor.

Use an opt-in read-only shadow-validation harness to compare new queries with
the current deterministic implementation and independent references. Reconcile
every historical book/portfolio contributor and total, including missing,
duplicate, mixed-currency and timestamp-boundary cases. Shadow comparison is
temporary migration tooling, not a permanent double-read production path.

Acceptance: agent and non-AI clients receive identical authoritative NPV/scope
facts. Cut over one tool at a time only after parity and failure checks pass.
Remove duplicate financial rules for migrated tools while retaining client-side
schema/identity checks. Preserve independent rollback with explicit compatible
release selection; do not silently resume legacy aggregation on a backend error.

## Stage 6 — authoritative Greeks and thin reporting tools

Move sensitivity family/source validation, factor/tenor/scenario/currency and
raw/par separation, exact target-shift semantics, coverage, decimal aggregation
and contributor ranking into the reporting query owner. Preserve cash finite-
difference, signed 1bp rate-Delta and stated-bump vega contracts. Missing rows
never establish zero. Return mathematical meaning and unavailable stencil
metadata explicitly. Per-unit Greeks/par conversion need their own verified
contract and data; never relabel existing results to imply those capabilities.

Keep compatible user-facing tool semantics. The combined agent/MCP app becomes
a typed request adapter and renderer for authoritative reporting facts. Both
entry paths still share one executor directly, with no internal MCP/HTTP round
trip. Other clients use the same business queries without importing agent
prompts or orchestration code.

Acceptance: repeat actual CSV/Decimal, independent-client, MCP and GPU checks
against the new owner. Reconcile all available figures and prove incomplete
parent buckets still have no total. Obtain real volatility results before
claiming positive live vega; label fixture-only coverage while that gate is
open. Remove duplicate agent aggregation after cutover. Prove separate backend/
agent deployment and rollback with compatible contracts, permissions and NPV
behavior preserved. Record any remaining domain-rule migration explicitly.

## Stage 7 — explicit conversational workflows and AI evaluations

Retain one bounded tool-using assistant. Replace growing keyword/regex routing
with a small typed intent/workflow layer, migrated one read use case at a time.
Deterministic code owns allowed transitions: resolve intent, clarify missing
scope/provider/run, validate a request, call an authorised business operation
and render its facts. The model proposes typed intent/arguments; code rejects
unrequested scope, selector, partial-result and action changes. Use constrained
outputs/tool schemas where the pinned runtime supports them, retaining strict
validation even when generation is constrained.

Separate conversational state from durable backend operation state. Pending
choices stay session-scoped and expiring; later backend jobs survive a closed
chat. Render numerical facts deterministically. Model explanations cannot
overwrite them. Prefer focused business-level tools and bounded pages rather
than filling the 256K context with raw catalogues or whole portfolios.

Build a repeatable evaluation harness with pinned model/GGUF, quantization,
runtime, prompt, tool-schema and business-contract versions. Cover varied
paraphrases, multi-turn clarification, ambiguous names, absent/stale data,
unsupported products, attempted identity/scope changes, untrusted tool text,
timeouts and cancellation. Separate deterministic tool-result replays from
authorised live checks; sanitise/protect recorded data and exclude credentials.

Measure task success, tool choice, argument fidelity, clarification/refusal,
numerical attribution, unrequested actions, truncation and model/tool/transport
latency separately. Set explicit acceptance thresholds and a baseline before
comparing candidates; repeat trials to expose sampling variance. CI runs
deterministic contracts/replays and the selected repeatable model profile;
live/backend and hardware-dependent checks are explicit opt-in suites.

Acceptance: prompt/model/runtime changes have reproducible comparisons and
visible failures/missing checks. Critical scope/permission/numerical and
unrequested-action cases must pass, as must the agreed task-success/latency
thresholds. A larger context window or successful demo is not accuracy evidence.
No multi-agent framework or model replacement is required by this checkpoint.

## Stage 8 — deployment profile, observability and command readiness

Validate the intended profile: local read-only, local commands, or hosted MCP.
Preserve user/tenant/party authority through delegation, expiry, logout and
revocation. Test real low-privilege and cross-party/tenant identities. Provision
and verify restricted broker subject permissions before claiming that boundary;
dedicated mTLS alone does not establish it. Financial/domain checks remain
server-enforced.

Hosted MCP requires its resource-scoped authentication/client enrollment and
TLS/origin controls; the local opaque-bearer prototype does not close that gate.
Unrequested remote hosting is not a prerequisite for local read validation.
Local command releases still require permission, immutable-review, idempotency,
unknown-outcome reconciliation and audit gates applicable to their actions.

Add request correlation and separate spans for model generation, tool selection,
adapter/transport, backend query and rendering, with redacted audit data. Measure
side-by-side and LAN p50/p95/p99 latency, fan-out, concurrency, errors and resource
use. Validate cancellation/timeouts, reconnects, dependency failures, config
preservation and independent rollback. Any cache must respect permissions,
tenant/party, run/result-set identity, selection dimensions and freshness.

Acceptance: publish the tested deployment/capability matrix and measured budgets,
distinguishing local/hosted and Windows/Linux NVIDIA gates. Dependency failures,
revocation and readiness behave explicitly. Before a command release, prove its
confirmation and duplicate/unknown-outcome handling end to end. Advertising a
tool or receiving a model request never enables a write capability by itself.

## Stage 9 — requested report execution

Enable only after the applicable Stages 4–8 gates. Use an authoritative report
command and backend-owned durable lifecycle. Fix the known normal-packager
simulation/sensitivity export gap and prepared-input pending-to-running bug
before exposing those paths. Operator starts used for Greek validation are
not an acceptable automatic agent workaround; see [evidence](greek-validation.md).

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

## Stage 10 — booking draft and review

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

## Stage 11 — explicit confirmed booking

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

## Stage 12 — product and market-data expansion

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

## Stage 13 — genuine market quotations

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

## Stage 14 — workflows and processes

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
the agent's separate generated-output allowance defaults to 2048 tokens and
is configurable; truncated calls remain errors.
Maintain the evaluation corpus and independent financial references throughout
Stages 4–14. Component-test counts, successful live questions and context
capacity establish their stated evidence, not general conversational accuracy
or production readiness. Distinguish proposed contracts, implemented features,
deployed versions and validated deployment profiles.

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

Stages 0/1 inspection and discovery, Stage 2's clean single-trade NPV
demonstration, and Stage 3's NPV/available raw Greek reads are delivered with
the explicit coverage and data limitations above. Next is Stage 4: inventory
rule ownership, capture the model/tool evaluation baseline, define the stable
business-query contract and deliver one authoritative stored-trade valuation
query through the current agent/MCP tool. Declare the exact reporting/API and
NATS changes before implementation. Follow with scope/Greek query migration
and conversation/evaluation/deployment gates in Stages 5–8.
Report editing, execution and trading writes remain separate approval/security
stages. No command is enabled by this read-only release.

## Design references

The ownership/deployment choices above are OreStudio-specific decisions. The
workflow/tool/evaluation direction is informed by
[Building Effective Agents](https://www.anthropic.com/research/building-effective-agents)
and [Writing effective tools for agents](https://www.anthropic.com/engineering/writing-tools-for-agents).
