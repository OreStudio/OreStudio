# Future architecture and backend PR proposals

Status: discussion backlog for the project lead, 2026-11-07. These proposals
are **not authorised for implementation in the current agent/evaluation work**.
No new reporting service, OreStudio server, ORE engine, schema, broker or main
UI changes are required to collect an honest agent evaluation baseline.
See the [active agent-only stages](business-use-cases-plan.md).

## Boundary and review process

Keep the current agent-side deterministic composition and its coverage,
provenance and financial-unit checks while evaluating it against existing
NATS APIs. Its calculated summaries remain client-derived, not a claim that
the backend already provides an authoritative aggregate query. Missing
capabilities/data are recorded as gaps, not repaired by server changes or
fabricated results to make an evaluation pass.

The future target is authoritative financial semantics in domain-owned
operations, with the independently deployed agent/MCP app owning intent,
clarification, bounded orchestration and presentation. Prefer extending the
existing C++ reporting/domain components. A new query-service deployment is
an option to justify, not a decision already taken. NATS remains the current
business transport; HTTP migration is a separate optional proposal.

Each future PR brief should identify the business problem, owning component,
current API/data gap, proposed versioned contract, auth/tenant/party rules,
schema/native-build/subject/ACL impacts, compatible rollout/rollback, and
acceptance evidence. Take those briefs to the project lead before selecting
or implementing backend work. Proposals may be split or combined after review.
An evaluation failure is evidence for discussion, not automatic approval of a PR.

## Existing backend work to separate when preparing PRs

The branch already contains historical commit `cc917e0d36` adding
`reporting.v1.ops.copy_run_scope` for earlier isolated validation. It was tested
in a temporary instance and is not permanently deployed in the original fleet.
This plan does not undo, extend or redeploy it. Audit that existing delta with
the project lead and select/split commits so a future **agent-only PR does not
silently include backend source changes**. Do not rewrite branch history or
claim the whole historical branch is agent-only merely because new work is.
No PR is opened by this plan update.

## Proposed PR sequence

| Proposal | Future concern | Current handling |
| --- | --- | --- |
| F1 | Stable business-query contracts and first trade valuation query | Existing NATS reads and agent adapters |
| F2 | Backend historical membership, completeness and NPV aggregation | Retain tested agent composition and report its limits |
| F3 | Backend Greek semantics and thin reporting tools | Retain raw bucket rules; never fill absent sensitivities with zero |
| F4 | Hosted identity, broker isolation and command security | Keep local read-only profile; record unproven controls |
| F5 | Report execution, packaging and lifecycle correctness | No execution tools or automatic operator workarounds |
| F6 | Backend-validated booking drafts and review UI | Evaluate intent with labelled fixtures; no persisted booking |
| F7 | Confirmed transactional booking and idempotency | No submission capability |
| F8 | Product support and market/volatility data coverage | Mark missing data/capability explicitly |
| F9 | Genuine two-way pricing and quote contracts | No invented executable quotations |
| F10 | Durable business workflows and recovery | No model-generated backend step sequences |

The following sections preserve the architectural proposal detail from the
previous plan. They are future acceptance criteria, not current release gates.
Agent-only tracing/evaluations can be built now; external service/security
changes within these proposals remain deferred.

## Future PR F1 — stable business contracts and first valuation query

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

Evaluation baseline work is active Stage 4 and does not wait for this PR.

Acceptance: the first authoritative query serves the same authorised result to
the adapter and an independent non-AI client. Contract tests cover pinned and
candidate backend versions, denied/expired identities, malformed responses and
missing capabilities. Record the rule-ownership map and declare exact server,
subject and ACL changes before implementation. No command tools are introduced.

## Future PR F2 — authoritative read queries and scope valuations

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
relationship rules identified in F1 to their owning operations where
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

## Future PR F3 — authoritative Greeks and reporting cutover

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

## Future PR F4 — hosted identity, broker isolation and command readiness

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

## Future PR F5 — report execution, packaging and lifecycle

Enable only after the applicable approved business/query/security gates. Use an authoritative report
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

## Future PR F6 — booking drafts and review

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

## Future PR F7 — confirmed booking

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

## Future PR F8 — product and market-data expansion

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

## Future PR F9 — genuine quotations

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

## Future PR F10 — durable business workflows

Begin with one backend-defined workflow such as funding or wash, once its
business implementation is available. Expose explicit domain commands and
status reads, never arbitrary shell commands, SQL, subjects or model-generated
step sequences. Identify inputs, permissions, approvals, idempotency and durable
state transitions. Backend orchestration owns recovery and compensation.

Acceptance: one authorised process runs with a durable ID, visible progress and
audited outcome. Test partial failure, duplicate initiation, agent downtime,
recovery and supported cancellation. Do not claim workflows exist merely because
the agent can describe them or publish a NATS message.

