# Business use cases: agent-only implementation and future architecture

Created: 2026-11-06. Scope revision: 2026-11-07, following the user's explicit
instruction to keep current work in the agent and agent evaluations. Stages
0–6 are delivered with their recorded limitations. Stage 4 established the
[honest baseline](evaluation-baseline.md), Stage 5 added
[measured improvements](stage5-validation.md), and Stage 6 delivered
[reproducible evaluation/release evidence](stage6-validation.md). Backend architecture proposals are a separate, unapproved
[future-PR backlog for project-lead discussion](architecture-future-prs.md).
Current runtime: agent 0.1.7, implementation
`5004b5e7c8e36f7c26e70788227ca92fdfa1675f`; see the Stage 6 validation above.
This extends the original infrastructure context in [plan.md](plan.md).

## Current scope boundary

- Work on the TypeScript agent/MCP application, its existing-API adapters,
  conversation behavior, evaluation harness/corpus/reports and local agent
  build/deployment tooling. Keep the current standalone app and LLM server.
- **Do not change reporting/main-server/ORE source or generated backend code,
  database schemas/business records, broker configuration/ACLs, server deployments or the
  main application UI as part of this work.** No native rebuild, backend
  lifecycle repair, new report execution, data import, queue purge or test-data
  provisioning to make evaluations pass. Use existing read-only NATS APIs and
  existing records for live checks; label fixtures separately. Normal existing
  login/logout session establishment is allowed; creating accounts or changing
  roles/permissions is outside this scope.
- Preserve current deterministic client-side market/reporting/Greek composition
  for now, with all unit, coverage, identity and provenance checks. Do not remove
  working tools while waiting for future backend queries. Do not describe these
  client-derived summaries as already implemented backend authority.
- Missing API/data/permission capabilities become explicit evaluation findings
  and future-PR discussion items. Continue independent agent work without
  implementing server workarounds or silently adding an HTTP/SQL fallback.
- Keep model-proposed identity/actions non-authoritative, financial facts
  deterministically rendered, and the current local read-only profile explicit.
  Poor quality scores do not authorise writes or weaken existing safeguards.
- Keep NATS as the sole business transport and the shared internal/MCP executor.
  Pin the existing model/runtime and tool contract for a baseline; compare agent
  prompt/schema/routing/inference-parameter changes through evaluations.
- Reuse this fork/worktree and existing model/native artifacts. Build/release
  the agent independently. Backend migration, hosted auth/broker hardening and
  future command capabilities require separate project-lead discussion/PRs.

## Active release sequence

| Stage | Outcome | Dependency/status |
| --- | --- | --- |
| 0 | Known capability and data baseline | Delivered, with recorded data gaps |
| 1 | Discovery of trades and reference data | Delivered through existing reads |
| 2 | Stored trade NPV and run health | Delivered, with ambiguity/partial handling |
| 3 | Book/portfolio NPV and available raw Greek buckets | Delivered client-side composition; limitations remain explicit |
| 4 | Run repeatable evaluations and publish an honest baseline | Delivered: runner, 26-case corpus, Windows/Linux checks and real-model/live baseline; failures retained |
| 5 | Improve agent intent, tools and conversation behavior against that baseline | Delivered: bounded intent/catalogues, selectors, refusal and identity-bound clarification; failed/blocked cases retained |
| 6 | Repeat, compare and package the local agent/evaluation process | Delivered: offline verifier/CI, preflight, provenance-aware comparison, private timing, metrics and independent 0.1.7 release |

The previous architecture-first sequence in `e2b59a301e` is superseded for
current work. Its reporting/query migrations and command stages are retained
as proposals F1–F10 in the future-PR backlog, not prerequisites to active Stage
4. Earlier validation records keep their historical stage numbers. The original
infrastructure checkpoints are also historical, not this active sequence.

Stages 0–3 below describe delivered behavior and past validation. Their records
of preparing datasets or triggering validation runs are historical evidence,
not permission to repeat those writes under the current agent-only scope.

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
Keep this tested client-side composition during the agent/evaluation phase.
Future proposals F1–F3 describe eventual domain ownership, subject to review;
they are not required to run evaluations now. Retain additive contracts, exact
decimal handling, complete populations and double-counting checks.
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

## Stage 4 — evaluation runner and honest baseline

Delivered in `ebb7fe7849`: see [usage](evaluation.md) and
[baseline evidence and limitations](evaluation-baseline.md). All 85 tests pass
on Windows/Linux. Handling checks passed 14/21 with the model/fixtures and
13/18 with existing live reads; failed checks and blocked capabilities remain
visible, including a known booking-refusal oracle false negative. Runtime 0.1.5
and all backend services remain unchanged. The following describes the
delivered evaluation contract, not a claim that every business task passes.

Build a runnable agent-side evaluation command and versioned case corpus using
the installed model and existing tool/backend contracts. Cover the business
questions already discussed: discovery, market points, stored trade NPV, scope
NPV, raw Greeks, ambiguous choices, multi-turn continuation, missing/stale data
and unsupported requests. Add malformed/untrusted tool text, identity/scope
changes, timeout/cancellation and inference failures. Command/booking intents
are refusal or labelled fixture cases; the harness cannot execute mutations.

Start with deterministic fixture/replay runs for reproducible regression checks,
then an explicit opt-in local-model/live-read profile against existing data.
Live checks must not create reports, accounts, trades, feeds or schemas. Record
the actual date, selected scope/run and staleness policy; historical expected
values remain labelled historical. Missing infrastructure/data may prevent a
case, but must not prevent the rest of the corpus from producing a report.

Every case records expected intent/tool/arguments or expected clarification,
refusal/unsupported behavior; actual tool trace and answer; numerical/date/
currency/provenance checks where applicable; latency and error details with
secrets removed. Pin model/GGUF hash, quantization, llama.cpp version and
settings, prompt, tool schemas, agent revision, backend contract and data/run
identifiers. A failed case's cause can remain unclassified until inspected;
do not automatically blame the backend for a model mistake.

Report distinct outcomes and reasons: pass, behavioral/quality failure,
blocked by missing backend capability, blocked by missing data/identity, and
execution/harness error. Show all counts and denominators; skipped/blocked cases
are never silently counted as passed. Separate **assistant handling correctness**
from **business capability fulfillment**: correctly saying vega is unavailable
can pass a handling test while the positive live-vega use case remains blocked.

Acceptance: one documented command produces machine-readable results and a
readable summary even if most quality cases fail. Runner completion means
the report is complete, not that the agent is accurate. Deterministic unit/
contract and harness-integrity checks must pass; initial model/business scores
are observations, not a release threshold. Normal evaluation mode completes
successfully when it writes a valid report with failures; an optional strict
mode can enforce explicitly chosen thresholds. Failure to initialise the harness
or write a valid report is an error; per-case execution errors are recorded and
the remaining cases continue. Baseline collection never depends on backend changes.

## Stage 5 — agent-only improvements and conversational workflows

Delivered in 0.1.6; see [workflow boundaries](intent-workflows.md) and
[comparison/rollout evidence](stage5-validation.md). All 94 tests pass on
Windows/Linux with identical artifacts. Final original-corpus handling was
42/42 model/fixtures and 33/36 live, with two repetitions.
Original graders/cases and model settings are unchanged; failures and capability
gaps remain visible. The following records the delivered stage contract.

Use Stage 4 evidence to prioritise improvements within the agent: tool
descriptions/catalogue selection, typed intent extraction, clarification states,
literal argument validation, deterministic rendering, bounded context/results,
retry/deadline handling for reads and model request parameters. Introduce an
explicit workflow layer incrementally where it improves measured behavior;
do not replace working routing or adopt a framework simply for architectural
appearance. Keep one bounded assistant and the shared agent/MCP executor.

Keep conversation state session-scoped and expiring. The model proposes a
request; code enforces legal transitions, permissions context and unrequested
selector/action checks. Continue using current deterministic composition;
future backend extraction is not part of this stage. Preserve financial units,
coverage and missing-not-zero rules, and do not infer unavailable facts.

Compare each change with a frozen baseline and held-out paraphrases, retaining
failed/blocked cases and traces. Separate changed prompts/settings from model
replacement; the existing LFM3 deployment is the baseline. Repeat stochastic
cases enough to reveal variance and record seeds/settings when supported.
Fixtures establish fixture behavior, not positive live IR/vega/booking support.

Acceptance: runnable before/after results explain improvements, regressions and
remaining failures. No requirement to make every evaluation pass or fix blocked
backend/data capabilities. Deterministic safeguards and unit/contract tests
continue to pass; quality failures remain visible and are not hidden by changed
denominators or weakened expectations. Findings inform discussion with the
project lead rather than silently expanding this work into server changes.

## Stage 6 — repeatable local evaluation and agent release

Delivered in 0.1.7; see [reproduction commands](reproducible-evaluation.md) and
[full validation/remaining failures](stage6-validation.md). All 101 tests pass on
Windows/Linux with identical artifacts. Model handling is 42/42 and live handling
32/36 over two repetitions; strict comparison retains the continuation regression
against Stage 5. Separate real-model paraphrase profiles pass 8/8 each. No
backend/broker changes or relaxed quality checks were used. The following
records the delivered stage contract.

Provide documented fixture/replay, local-model and opt-in live-read commands;
run selection, repeat counts, report locations and comparison behavior must be
clear. Keep private data, credentials and raw traces outside source control;
commit only safe cases/artifacts and reproducible summaries as appropriate.
CI can run deterministic agent/harness checks without a live OreStudio server;
GPU/backend-dependent runs are explicit profiles, with missing prerequisites
reported instead of misrepresented as passing tests.

Add agent-side correlation/timing for inference, selection, tool/adapter calls
and rendering, with redacted errors. Report task/tool/argument accuracy,
clarification/refusal behavior, numeric attribution, truncation, failures and
sample sizes. Use p50/p95 only with adequate samples; individual smoke timings
are not benchmarks. No server instrumentation or broker changes are included.

Package and deploy only the standalone agent when runtime code changes, with
private config preservation, rollback and appropriate Windows/Linux checks.
The existing backend and model artifacts remain reusable. Native Linux NVIDIA,
least-privilege/hosted/broker isolation gaps stay explicit; they are not silently
required to collect a local development evaluation baseline.

Acceptance: another developer can run the selected available profile, inspect
failures and reproduce a comparison without changing OreStudio services. The
local read-only assistant remains usable with documented limitations even if
most quality evaluations fail. Report capability gaps and future PR candidates
separately from the current agent release; no production-readiness or autonomous
command claim follows from having an evaluation process.

## Future architecture and project-lead handoff

See [future architecture and backend PR proposals](architecture-future-prs.md)
for F1–F10, component ownership, query/aggregation migration, packaging/lifecycle
bugs, hosted security, report execution, booking and market/quote/workflow gaps.
These are discussion items and separate future PRs, not active tasks to unblock
Stage 4. Use evaluation traces to explain each gap's business impact and desired
contract; the project lead decides the backend work and rollout sequence.

## Delivery discipline

Keep deterministic component/contract and harness-integrity tests passing.
Publish model-quality scores and capability/data gaps honestly; a low score is
a valid baseline and does not require a server repair. Do not fabricate data,
remove difficult cases or weaken identity/provenance/coverage checks to improve
scores. Record unsupported cases with expected behavior and business gaps
separately. Use existing authenticated reads for live checks and labelled
fixtures where real prerequisites are absent.

Commit/push reviewable agent/eval checkpoints on the existing fork branch when
validated. Keep backend proposals in documentation until separately agreed;
neither a failing eval nor this plan authorises modifying reporting/main-server/
ORE code, schemas, broker configuration or deployments. Before a future PR,
review the historical backend delta separately as recorded in the handoff.
Do not open a PR without an explicit request in that turn.

## Immediate next checkpoint

The active **local agent/evaluation Stages 0–6 are delivered** with recorded
failures and data/capability limits. Review the continuation measure-selection
finding for a further agent-only iteration and take the architecture/data/unit
gaps to the project lead. Backend work remains a separate future PR decision;
no reporting/server/ORE changes, data provisioning or hosted rollout is approved
by completion of this local process.

## Design references

The future ownership direction is an architectural proposal, not a prerequisite
for using/evaluating the agent now. The workflow/tool/evaluation direction is
informed by [Building Effective Agents](https://www.anthropic.com/research/building-effective-agents)
and [Writing effective tools for agents](https://www.anthropic.com/engineering/writing-tools-for-agents).
