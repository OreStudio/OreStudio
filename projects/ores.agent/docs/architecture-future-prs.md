# Future server changes for agent use cases

For project-lead review. Updated: 2026-11-08.

**No new server component or service process is proposed.** All components below
already exist. The agent is implemented in this fork. Reuse existing APIs first.
Server additions below are conditional proposals, not prerequisites for agent work.

Server changes need lead approval and separate PRs. Current work remains
[agent and evaluations only](business-use-cases-plan.md).

## Where logic belongs, and why

The boundary is who defines and guarantees a business result. A calculation does
not automatically belong in the server.

| Server responsibility | Agent responsibility |
| --- | --- |
| Enforce access, tenant and party rules. | Interpret questions and ask for missing details. |
| Define stored measures, units, trade terms and booking rules. | Select tools, manage conversation and format replies. |
| Commit bookings and manage durable report/business processes. | Obtain confirmation and display progress. |
| Provide a shared result when clients need the same business guarantee. | Calculate temporary analysis from returned data, with its basis and limits stated. |

Before moving a rule, ask:

1. Do other clients need the same rule?
2. Must the rule hold even if the agent makes a mistake?
3. Does it require a consistent database view or a transaction?
4. Does client-side work cause a measured latency or reliability problem?

First check existing APIs. Shared calculations can use a library if no server
transaction or data guarantee is needed.

Keep current agent aggregation and checks. State the run, dates, units, source
and coverage. Label client-derived totals. Move logic only for an agreed business
guarantee or measured problem. Keep defensive agent checks after any move.

## Existing components and proposed changes

**Existing:** present in source; not necessarily tested live. **Verify:** check
for a gap first. **Keep:** no change proposed. **Optional:** not needed for the current agent.

| Existing component | Existing function | Proposed change |
| --- | --- | --- |
| `ores.agent` | Read-only chat/MCP, clarification, summaries, evaluations and separate releases. | **Keep** reads/analysis. Add approved action tools. F1–F3 queries are optional. |
| `llama.cpp` | Model inference and proposed tool calls. Separate process. | **Keep.** No new inference server. |
| `ores.reporting` | Report definitions, triggers, status, value rows and sensitivity metrics. | **Optional F1–F3:** shared queries. **Verify F5:** execution blockers. |
| `ores.trading` | Trade terms, historical bookings, transactional booking and version-checked draft execution. | **Reuse F6–F7.** Add only confirmed missing guarantees. |
| `ores.refdata` | Currencies, counterparties, books, portfolios and hierarchy. | **Keep:** use existing reads. Verify required conventions in F6/F8. |
| `ores.marketdata` | Series, observations, fixings, identity resolution and dated reads. | **Data F8:** missing observations. Keep conversational selection in the agent. |
| `ores.analytics` | Pricing-model configuration and product-to-engine mappings. | **Conditional F9:** executable quote contract. Confirm owner and existing support. |
| `ores.ore` | ORE file conversion and report-package preparation. | **Verify F5:** fix the recorded export gap only for affected report paths. |
| ORE engine and `ores.compute` | Price/risk calculations, worker jobs and output collection. | **Keep:** reuse for F5/F9. Verify required product calculations. |
| `ores.workflow` | Stored process steps, progress and recovery. | **Conditional F10:** add a missing business process, not a new workflow engine. |
| `ores.scheduler` / `ores.storage` | Timed jobs / input and output file storage. | **Keep.** No replacement or new API proposed. |
| `ores.iam` / `ores.service` | Accounts, sessions, caller validation and access checks. | **Verify F4:** access controls. Hosted identity only if required. |
| NATS / `ores.nats` | Requests, replies, events and broker access controls. | **Verify F4:** subject permissions. Change grants only where tests identify a gap. |
| `ores.http` | HTTP gateway to NATS operations. | **Keep.** The agent continues to use NATS directly. |
| Main application UI | Manual booking forms and draft capture. | **Optional F6:** review handoff. Agent-side review needs no main UI change. |

Keep the combined agent/MCP application separate, with one tool executor.
Business calls use NATS. Model calls use HTTP. No extra gateway is proposed.

Keep the existing structure: `api` defines messages; `core` holds rules, data
access and handlers; `service` starts the process.

## Proposals reviewed against this boundary

F1–F3 are optional improvements. F4 starts with verification. F5–F10 depend on
the selected business action. None is required for current agent evaluation.

### F1 — One stored trade value

**Question:** “What is the base NPV of trade X from run Y?”

**Exists:** Reporting returns rows and run health. The agent checks dates,
units and sources.

**Proposal — optional:** Add a checked reporting query if other clients need
the same result. Fix missing server metadata at its source. Otherwise, keep
current reads and agent checks.

**Check:** Match an independent client. Reject invalid units and denied access.

### F2 — Book and portfolio totals

**Question:** “What is the NPV of book Group FX Hedges? Show the contributors.”

**Exists:** The agent combines historical bookings, portfolio hierarchy and
stored values from existing APIs.

**Proposal — optional:** Keep agent totals. Add a reporting summary only for a
shared business total, consistent historical result set or measured read cost.
One endpoint alone does not guarantee consistency.

**Check:** Verify membership, coverage, stable pages, exact sums and currency.
Keep booking membership separate from direct portfolio assignments.

### F3 — Greeks

**Question:** “Show GBP one-year Delta for this book and report run.”

**Exists:** Reporting stores metrics. The agent validates and groups available
Delta, Gamma, signed one-basis-point rate Delta and stated-shift vega.

**Proposal — optional:** Keep grouping and analysis in the agent. The server
must describe units and shifts correctly. Add shared risk totals only when
reuse or consistency justifies them.

**Check:** Match CSV results. Preserve factor, shift, scenario, currency and
raw/par groups. These are stored cash differences. Per-unit Greeks need separate
proof. Missing rows are not zero. Incomplete groups have no total.

### F4 — Access and separate deployment

**Question:** “Show the risk for my party's books.”

**Exists:** Sessions, server access checks, NATS TLS, separate agent releases
and timing. Live checks mainly use an administrator.

**Proposal — verify first:** Test low-permission and cross-tenant/party access.
Fix only confirmed gaps. Server access rules must hold even if the agent fails.
Hosted authentication is conditional on remote hosting.

**Check:** Test expiry, denial, failures and rollback. Measure latency. Verify
Windows/Linux GPU use. Exclude secrets from logs.

### F5 — Start a report

**Question:** “Run the approved NPV report for book X at date Y.”

**Exists:** Report trigger/status APIs, packaging, workflow and compute workers.
The agent has no execution tool.

**Proposal — agent integration first:** Reuse the trigger. Server execution
must survive agent failure. Add only missing user-context or outcome guarantees.
Recheck the recorded [export/run-state defects](greek-validation.md) before fixing them.

**Check:** Confirm scope, date and resource use. Resolve timeouts before retries.
Closing chat does not cancel a report.

### F6 — Prepare a booking draft

**Question:** “Buy 20M EUR/USD six-month forward with counterparty X.”

**Exists:** Trading draft capture, product records and manual web booking.
The agent has no booking tools.

**Proposal — agent/UI work first:** Extract terms, ask questions and show a
review. An unsaved draft can stay in the agent. Trading validates saved records.
Extend server validation or UI handoff only for a confirmed product gap.

**Check:** Allow correction or discard. A saved draft must not trigger live processing.

### F7 — Confirm a booking

**Question:** “Book this reviewed draft.”

**Exists:** Transactional `book_trade`; duplicate-ID conflicts; version-checked
`execute_trade`. Basic transactional booking already exists.

**Proposal — reuse first:** Bind agent confirmation to the exact draft version, user and party.
Use existing transactions and read back the trade ID and terms after a timeout.
Add server retry support only if existing reads cannot establish the outcome.
A duplicate-ID conflict alone does not prove success.

**Check:** One confirmation creates one booking. Test edits, repeats, expiry,
revocation, restarts and uncertain outcomes.

### F8 — Add products and market data

**Question:** “Show the six-month GBP rate.” Later: “Prepare an option draft.”

**Exists:** Market/product models and agent FX/rate tools. The evaluated local
data lacks a positive supported IR lookup.

**Proposal — data first:** Supply observations separately. Missing data does not
justify API redesign. Add tools for supported products. Change server validation
or conventions only for confirmed product gaps.

**Check:** Record supported functions and data gaps for each product.

### F9 — Bid and offer prices

**Question:** “Give me a bid and offer for a EUR/USD risk reversal.”

**Exists:** Pricing configuration and engine calculations. No executable quote
flow has been verified for this use case.

**Proposal — required for executable quotes:** Verify existing pricing first.
Add only the missing quote contract; proposed owner: `ores.analytics`.
The server guarantees terms, pricing basis, spreads, limits, ID and expiry.
The agent collects terms and presents results. Stored NPV cannot supply this guarantee.

**Check:** Verify prices, sides, units, expiry and linkage to reviewed booking.

### F10 — Business processes with saved progress

**Question:** “Start the approved funding process and show its progress.”

**Exists:** A workflow engine with saved steps and recovery. A funding or wash
process has not been established here.

**Proposal — conditional business work:** Reuse an existing process or have its
business owner define one. Server workflows must survive agent downtime.
The agent starts work and reads status. Ordinary read-only tool sequences can
stay in the agent.

**Check:** Verify one process ID, recovery, duplicates and supported cancellation.

## Before a server PR

Confirm the gap in the current API and deployment. Each PR needs an owner,
API version, access rules and rollback plan. Declare schema, native-build and
NATS changes. Use other components' APIs, not their SQL tables.

If F1–F3 are approved, compare old and new results. Move one tool at a time.
Remove only the rules covered by the new server guarantee. Keep response and
identity checks. Do not silently replace a failed server guarantee with local
analysis. Model output cannot grant permission or replace user confirmation.

## Historical backend addition excluded from this branch's changes

The reporting addition in `cc917e0d36` and its scope-copy helper were reverted.
No current tool or evaluation calls `reporting.v1.ops.copy_run_scope`.
Earlier preparation used it once; those records remain. Fresh test data needs
separate agreement. The [historical notes](valuation-validation.md) are not setup instructions.

## Decisions for the lead

1. Keep the server unchanged for current agent development and evaluation.
2. Decide whether other clients, data consistency or measured performance justify F1–F3.
3. Select the next business action. Approve only the server gaps that block it.

Source checks: [report operations](../../ores.reporting/modeling/ores.reporting.operations.org),
[booking operations](../../ores.trading/modeling/ores.trading.trade_operations.org),
[web booking](../../ores.web/packages/bff/src/booking.ts),
[agent reads](../src/backend.ts) and [live baseline](business-baseline.md).
