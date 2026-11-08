# Future server changes for agent use cases

For project-lead review. Updated: 2026-11-08.

**No new server component or service process is proposed.** All components below
already exist. The agent is implemented in this fork. Proposed work adds APIs,
moves rules or connects existing functions.

Server changes need lead approval and separate PRs. Current work remains
[agent and evaluations only](business-use-cases-plan.md).

## Existing components and proposed changes

**Existing:** present in this source; not necessarily tested live.
**Verify:** check for a gap before changing code.
**Keep:** no component change proposed.

| Existing component | Existing function | Proposed change |
| --- | --- | --- |
| `ores.agent` | Read-only chat, MCP tools, clarification, financial summaries and evaluations. Separate deployment. | **Change:** use new server queries in F1–F3. Add reviewed command tools in F5–F7 and F10. |
| `llama.cpp` | Model inference and proposed tool calls. Separate process. | **Keep.** No new inference server. |
| `ores.reporting` | Report definitions, run triggers, status, stored value rows and sensitivity rows. | **Change F1–F3:** add checked value and risk queries. **Verify/fix F5:** report execution gaps. |
| `ores.trading` | Trade terms, historical bookings, transactional `book_trade`, draft capture and version-checked `execute_trade`. | **Verify F6–F7:** reuse these operations. Add only missing draft-validation or retry guarantees. |
| `ores.refdata` | Currencies, counterparties, books, portfolios and hierarchy. | **Keep:** use existing reads. Verify required conventions in F6/F8. |
| `ores.marketdata` | Series, observations, fixings, identity resolution and dated reads. | **Data F8:** provide missing rate/volatility data. **Verify:** existing APIs before adding point-selection rules. |
| `ores.analytics` | Pricing-model configuration and product-to-engine mappings. | **Change F9:** proposed owner of a new bid/offer quote API. Lead to confirm. |
| `ores.ore` | ORE file conversion and report-package preparation. | **Verify/fix F5:** missing sensitivity configuration export found in earlier validation. |
| ORE engine and `ores.compute` | Price/risk calculations, worker jobs and output collection. | **Keep:** reuse for F5/F9. Verify required product calculations. |
| `ores.workflow` | Stored process steps, progress and recovery. | **Change F10:** add a business process definition. Reuse the engine. Verify report steps in F5. |
| `ores.scheduler` / `ores.storage` | Timed jobs / input and output file storage. | **Keep.** No replacement or new API proposed. |
| `ores.iam` / `ores.service` | Accounts, sessions, caller validation and access checks. | **Verify F4:** existing access controls. Add hosted identity support only if hosting is required. |
| NATS / `ores.nats` | Requests, replies, events and broker access controls. | **Verify F4:** subject permissions. Change grants only where tests identify a gap. |
| `ores.http` | HTTP gateway to NATS operations. | **Keep.** The agent continues to use NATS directly. |
| Main application UI | Manual booking forms and draft capture. | **Change F6:** connect agent-prepared terms to review. Verify product support before extending forms. |

Keep the combined agent/MCP application separate, with one tool executor.
Business calls use NATS. Model calls use HTTP. No extra gateway is proposed.

Keep the existing structure: `api` defines messages; `core` holds rules, data
access and handlers; `service` starts the process.

## Proposed work by natural-language question

Each section separates current support from new work. These are review proposals,
not approved implementation stages.

### F1 — One stored trade value

**Question:** “What is the base NPV of trade X from run Y?”

**Exists:** Reporting provides result rows and run health. The agent reads these
and checks dates, units and sources. Some invalid data is withheld.

**New:** Add one reporting query that performs these checks and returns a typed
value with decimal-string amount, currency, run, pricing date, source and status.
Move these rules from the agent so other clients get the same checked result.

**Check:** Match an independent client. Reject invalid units and denied access.

### F2 — Book and portfolio totals

**Question:** “What is the NPV of book Group FX Hedges? Show the contributors.”

**Exists:** Trading provides historical bookings. Reference data provides the
hierarchy. Reporting provides stored values. The agent combines them into totals.

**New:** Add a reporting query for membership, coverage, totals and contributors.
Reuse the existing domain reads. Move aggregation from the agent to reporting.
Keep historical booking membership separate from direct portfolio assignments.

**Check:** Match contributors and totals. Require complete coverage, stable pages
and one currency.

### F3 — Greeks

**Question:** “Show GBP one-year Delta for this book and report run.”

**Exists:** Reporting stores sensitivity metrics. The agent checks and groups
available Delta, Gamma, signed one-basis-point rate Delta and stated-shift vega.

**New:** Add a reporting risk query. Move grouping, coverage checks, exact sums
and contributor ranking from the agent. Preserve factors, shifts, scenarios,
currencies and raw/par separation. This gives all clients the same definitions.

**Check:** Match CSV results. Missing rows are not zero. Incomplete groups have
no total. Per-unit Greeks and live vega need separate proof.

### F4 — Access and separate deployment

**Question:** “Show the risk for my party's books.”

**Exists:** IAM sessions, service access checks, NATS TLS, separate agent releases
and request timing. Current live checks mainly use an administrator account.

**New:** Add low-permission and cross-tenant/party tests. Verify broker subject
permissions; change configuration only if needed. Hosted MCP authentication is
new work only if remote hosting is selected. Separate deployment already works.

**Check:** Test access, failures and rollback. Measure latency and Windows/Linux
GPU use. Exclude secrets from logs.

### F5 — Start a report

**Question:** “Run the approved NPV report for book X at date Y.”

**Exists:** Reporting has `trigger_report_instance` and run status. ORE packaging,
workflow execution and compute workers exist. The agent has no execution tool.

**New:** Add an agent tool for reviewed report submission. Verify that the existing
trigger supports user context and safe retries. Earlier validation found package
export and pending-to-running defects; fix them if still present. See the
[record](greek-validation.md). No new report engine is proposed.

**Check:** Confirm scope, date and resource use. Resolve timeouts before retries.
Closing chat does not cancel a report.

### F6 — Prepare a booking draft

**Question:** “Buy 20M EUR/USD six-month forward with counterparty X.”

**Exists:** Trading supports draft capture and product records. The main web
application has manual booking/draft support. The agent has no booking tools.

**New:** Extract terms in the agent, ask for missing details and open a review
flow. Reuse trading operations. Verify the chosen product's validation and UI
support before proposing extra server APIs or form changes.

**Check:** Allow correction or discard. A saved server draft must not trigger
live processing.

### F7 — Confirm a booking

**Question:** “Book this reviewed draft.”

**Exists:** `book_trade` writes related records in one transaction. A repeated
trade ID returns a conflict. `execute_trade` takes a draft live and checks its
version. Basic transactional booking is not new work.

**New:** Connect explicit user confirmation to these operations. Bind it to the
exact draft version, user and party. Add durable retry/result handling only where
existing operations are insufficient. A duplicate-ID conflict alone does not
return the original successful booking result.

**Check:** One confirmation creates one booking. Test changed drafts, revoked
approval, repeats, restarts and uncertain outcomes.

### F8 — Add products and market data

**Question:** “Show the six-month GBP rate.” Later: “Prepare an option draft.”

**Exists:** Market-data and product models. The agent has FX spot and basic rate
lookup tools. The evaluated local data lacks a positive supported IR lookup.

**New:** Supply required data through a separate data task. Verify product support
before adding agent tools. Extend trading validation, conventions or forms only
for confirmed gaps. Existing models do not prove a complete booking flow.

**Check:** Record each product's supported functions. Label test data and gaps.

### F9 — Bid and offer prices

**Question:** “Give me a bid and offer for a EUR/USD risk reversal.”

**Exists:** Pricing configuration, engine calculations and stored NPV reads.
No executable bid/offer quote flow has been verified for this use case.

**New:** Propose a quote API in `ores.analytics`, subject to lead approval.
It needs product pricing, volatility data, spreads, limits, quote ID and expiry.
The agent asks for terms and displays the quote. Stored NPV is not a quote.

**Check:** Verify sides, units, expiry and linkage to reviewed booking.

### F10 — Business processes with saved progress

**Question:** “Start the approved funding process and show its progress.”

**Exists:** The workflow engine stores steps and supports recovery. This does not
prove that a funding or wash process exists.

**New:** Define one process in its business component. Add workflow steps and an
agent start/status tool. The lead must select the business owner. Reuse the engine;
do not let the model generate SQL, shell commands or server step sequences.

**Check:** Verify one process ID, recovery, duplicates and supported cancellation.

## Before a server PR

Confirm the gap in the current API and deployment. Each PR needs an owner,
API version, access rules and rollback plan. Declare schema, native-build and
NATS changes. Use other components' APIs, not their SQL tables.

For F1–F3, compare old and new results. Switch one tool at a time, then remove
its duplicate financial rules. Keep response and identity checks. Server errors
must not silently activate old calculations. Model output cannot grant permission
or replace user confirmation.

## Historical backend addition excluded from this branch's changes

The reporting addition in `cc917e0d36` and its scope-copy helper were reverted.
No current tool or evaluation calls `reporting.v1.ops.copy_run_scope`.
Earlier preparation used it once; those records remain. Fresh test data needs
separate agreement. The [historical notes](valuation-validation.md) are not setup instructions.

## Decisions for the lead

1. Confirm the changed responsibilities in the table; retain the existing components.
2. Select the first server query. Proposed start: F1, then F2 and F3.
3. Confirm gaps before approving booking, report, market-data or quote work.

Source checks: [report operations](../../ores.reporting/modeling/ores.reporting.operations.org),
[booking operations](../../ores.trading/modeling/ores.trading.trade_operations.org),
[web booking](../../ores.web/packages/bff/src/booking.ts),
[agent reads](../src/backend.ts) and [live baseline](business-baseline.md).
