# Future server changes for agent use cases

For project-lead review. Updated: 2026-11-08.

These proposals need the lead's approval and separate PRs. Current work covers
only the agent and its evaluations. Keep working tools and checks. Record missing
APIs and data as evaluation gaps. See the [current plan](business-use-cases-plan.md).

## Component responsibilities

Use existing C++ components where possible. These are proposed responsibilities.
Some required APIs do not exist yet.

| Component | Function |
| --- | --- |
| `ores.agent` | Interpret questions. Ask for missing details. Call tools. Format replies. Obtain user confirmation. |
| `llama.cpp` | Run the model. Propose text and tool calls. No direct business-data access. |
| `ores.reporting` | Select stored runs. Return values, totals, Greeks, coverage and sources. Own report commands and status. |
| `ores.trading` | Own trade terms, bookings and historical trade-to-book membership. Validate drafts. Commit complete bookings. |
| `ores.refdata` | Own currencies, counterparties, books, portfolios, their hierarchy and conventions. |
| `ores.marketdata` | Own series, observations, fixings, sources and dates. Select points. Identify stale data. |
| `ores.analytics` | Own pricing-model configuration and product-to-engine mappings. Proposed owner of a future pricing API; the lead must confirm this. |
| `ores.ore` | Convert ORE files. Prepare report packages. Separate from the risk engine. |
| ORE engine and `ores.compute` | ORE calculates prices and risk. Compute assigns jobs, runs workers and collects outputs. |
| `ores.workflow` | Store process steps and progress. Run defined recovery steps. Business components define process rules. |
| `ores.scheduler` | Start approved jobs at set times. |
| `ores.storage` | Store input packages and output files. |
| `ores.iam` and `ores.service` | IAM owns accounts and sessions. Shared code validates caller context. Business services enforce access rules. |
| NATS and `ores.nats` | Carry requests, replies and events. Broker rules restrict subject access. |
| `ores.http` | Map HTTP requests to NATS operations. The agent does not use this path. |
| Main application UI | Let users inspect and correct terms. Show the exact draft or command for confirmation. |

For components with `api`, `core` and `service` parts:

- Put request and response types in `api`.
- Put business rules, data access and request handlers in `core`.
- Put process startup and handler registration in `service`.

Keep the combined agent/MCP application separate from the server. Both agent
and MCP calls use one internal tool executor. Use NATS for business calls.
Keep model inference on its separate HTTP connection. No extra MCP gateway
or new query service is proposed.

## Proposed PRs and example questions

These example questions explain the need. They do not claim current support.
The lead can split or combine F1–F10.

### F1 — One stored trade value

**Question:** “What is the base NPV of trade X from run Y?”

Reporting returns the amount, currency, run, pricing date, source and status.
Use versioned request types and decimal-string amounts. All clients get the
same server-checked value.

**Check:** An independent client gets the same result. Invalid units, denied
access and unsupported API versions produce clear errors.

### F2 — Book and portfolio totals

**Question:** “What is the NPV of book Group FX Hedges? Show the contributors.”

Reporting gets historical membership from trading and portfolio hierarchy from
reference data. It checks coverage and adds stored values. This replaces several
agent reads. Keep booking membership separate from direct portfolio assignments.

**Check:** Verify totals and contributors. Missing or duplicate trades prevent a
complete total. Keep one result version across pages. Do not combine currencies
without a defined conversion.

### F3 — Greeks

**Question:** “Show GBP one-year Delta for this book and report run.”

Move sensitivity checks and totals to reporting. Keep factors, shifts, scenarios,
currencies and raw/par results separate. Define signed one-basis-point rate Delta
and vega at its stated volatility shift. These remain stored cash differences,
not per-unit derivatives. Missing results do not mean zero. One server definition
keeps these rules the same for all clients.

**Check:** Compare with stored CSV results. Incomplete groups have no full total.
Do not claim per-unit Greeks or live vega without the required data and checks.

### F4 — Access and separate deployment

**Question:** “Show the risk for my party's books.”

IAM, shared service code and each business service must preserve the user's
permissions. Restrict NATS subjects. Hosted MCP needs separate authentication
and TLS controls. Keep the agent independently deployable on Windows and Linux.

**Check:** Test access, expiry, logout and tenant/party separation. Measure local
and LAN latency. Test cancellation, failures, rollback and Windows/Linux GPU use.
Exclude secrets from timing and audit records.

### F5 — Start a report

**Question:** “Run the approved NPV report for book X at date Y.”

Reporting validates the request and owns run status. `ores.ore` prepares the
package. Workflow, compute and the engine execute it. First fix the known
[package-export and run-state defects](greek-validation.md).

**Check:** Confirm the definition, scope, date and resource use before submission.
Create one durable run. After a timeout, check its status before retrying.
Closing chat must not claim to cancel a running report.

### F6 — Prepare a booking draft

**Question:** “Buy 20M EUR/USD six-month forward with counterparty X.”

The agent asks for missing terms. Trading validates the draft with reference
and market data. The main UI shows every material term for review. Use a
supported review handoff. A saved draft must not trigger a booking.

**Check:** The user can correct or discard the draft. No trade is booked.
Do not invent rates, dates, settlement terms or counterparties.

### F7 — Confirm a booking

**Question:** “Book this reviewed draft.”

Trading commits one complete booking. Bind confirmation to the draft version,
user and party. Edits cancel confirmation. Use a request key so repeated
submissions return the original booking.

**Check:** One confirmation creates one booking. Test repeated clicks, expired
approval, revoked access, agent restart and timeout after the server commits.
The agent cannot enable this tool through separate record-write operations.

### F8 — Add products and market data

**Question:** “Show the six-month GBP rate.” Later: “Prepare an option draft.”

Market data needs a supported series, date, units and source. Reference data
provides conventions. Trading validates each product. Add one product at a time.
Missing rate or volatility data needs a separate data task.

**Check:** State which products support lookup, validation, pricing and booking.
Keep test data clearly labelled. Report missing or stale data.

### F9 — Bid and offer prices

**Question:** “Give me a bid and offer for a EUR/USD risk reversal.”

Propose `ores.analytics` for the pricing API. Confirm this with the lead. The API
needs engine pricing, market volatility data, spread rules and limits. Reporting's stored NPV query
cannot supply an executable quote. The agent asks for missing terms and presents results.

**Check:** Return the exact terms, sides, units, market snapshot, expiry and quote
ID. Reject expired quotes. Link any execution to that quote and reviewed booking.

### F10 — Business processes with saved progress

**Question:** “Start the approved funding process and show its progress.”

The business component defines allowed actions and approvals. Workflow stores
steps, progress and recovery state. The agent starts a defined command and
reads status. It must not generate server step sequences, SQL or shell commands.

**Check:** One request creates one process ID. Test partial failure, repeated
submission, agent downtime, recovery and supported cancellation.

## Rules for server changes

Each PR needs an owner, API version, access rules and rollback plan. Declare
schema, native-build and NATS changes. Use other components' APIs, not their SQL
tables. Model output cannot grant permission or replace user confirmation.

For F1–F3, compare server queries with current agent calculations. Switch one
tool at a time after values and failure cases agree. Remove duplicate financial
rules from that tool. Keep response and identity checks. A server error must
not silently activate old calculations.

## Historical backend addition excluded from this branch's changes

The reporting addition in `cc917e0d36` and its scope-copy helper were reverted.
They remain in Git history but are absent from the branch's final changes.
No current tool or evaluation calls `reporting.v1.ops.copy_run_scope`.

Earlier local data preparation used that operation once. Existing records remain.
A fresh environment needs separately agreed data preparation or a revised test
corpus. The [historical notes](valuation-validation.md) are not setup instructions.

## Decisions for the lead

1. Confirm the component responsibilities, including the pricing API owner.
2. Select the first server query. Proposed start: F1, then F2 and F3.
3. Agree the data and access work needed before reports, bookings or quotes.
