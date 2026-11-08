# Read stored trade values

Tools: `find_report_runs`, `read_run_status` and `read_trade_results`.
They read existing results; they do not price trades or start reports.

Examples:

- “Find report runs for trade <TRADE_UUID>.”
- “What is the NPV of trade <TRADE_UUID> from run <RUN_UUID>?”
- “Show the status of report run <RUN_UUID>.”
- “Show the partial NPV of trade <TRADE_UUID> from run <RUN_UUID>.”

Partial inspection needs an explicit request and run ID. It permits only a
completed-with-errors run. It never permits failed or running output.
The model cannot enable partial inspection on its own.

## Selection and checks

Supported measures are `npv` and `npv_base`. The tool first checks the trade's
tenant and selected party. It then checks result identity, measure, currency,
source, definition and pricing date against the run. Exactly one matching row
is required. Runs are never combined into one value. No FX conversion is done.

Default selection needs clean completed health, completion metadata and an
actual pricing date. It uses the newest eligible pricing date. Different
configurations/cuts, duplicate rows or multiple currencies can require a choice.
Supply the exact run ID when needed. Health is checked again before return.

Replies show the stored figure, currency, trade/run IDs, dates, health, market
cut and source. Figures are rendered from evidence. Zero is a value; absence is
not zero. Dates older than three days are labelled stale for undated requests.
An explicit historical date must match the actual pricing/result date.

A trade-filtered run list shows runs with stored rows for that trade. It cannot
prove the population of attempts with no results. A known run can be inspected
with `read_run_status`.

Scans allow 2,000 rows, ten run-list health checks per page and twenty selection
candidates. The tool deadline is 30 seconds. Reads across tables are not atomic.

## Current data limit

The known live trade base-NPV case fails `invalid_result_units` and withholds
the value. Scope base-NPV has a separate explicit currency-metadata join; it
does not change this trade-read result. See [current validation](validation.md).
The removed scope-copy operation is not a runtime requirement. Earlier report
preparation is a [historical record](valuation-validation.md).

```text
node scripts/smoke-reporting.mjs <AGENT_URL> <PRIVATE_CREDENTIALS_JSON> <PARTY_UUID> <OPTIONAL_OTHER_PARTY_UUID>
```

This reads existing records. It does not create a clean report dataset.
