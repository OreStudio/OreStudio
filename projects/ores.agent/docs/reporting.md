# Stage 2: stored single-trade valuation results

Version 0.1.3 adds `find_report_runs`, `read_run_status`, and
`read_trade_results`. They use existing NATS read operations already verified
by Stage 0. No server source/build, broker configuration, pricing call, report
trigger or data import is required. This release does not execute reports.

## Examples

- “What is the NPV of trade 05dcc4f0-4af9-597f-8e20-94c3018c6b4c?”
- “Find report runs for trade 05dcc4f0-4af9-597f-8e20-94c3018c6b4c.”
- “Show the status of report run bdfd79f3-0089-4512-8fb6-fd8a1e783291.”
- “Show the partial NPV of trade 05dcc4f0-4af9-597f-8e20-94c3018c6b4c from run bdfd79f3-0089-4512-8fb6-fd8a1e783291.”

The last example explicitly inspects errored output; it is not a clean
valuation. Exact `Show/Read/Inspect [the] partial npv/npv_base of/for trade
<UUID> from/in run <UUID>` commands are parsed directly into the same validated
tool execution. This avoids probabilistic transcription of two UUIDs and the
opt-in flag. Other reporting questions use local model tool selection. The
model cannot opt into partial output when the question did not request it.
There is no general report-choice conversation memory: supply the run UUID in
the full request. Existing book/provider clarification remains separate.

## Selection and provenance

Trade-result reads validate the exact trade in the signed-in tenant/selected
party before reading results. Backend filters constrain party, trade, subject
type, measure and optional run/currency; returned rows must match those fields.
Only `npv` and `npv_base` are supported in this slice. Results retain the
backend's finite JSON numeric figure and currency, without FX conversion,
aggregation or invented precision. Zero is a value; absence is not zero.

Default eligibility requires clean completed run health, completion metadata
and an actual pricing date. The newest eligible actual pricing date is used;
within one configuration the latest completed run is preferred. Different
report definitions, market cuts or engine settings at that date require a run
choice. Equally recent runs and multiple currencies/duplicate rows require
clarification. Runs are never combined into one value. Only one matching row
can produce an answer. A requested valuation date is matched to the actual
pricing/result date, not the run's completion time.

The result's report-definition ID and pricing as-of must agree with the run.
Health is checked again before returning a value. Failed, running, unknown or
undated runs never yield figures. `allowPartial=true` additionally requires an
explicit run ID and only permits a completed-with-errors run; it does not
permit failed/running output. All partial output is prominently labelled.

Answers render authoritative fields directly from tool evidence: measure,
figure, currency, trade/run IDs, run name, health, actual pricing date, result
as-of, completion time, market cut (or absence), pricing-date adjustment note
and source. The model does not rewrite numerical results. By default a stored
pricing date more than three calendar days old is labelled stale; an explicit
historical valuation date remains visibly dated. No new valuation is implied.

`find_report_runs` without a trade searches selected-party run metadata. With
a trade, it shows only runs linked by stored result rows; it cannot establish
every attempted run's trade population. Incomplete attempts with no stored
rows may therefore be absent. `read_run_status` can inspect a known run UUID
independently. Neither tool implies a run has usable valuation results.

Reads are bounded at 2,000 rows per scan, ten run-list health checks per page,
and twenty candidate-run health checks for result selection. Narrow the run,
date or currency if a bound is reached. Inconsistent pages return no figure.
The thirty-second reporting cancellation deadline retains the existing NATS
request's up-to-eight-second in-flight completion bound. Reads are not claimed
to be one atomic snapshot across mutable backend tables.

## Live data gate

The actual Acme dataset currently supplies NPV rows for the example trade from
Leverage Ratio and Large Exposures runs, both **completed with errors**. Default
requests withhold their figures. Explicit inspection of the Leverage Ratio
run returns a partial row with actual pricing date 2026-11-06 and source
`npv.csv`, including the incomplete/errored warning. This proves the partial
and provenance paths, not a clean valuation scenario.

Clean/stale success, true zero, currency/configuration ambiguity and failure
cases are tested with labelled fixtures, including an independent MCP client.
Live validation additionally uses an actual separate selected-party login to
confirm that the first party's trade result is unavailable there. Role-level
least-privilege and cross-tenant tests remain separate open gates.

Run the existing-demo smoke without creating reports:

```text
node scripts/smoke-reporting.mjs <agent-url> <private-credentials.json> <Acme-Plc-party-id> <optional-other-party-id>
```

The Stage 2 code slice can be delivered independently, but its clean live NPV
acceptance gate remains open until one approved valuation run is prepared or
repaired and reconciled against the report UI/backend. Such work must be
declared separately. This release does not silently fix reports, trigger jobs,
change market data, or implement Greeks/book/portfolio aggregation.
