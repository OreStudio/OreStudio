# Stored book and portfolio Greeks

Version 0.1.5 extends `summarize_scope_results` with stored raw sensitivity
buckets. This implementation checkpoint passes 76 Windows tests. Final deployed
and Linux evidence will be recorded in [validation](validation.md).

Use `measure: greeks`, `delta`, `gamma`, `vega` or `dv01`, the literal book or
portfolio name/UUID, and an explicitly chosen run UUID. Historical booking
membership, selected-party ownership, clean-run health, actual pricing date,
microsecond timestamps and stale-data labeling follow [scope reporting](scope-reporting.md).
There is no report execution or repricing tool.

Only typed `sensitivity_report` metrics from `sensitivity.csv` are accepted.
The tool reads `Delta` and `Gamma`, not trade-currency duplicates, par results,
scenario P&L, analytical Greeks from unrelated families or NPV replacements.
It verifies tenant, party, run, definition, subject, date, canonical source,
explicit currency agreement and `IsPar=false` for every row. Missing/invalid
dimensions withhold figures; rounded-to-zero shifts cannot establish units.

## Units and aggregation

ORE's stored Delta/Gamma are **cash finite differences at the reported target
shifts**. They have not been divided by the shifts. The stream uses the target
shift sizes; the pricing writer records those beside the cash figures. We do
not invent a forward/central/backward stencil from the output: its exact
stencil is not supplied by this metric API. No derivative normalization,
currency conversion, sign reversal or gamma half-factor is applied.

Every bucket keeps its measure, full factor/tenor key, exact decimal target
shift, second factor/shift for cross-gamma, scenario, currency, source, family
and raw/par status. Different dimensions never share a total. Shift text is
normalized losslessly, so distinct decimal shifts cannot collapse through JS
floating-point conversion. Totals use exact decimal sums of the canonical
stored-double renderings; precision lost by CSV formatting/storage cannot be
recovered. Contributor order is absolute amount then trade UUID.

- `delta` reads raw `Delta` for a single factor; cross-gamma rows' Delta column
  is not treated as another delta observation.
- `gamma` reads `Gamma`, keeping diagonal and each two-factor cross bucket
  separate. A missing/null engine gamma is unavailable, not zero.
- `vega` selects Delta on a volatility-factor key. It is **cash for the stated
  volatility bump**, not per unit or automatically per volatility point.
- `dv01` selects raw DiscountCurve/YieldCurve/IndexCurve Delta at an exact
  `0.0001` target shift. It is the **signed ORE finite difference for a +1bp
  target bump**, not the negative of that amount or an inferred par-rate DV01.
  Other rate bumps are not silently rescaled.

For every bucket, a full-scope total needs exactly one figure for **every**
historically booked trade. Missing, suppressed or duplicate rows withhold that
bucket's total. ORE can omit zero/small sensitivities, but neither a clean run
nor an absent row proves a specific missing figure is zero. A complete bucket
can return its total while another is explicitly incomplete. There is no
grand total across risk factors. Per-trade failure counts remain unknown/null.

## Selection, pagination and bounds

Optional literal `factor`, `factor2` and `scenario` selectors narrow buckets.
The agent cannot invent selectors absent from the user's question. Omitted
selectors retain separate buckets rather than choosing one arbitrarily.
`bucketOffset`/`bucketLimit` page buckets (default five, maximum ten), while
`offset`/`limit` page each bucket's contributors (default ten, maximum 25).
Each page's total still covers its entire verified scope. Output includes
next offsets and each contributor's metric UUID, engine result UUID and CSV
line. The 2,000-metric scan, 1,000-trade scope, 30-second tool deadline and
32 KiB output limit still apply; request smaller pages when needed.

Examples, substituting a real run UUID:

```text
Show Greeks for portfolio "Group Treasury Portfolio" from run <UUID>
Show delta for book "Group FX Hedges" from run <UUID> for factor DiscountCurve/GBP/0/1Y
Show gamma for book "Group FX Hedges" from run <UUID> for factor DiscountCurve/GBP/0/1Y
Show dv01 for book "Group FX Hedges" from run <UUID> for factor DiscountCurve/GBP/0/1Y
```

## Reproduction and upstream contract

The raw snapshot and actual engine CSV are private local evidence. The Python
reference imports no agent code. It verifies every stored Delta/Gamma against
its real CSV row, including trade, currency, measure, line, factors and shifts,
then independently reconstructs historical scopes and sums using `Decimal`.

```text
node scripts/inspect-scope-reference.mjs <config> <credentials> <party> <run> <private-raw.json> greeks
python scripts/reconcile-greek-reference.py <private-raw.json> <actual-sensitivity.csv> <private-reference.json>
node scripts/smoke-greeks.mjs <agent-url> <credentials> <party> <private-reference.json> <other-party> <errored-run>
```

The pinned native engine source is commit
`3d75a69087911a7e2cf1e6882da7bc27135c0762`. The contracts were checked in
`OREAnalytics/orea/cube/sensitivitycube.cpp`, `engine/sensitivitycubestream.cpp`
and `app/reportwriters/pricingreportwriter.cpp`. The official
[ORE user guide](https://opensourcerisk.org/content/uploads/2026/04/userguide.pdf)
also explains sensitivity.csv's finite-difference units. This feature adds no
backend subject or broker change: it reuses `reporting.v1.report_metrics.list`.
Native Linux GPU execution, hosted auth and broker hardening remain open gates.
