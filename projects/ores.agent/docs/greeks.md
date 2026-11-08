# Stored book and portfolio Greeks

Use `summarize_scope_results` with `greeks`, `delta`, `gamma`, `vega` or `dv01`.
Supply a literal book/portfolio and explicit run ID. Membership and ownership
follow [scope reporting](scope-reporting.md). No report execution or repricing is exposed.

Examples:

```text
Show Greeks for portfolio "Group Treasury Portfolio" from run <RUN_UUID>
Show delta for book "Group FX Hedges" from run <RUN_UUID> for factor DiscountCurve/GBP/0/1Y
Show gamma for book "Group FX Hedges" from run <RUN_UUID> for factor DiscountCurve/GBP/0/1Y
Show dv01 for book "Group FX Hedges" from run <RUN_UUID> for factor DiscountCurve/GBP/0/1Y
```

## Meaning and units

Accepted rows are raw `sensitivity_report` metrics from `sensitivity.csv`, with
`IsPar=false`. The tool checks identity, date, source, currency and shift metadata.
Invalid or missing dimensions withhold figures.

ORE Delta/Gamma are cash finite differences at stated target shifts. They have
not been divided by those shifts. The metric API does not state the exact
forward/central/backward stencil. No normalization, FX conversion, sign reversal
or gamma half-factor is applied.

| Selector | Meaning |
| --- | --- |
| `delta` | Single-factor raw Delta; excludes cross-gamma rows' Delta columns. |
| `gamma` | Gamma; diagonal and each cross-factor group remain separate. |
| `vega` | Volatility-factor Delta: cash for the stated bump, not per-unit vega. |
| `dv01` | Signed rate Delta at exactly `0.0001` target shift for DiscountCurve/YieldCurve/IndexCurve. No sign reversal or rescaling. |

Each group keeps measure, factors, exact shifts, scenario, currency, source,
family and raw/par status. Different groups are never summed together.
Decimal normalization preserves distinct shifts. Exact sums cannot recover
precision lost in source formatting or double storage.

A full total needs one figure for every historical member. Missing, suppressed
or duplicate rows withhold that group's total. Omitted engine rows do not prove
zero. A complete group can return a total while another remains incomplete.
There is no grand total across risk factors.

## Pages and verification

`factor`, `factor2` and `scenario` use literal user selectors. Bucket pages default
to five (maximum ten). Contributor pages default to ten (maximum 25). Totals
still cover the whole checked scope. Scan, deadline and output limits follow
scope reporting.

```text
node scripts/inspect-scope-reference.mjs <CONFIG> <CREDENTIALS> <PARTY> <RUN> <PRIVATE_RAW_JSON> greeks
python scripts/reconcile-greek-reference.py <PRIVATE_RAW_JSON> <ACTUAL_SENSITIVITY_CSV> <PRIVATE_REFERENCE_JSON>
node scripts/smoke-greeks.mjs <AGENT_URL> <CREDENTIALS> <PARTY> <PRIVATE_REFERENCE_JSON> <OTHER_PARTY> <ERRORED_RUN>
```

The independent reference checks every metric against its real CSV row and sums
with `Decimal`. Engine reference: `3d75a69087911a7e2cf1e6882da7bc27135c0762`,
`sensitivitycube.cpp`, `sensitivitycubestream.cpp` and `pricingreportwriter.cpp`.
See [historical dataset](greek-validation.md) and [validation](validation.md).
Positive live vega and per-unit Greeks remain unverified.
