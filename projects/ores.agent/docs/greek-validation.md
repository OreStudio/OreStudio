# Historical Greek test-data preparation

Historical operator preparation for the 0.1.5 release. This is not an agent
command or current setup procedure. It created new validation records but changed
no native code, schema, broker configuration or main deployment.

Engine source: `3d75a69087911a7e2cf1e6882da7bc27135c0762`.
Input came from clean NPV run `884d0bc9-932d-4c4c-9b66-ee97d2b810bb`: 73 Plc
trades, market date 2026-11-06, conventions, curves and pricing engines.
Historical reads independently established each new run's booking scope.

## Input and attempts

The copied input added sensitivity/simulation configuration for GBP/USD/EUR
curves, GBP-SONIA and USDGBP/EURGBP spots. Curve bumps were central absolute
0.0001 at 1Y/5Y/10Y; FX bumps were central relative 0.01. CSV target shifts were
absolute. Settings: `parSensitivity=N`, threshold -1, one thread and
`continueOnError=false`. No volatility surface or option trade was invented.

A new 921600-byte tar used `pre_processing=substitute` and `post_processing=execute`.
Standard compute and collection produced metrics. Existing runs/settings were
not edited or deleted. The normal exporter lacked simulation/sensitivity export;
that was an execution gap, not a read-tool dependency.

| Attempt | Run | Actual outcome |
| --- | --- | --- |
| Initial prepared input | `1b65c0c7-4fe7-40b6-acb0-13a9aea36f61` | Failed; prepared-input path never moved pending to running, so completion's transition was refused |
| Explicitly started original input | `93a5db47-1d13-4a7c-93f6-132472492a30` | Completed with errors; simulation omitted GBP-SONIA, so sensitivity rebuild of ten Bond trades failed |
| Corrected simulation input | `134b921d-eb6a-4277-964f-13d4b33429f5` | Completed clean; zero recorded errors and one warning |

Initial definition: `f050e486-e84c-4ac9-b438-1036395d4be4`.
Corrected definition: `6160fcaa-9a1f-4041-a236-aa4e506db71a`.
The original failed/errored attempts remain unchanged.

The prepared-input path did not move pending to running. For later new runs,
the operator used version-checked `report_instances.put` before completion,
recording the date from `ore.xml`. No failed run was relabelled clean.
This workaround must not become an automatic agent execution path. Recheck the
recorded lifecycle/export gaps before proposing a future fix.

## Reconciliation

The corrected run stored 510 Delta/Gamma metrics from 255 real CSV rows.
An independent Python `Decimal` reference checked every trade, measure, currency,
factor, shift and source line. It imported no agent arithmetic.

Group FX Hedges had 63 trades. For `DiscountCurve/GBP/0/1Y` at shift `0.0001`,
complete totals were **Delta GBP -6502.44** and **Gamma GBP 0.14**.
The DV01 selector returned that same signed Delta without division or negation.
Other groups were sparse. Missing loan-book contributions did not become zeros
in the parent total. Positive live vega was not established.

Private evidence: ignored `build/output/greek-*` and `agent-greeks-*`.
Prepared archives and operator scripts are not replayable migrations.
Read-only reference commands are in [Greeks](greeks.md); release evidence is in
[validation](validation.md).
