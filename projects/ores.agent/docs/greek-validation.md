# Isolated historical Greek validation dataset

This is operator test-data preparation, not an agent/MCP command capability.
No native C++ code, database schema, broker configuration or main deployment
was changed. The agent continues to use existing read-only NATS operations.

The native engine is the existing pinned ORE build at source
`3d75a69087911a7e2cf1e6882da7bc27135c0762`. Its actual clean NPV input archive
for run `884d0bc9-932d-4c4c-9b66-ee97d2b810bb` supplied all 73 Acme Plc trades,
2026-11-06 market data, conventions, curve configuration and pricing engines.
Those trade IDs were checked against authenticated current NATS reads before
creating the new report. Historical booking reads at each new run's start
independently establish the scope used by the Greek tool.

## Inputs and execution

The operator copied the immutable package locally and added a sensitivity
analytic, simulation market and sensitivity configuration. The simulation
market includes GBP, USD and EUR curves, GBP-SONIA and USDGBP/EURGBP spots.
Discount curves use central 0.0001 absolute rate bumps at 1Y/5Y/10Y; FX spots
use central 0.01 relative bumps. The CSV records the resulting **absolute
target shifts**. `parSensitivity=N`, `outputSensitivityThreshold=-1`, one
engine thread and `continueOnError=false` were set. No volatility surface or
option trade was invented; this dataset does not prove positive live vega.

The prepared tar is 921600 bytes, UTF-8-preserving including its tar headers,
and fits the existing NATS object put limit. It was uploaded through
`storage.v1.objects.put` under a fresh key and used by an unscheduled,
non-official report with `pre_processing=substitute`, `post_processing=execute`.
The standard compute worker fetched that input and uploaded the actual engine
results; standard collection wrote typed metrics and canonical log events.
Existing source definitions, configurations, queues and old results were not
edited or deleted. Only fresh operator validation definitions/runs were written.

This mode was chosen because the normal run-configuration exporter currently
supports only conventions, curve configuration, today's market and pricing
engines. It cannot yet export the simulation/sensitivity configurations needed
for an automatically packaged sensitivity report. Extending that generator is
a future **report-execution** prerequisite, not an agent Greek-read dependency.

## Recorded attempts and lifecycle limitation

| Attempt | Run | Actual outcome |
| --- | --- | --- |
| Initial prepared input | `1b65c0c7-4fe7-40b6-acb0-13a9aea36f61` | Failed; prepared-input path never moved pending to running, so completion's transition was refused |
| Explicitly started original input | `93a5db47-1d13-4a7c-93f6-132472492a30` | Completed with errors; simulation omitted GBP-SONIA, so sensitivity rebuild of ten Bond trades failed |
| Corrected simulation input | `134b921d-eb6a-4277-964f-13d4b33429f5` | Completed clean; zero recorded errors and one warning |

The initial definition is `f050e486-e84c-4ac9-b438-1036395d4be4`. The corrected
definition is `6160fcaa-9a1f-4041-a236-aa4e506db71a`. It has a new archive/key;
the original failed/errored input and runs remain unchanged. No run was
relabelled clean or repaired after finishing, and no output was fabricated.

The existing `resolve_prepared_input` step names the archive but does not
perform the `pending -> running` transition that the normal gathering path
performs. For each later **new** run, the operator used the existing
`report_instances.put` API with `must_match_version` to explicitly start it
and record the historical pricing date verified in the immutable `ore.xml`.
This was a real pending-to-running transition, before completion. The failed
run was never changed. The pricing note identifies operator-prepared historical
input. The automatic prepared-input lifecycle bug remains open and must be
fixed before exposing that execution path through an agent; no native build
or temporary reporting instance was ultimately needed for this read stage.

## Reconciliation

The corrected run stores 510 Delta/Gamma metrics from 255 actual sensitivity
CSV rows. The independent Python reference joins every metric to its engine
CSV line, checks trade/measure/currency/factor/shift identity, and sums with
`Decimal` without importing agent arithmetic. It reconstructs three nonempty
historical scopes from books, portfolios, trades and bookings.

For Group FX Hedges (63 trades), raw factor `DiscountCurve/GBP/0/1Y` at a
0.0001 target bump has complete coverage: **Delta GBP -6502.44** and
**Gamma GBP 0.14**. The DV01 selector returns that same signed engine Delta;
it does not negate it or divide it by 0.0001. Other factor buckets have sparse
coverage and therefore no full-scope total. The loans book and 73-trade parent
cannot acquire fabricated zero contributions from their missing rows.

Local operator scripts, prepared archives, raw snapshots and CSV evidence
remain private under ignored `build/output/greek-*` and `agent-greeks-*` paths.
They checkpoint writes and are not blindly replayable migrations. The public
read-only inspection, independent reconciliation and deployed smoke scripts
are listed in [Greek usage](greeks.md). Final application rollout evidence is
recorded in [validation](validation.md).
