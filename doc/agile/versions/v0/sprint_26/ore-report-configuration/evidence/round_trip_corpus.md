# Round-trip corpus census

The corpus is `external/ore/examples/`, a verbatim sync of the ORE Engine
tree (engine 1.8.17.0, commit `3d75a69087911a7e2cf1e6882da7bc27135c0762`,
synced 2026-09-11). It holds **1623 `.xml`** files.

## Count by kind, not by file name

A round trip covers a *document kind*, so the unit is the set of files
whose root element is that kind's root, found by glob:
`find external/ore/examples -iname "<kind>*.xml"`. Counting files named
exactly `<kind>.xml` undercounts badly, because the corpus holds many
variants of the same kind: `ore_histvar.xml`, `simm_calibration.xml`,
`creditsimulation_*.xml`.

| Kind | Glob | Root element | Files |
|------|------|--------------|-------|
| run document | `ore*.xml` | `<ORE>` | 416 |
| portfolio | `portfolio*.xml` | `<Portfolio>` | 219 |
| simulation | `simulation*.xml` | `<Simulation>` (4 are `<CrossAssetModel>`) | 172 |
| pricing engines | `pricingengine*.xml` | `<PricingEngines>` | 125 |
| netting | `netting*.xml` | `<NettingSetDefinitions>` | 99 |
| today's market | `todaysmarket*.xml` | `<TodaysMarket>` | 98 |
| curve config | `curveconfig*.xml` | `<CurveConfiguration>` | 89 |
| conventions | `conventions*.xml` | `<Conventions>` | 72 |
| sensitivity | `sensitivity*.xml` | `<SensitivityAnalysis>` | 50 |
| stress | `stresstest*.xml` | `<StressTesting>` | 18 |
| credit simulation | `creditsimulation*.xml` | `<CreditSimulation>` | 14 |
| currencies | `currencies*.xml` | `<CurrencyConfig>` | 13 |
| script library | `scriptlibrary*.xml` | `<ScriptLibrary>` | 12 |
| collateral balances | `collateralbalance*.xml` | `<CollateralBalances>` | 10 |
| calendar adjustment | `calendaradjustment*.xml` | `<CalendarAdjustments>` | 5 |
| counterparty | `counterparty*.xml` | `<CounterpartyInformation>` | 4 |
| SIMM calibration | `simm*.xml` | `<SIMMCalibrationData>` | 4 |
| reference data | `referencedata*.xml`, `reference_data*.xml` | `<ReferenceData>` | 14 |
| Basel traffic light | — | `<BaselTrafficLightConfig>` | **0** |
| historical return | — | **no global element** | **0** |

Earlier counts in this file's history came from `-name "<kind>.xml"`,
which reported 82 run documents instead of 416 and 2 credit simulation
files instead of 14. Those numbers are wrong for a round trip.

## Three kinds cannot be round tripped as they stand

All three are included by `external/ore/xsd/input.xsd`, and none of them
has generated C++ types in the checked-out bindings:

- `simmcalibration.xsd` declares the global element `<SIMMCalibrationData>`
  at line 3 and the corpus holds 4 files, but `SIMMCalibrationData`
  appears zero times in
  `projects/ores.ore/core/include/ores.ore.core/domain/domain.hpp`.
- `baselTrafficLightconfig.xsd` declares `<BaselTrafficLightConfig>` at
  line 2, which appears zero times in the same header. The corpus holds
  no file of this kind.
- `historicalreturnconfig.xsd` declares **no global element at all**. It
  defines the type `ReturnType` (a `Type` plus a `Displacement`, keyed)
  and nothing that can be a document root, so there is no
  `historicalreturnconfig.xml` to import. The type is used inside the
  documents that reference it, so this is a section of another document
  and not a file type.

The checked-out bindings are also older than a regeneration that once
landed: `domain.hpp` is 13,506 lines here, while commit `65476f2c23`
produced 16,268, and commit `f46e7a1076` restored the shorter file.
`doc/knowledge/external/xsd-cpp-generator.org:91-93` does not describe
the checked-out tree.

## What the harness can reuse

The xml facade `projects/ores.ore/core/src/xml/roundtrip.cpp` walks a
directory and converts by `document_kind`, but
`document_kind.cpp` recognises only four kinds (`Portfolio`,
`CurrencyConfig`, `CalendarAdjustments`, `Conventions`) and counts every
other file as `unsupported`. Every other kind round trips today only
through the generated `domain::load_data` / `domain::save_data` pairs,
exercised by about twenty-five files
`projects/ores.ore/core/tests/xml_*_roundtrip_tests.cpp` that hard-code
their input paths. The harness extends `document_kind` and the walker;
it does not start from nothing.
