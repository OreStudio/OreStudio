## Why

The engine settings of a report were 19 columns on the run settings row. Reports could not share them, the screens could not document them, and eight of the columns are not in the official ORE documentation. The design merged in PR 3619 makes them a named configuration that a report definition binds in its own slot. This change builds that data model and removes the columns.

## Scope

- Four models under `projects/ores.reporting/modeling/`: the central `engine_settings` row and three parts, `engine_settings_fixings`, `engine_settings_building` and `engine_settings_execution`. They hold the 15 documented settings. A null setting means the engine default, and a missing part row means the default for the whole group.
- The central row binds the profile `uuid-identified-lookup` and sets `generate_touch_function`. Each part binds `fk-scoped-child`, follows `book` as its reference entity and flags its foreign key `bump_parent_version`, so a write to a part makes the next version of the central row. A unique index allows one row of a part for each configuration. The drift check passes.
- Generated SQL, C++ and TypeScript for the four entities.
- The configuration type `engine_settings`, a publish function, an artefact type and an operation, so a dataset can stage a document.
- The 19 engine columns are removed from `risk_report_config`. The currency, the market data kind and the package layout columns stay for their own tasks.
- `run_document_projection` reads each Setup parameter into its part as a typed value and writes it back. A flag reads any spelling that ORE's parseBool accepts and is written as `true` or `false`. A value ORE does not read is refused.
- A Setup parameter that the model does not hold is dropped, and the import result carries one warning for each. The `fixingCutoff` warning adds that the numbers may differ.
- `run_document_service` saves, reads and removes the engine settings of a run document. The export skips the `engine_settings` slot, because it fills no input file.
- The ACME seed stages seven engine settings configurations and binds them by name in the report definitions.

## Decisions to review

- Flags are written as `true` and `false`. A document that used `Y` comes back as `true`. The story records this as a default the owner can change.
- Each ACME report binds the Standard or the Simulation engine settings of its entity. Simulation reports use 4 threads and the others 1.
- The stale CMake file list of `ores.reporting` is regenerated. The unrelated stale list of `ores.marketdata/service` is left alone.

## Not in this change

- Moving the currency and the market data kind to the report definition.
- Recording what a run used.
- The engine settings messaging, service and screens.

## Verification

- `ores.reporting.core.tests` and `ores.ore.core.tests` pass, including the 422-document corpus round trip and the database import and export tests.
- The other `ores.ore` and `ores.reporting` test targets build.
- 11 pgTAP tests in `reporting_engine_settings_publish_test.sql` pass. The risk report config, official reports, report bindings and document publish tests also pass.
- `./compass.sh lint`, `check_model_drift.py` and `validate_schemas.sh` are clean. The schema validation shows only the four warnings that were there before.

🤖 Generated with [Claude Code](https://claude.com/claude-code)
