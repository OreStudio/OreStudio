# ORE Studio Project Memory

## Current Branch
`claude/add-reporting-component-LYz0u` (was `feature/reporting_qt_ui`)

## Key Architecture Notes
- C++23, CMake, Qt6, PostgreSQL temporal tables
- Code generation via `projects/ores.codegen/run_generator.sh`
- Profiles: `domain`, `repository`, `qt`
- Models in `projects/ores.codegen/models/{component}/`

## Codegen Template Column Type Flags (generator.py)
When iterating `columns` in mustache templates, `generator.py` sets:
- `is_uuid` — non-nullable UUID (`type: uuid`, `nullable: false`)
- `is_optional_uuid` — nullable UUID (`type: uuid`, `nullable: true`)
- `is_optional_timestamp` — nullable timestamp (`type: timestamp`, `nullable: true`)
- `is_nullable_string` — nullable, non-UUID, non-timestamp column
- `is_simple` — everything else

## Template Files Modified
- `cpp_domain_type_entity.hpp.mustache` — added `is_optional_timestamp` branch
- `cpp_domain_type_mapper.cpp.mustache` — added `is_optional_timestamp` branches (both directions)
- `cpp_qt_client_model.cpp.mustache` — added `is_uuid` branch + `qlonglong` cast for int

## RelativeTimeHelper
Added `format(const std::optional<time_point>&)` overload returning `"N/A"` for nullopt.

## Reporting Qt UI (Sprint 14)
- 4 entities: report_type, concurrency_policy, report_definition, report_instance
- 48 generated Qt files (models, windows, dialogs, controllers)
- `report_instance` has `name` + `description` (copied from definition at creation via SQL trigger)
- Icons: Chart (types), Settings (policies), DocumentTable (definitions), Record (instances)
- Menu: `Reporting` between Trading and System; Scheduler moved from System into Reporting
- Toolbar: `ActionReportDefinitions` + `ActionReportInstances` added

## SQL Schema Pattern
- Temporal tables with `valid_from`/`valid_to`, insert trigger for versioning
- `reporting` prefix: `ores_reporting_report_instances_tbl` etc.
- Schema file: `projects/ores.sql/create/reporting/`

## User Preferences
- No test plan section in PRs
- Git commit titles: `[COMPONENT] Description`
- Icons from FluentUI (`~/Development/fluentui-system-icons-main/assets`) or Solar (`~/Development/solar-icons/packages/core/svgs`)
