# Reporting model reconnaissance

Answers obtained from the reporting component, with the evidence each
answer rests on. Measured on `feature/ore-report-configuration` at
`c1c9525781`.

## What exists and what does not

`projects/ores.reporting/modeling/` holds eight entity models:

- `ores.reporting.concurrency_policy.org`
- `ores.reporting.operations.org`
- `ores.reporting.report_definition.org`
- `ores.reporting.report_instance.org`
- `ores.reporting.report_type.org`
- `ores.reporting.risk_report_config.org`
- plus `ores.reporting.module.org` and `component_overview.org`

`report_definition` maps to `ores_reporting_report_definitions_tbl` and
`report_type` to `ores_reporting_report_types_tbl`.

**Five tables of the target model do not exist anywhere**: a search of
`projects/*/modeling` and `projects/ores.sql/create` finds no
`configuration_type`, `configuration`, `configuration_parameter`,
`parameter_definition` or `value_domain`. The registry task starts from
nothing rather than from an edit.

## The report definition as it stands

`projects/ores.sql/create/reporting/reporting_report_definitions_create.sql:39-66`
carries `report_type text not null`, so the type is a bare code beside a
`report_types` lookup that already exists.

**Correction to the classification task's premise.** The task says to
replace the workflow-shaped columns `pre_processing`,
`prepared_input_key`, `post_processing`. Those columns are **not** in the
table and not in the model. They survive only in an untracked build
artifact,
`projects/ores.web/packages/wire-protocol/dist/generated/reporting/domain/report_definition.d.ts`.
The run-shaped columns the table does carry are `fsm_state_id` and
`scheduler_job_id`; `schedule_expression` and `concurrency_policy` are
kept by the target model.

## Foreign keys

The reporting convention is not the `:references:` property used by the
one compute model. It is a `* Foreign keys` section with a block per
column. The only example in the component is
`projects/ores.reporting/modeling/ores.reporting.risk_report_config.org:499-509`:

```
** report_definition_id
:PROPERTIES:
:table:         ores_reporting_report_definitions_tbl
:target_column: id
:parent_entity: report_definition
:nullable:      false
:error_message: Invalid report_definition_id: %. No active report definition found with this id.
:list_by:       true
:END:
```

It generates validation at
`projects/ores.sql/create/reporting/reporting_risk_report_configs_create.sql:136-145`
and indexes at `:110-126`. New reporting tables use this convention.

## Generating and applying SQL

Regeneration for a whole component names an address:

```
./compass.sh codegen regenerate --component reporting --address ores.sql.schema
```

The same command takes `--address ores.cpp`, `--address ores` for the
graph root, or `--all`. Generated create scripts land in
`projects/ores.sql/create/reporting/`, their drop twins in
`projects/ores.sql/drop/reporting/`, and both are aggregated by
`reporting_create.sql` and `reporting_drop.sql`.

There is no migration runner. `projects/ores.sql/modeling/component_overview.org:30-37`
records that the create scripts carry the canonical schema and that the
one-shot `ALTER TABLE` script under `migration/` is applied by hand. A
fresh schema is built with:

```
./compass.sh db recreate --kill --yes
```

## Tests

Domain test: `projects/ores.reporting/api/tests/domain_report_definition_tests.cpp`.
Repository twin:
`projects/ores.reporting/core/tests/repository_report_definition_repository_tests.cpp`.
The ctest target is `ores.reporting.api.tests`:

```
./compass.sh test run -- -R ores.reporting.api.tests
```
