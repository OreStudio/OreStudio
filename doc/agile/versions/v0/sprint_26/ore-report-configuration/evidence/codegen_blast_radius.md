# What one entity costs to generate

Verified at `36066258d0` on `feature/ore-report-configuration`.

The command works in this build (the `codegen entity generate` form in
the docs does not exist):

```
./compass.sh codegen generate --model <path/to/model.org> --address ores --dry-run
```

`--dry-run` writes nothing and lists every path the entity would touch.

For one **analytics** entity
(`projects/ores.analytics/modeling/ores.analytics.pricing_model_config.org`)
that is **38 paths**:

| Area | Files |
|------|-------|
| api domain | 4 headers + 3 sources (`_json_io`, `_table`, `_table_io`) |
| api generator | 2 (faker-based row generator) |
| api protocol, eventing | 2 |
| core repository | 3 headers + 3 sources (entity, mapper, repository) |
| core service | 1 header + 1 source |
| core messaging | handler, registrar, history provider registrar |
| core presentation | history field mapper (header + source) |
| core tests | 1 eventing integration test |
| service messaging | event registrar (header + source) |
| shell | commands header, source and test |
| docs | `doc/recipes/shell/<entity_plural>/<entity>.org` |
| SQL | create, drop, notify trigger create, notify trigger drop |
| TypeScript | domain and protocol under `wire-protocol/src/generated/` |

Two consequences for the plan:

- Adding an entity is never a one-file change. A slice that adds four
  tables touches roughly 150 paths, and the SQL and TypeScript are
  generated rather than written.
- The generator emits a shell recipe document and shell commands for
  every entity. Those are part of the review, not noise.

For a **reporting** entity the count is 32, measured in
`evidence/reporting_model_recon.md`.
