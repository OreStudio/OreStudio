# Credit simulation slice: state and next steps

Written at `1105e8416c` on `feature/ore-report-configuration`, 37 commits
ahead of the remote. This is what exists, what is proven, and what is not.

## Proven

- **Reporting registry complete.** Six tables, live in the development
  database and aggregated on both the create and the drop side:
  `configuration_types`, `configurations`, `report_configurations`,
  `parameter_value_domains`, `parameter_definitions`,
  `configuration_parameters`. Verified with a `pg_tables` count returning 6.
- **Credit simulation round trips, in memory.** Four tables,
  `credit_simulation_config`, `_entity_config`, `_matrix_config`,
  `_matrix_row_config`. The matrix is one row per source rating with the
  target ratings as columns (`p_aaa` … `p_default`), and the scale is an
  ordered constant in `credit_simulation_mapper.hpp` mirrored by the seeded
  `ores_analytics_credit_ratings_tbl`. All 14 corpus documents round trip:
  matrix name, `t0`/`t1`, 64 probabilities cell by cell, and the 8 state
  labels. The full suite is green at 550 cases.

Reproduce with:

```
cmake --build build/output/linux-clang-debug-make --target test_ores.ore.core.tests -j 8
set -a; . ./.env; set +a
./build/output/linux-clang-debug-make/publish/bin/ores.ore.core.tests "creditsimulation*"
```

The environment load is not optional: the binary aborts with
`Required environment variable not set: ORES_TEST_DB_USER` without it.

## Not proven

- **The database leg has never been attempted.** Nothing in the round trip
  writes to or reads from PostgreSQL. This is the objective's central claim
  and the largest gap. The next step is a sibling test that takes the mapped
  entities, writes them through the generated repositories, reads them back,
  and only then calls `reverse`.

  The wiring is known, from
  `projects/ores.analytics/core/tests/repository_pricing_model_config_repository_tests.cpp:38-55`:

  ```cpp
  using ores::testing::scoped_database_helper;
  scoped_database_helper h;
  auto ctx = ores::testing::make_generation_context(h);   // for generators only
  repo.write(h.context(), entity);
  auto read = repo.read_latest(h.context());
  ```

  A repository needs no synthetic data: map a corpus document, then write the
  four vectors and read them back. Compare the re-read rows against the mapped
  ones before calling `reverse`, so a failure names which leg broke. Note that
  a re-read regenerates nothing but does reorder, so match rows by their
  natural key — matrix plus `from_rating` — and not by position.
- Nine of eleven document kinds have no tables at all.
- 121 generated files still exist for the three retired entities
  (`credit_ratings`, `matrix_state_config`, `matrix_cell_config`). The models
  and tables are gone; the generated code is not, because the component file
  lists are generated from it. Removing them means deleting three models,
  deleting those 121 files, and regenerating the component lists.

## Traps that cost time, recorded so they do not cost it again

- **The binding strips XML comments on load.** A `Data` element's state
  comment is in the file and gone after `load_data`
  (`stripComments`, `projects/ores.ore/core/src/domain/domain.cpp:635`). Any
  assertion about labels must be made against the serialised text, not a
  re-parsed document. This made one assertion unsatisfiable and made an
  earlier label comparison vacuous — it compared two empty vectors across 14
  files and passed.
- **The serialiser escapes the comment it is given.** `save_data` escapes `<`
  and `>` (`escape_xml`, `domain.cpp:874`) and `stripComments` does not
  unescape, so a comment written by `reverse` comes back as the literal text
  `&lt;!-- ... --&gt;`. That broke the value parse as well as the labels. The
  shared codec now accepts both spellings.
- **Which means the exported document is not yet known to be faithful.**
  Round trip is now green because our import tolerates our export, and those
  are the two sides we control. Whether ORE's own reader accepts an escaped
  comment where it wrote a real one is **untested**. This is the same shape as
  the vacuous label comparison: our two ends agreeing does not prove the
  artefact is right. Check it against the ORE schemas or the engine before
  treating this document as round-tripped.
- **A codegen model with a malformed header fails silently.** Lines not
  starting at column zero, or a stray `+` before `#+filetags`, leave the
  entity type unresolved and the generator writes 38 files named `unknown_*`
  instead of naming the bad line. Check the header first.
- **`:tablename:` in the entity's own `* Flags` block wins.** The composed
  fallback truncates `sql_name_base` to 50 bytes and two long names can
  collapse onto the same truncated table name.
- **The create and drop aggregates are hand-maintained and asymmetrically
  named**: `analytics_create.sql` against `drop_analytics.sql`, and
  `reporting_create.sql` against `reporting_drop.sql`. `codegen generate`
  never touches them. The component name for regeneration is `analytics-cpp`,
  not `analytics`.
- **`compass add entity_org --component <c> --slug <s> --shape fk-scoped`
  scaffolds a valid model.** Use it instead of writing 160 lines by hand.
- **Delegate with a bounded brief.** Three agents on this slice read
  extensively and committed nothing until told to write first, never read a
  file over 500 lines in full, and build and run before reporting.

## Document kinds still to do

`ore.xml` (416 files), `simulation.xml` (91), `curveconfig.xml` (76),
`conventions.xml` (67), `sensitivity.xml` (40), `stresstest.xml` (16),
`creditsimulation.xml` (done), `currencies.xml` (13), `simmcalibration.xml`
(2, and it has no generated binding at all), Basel traffic light and
historical return (no corpus file; historical return has no document root,
so it is a section of another document rather than a file type).
