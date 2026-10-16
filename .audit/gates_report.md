# Local checks

**PASS.**  52 run, 52 passed, 0 failed.  Wall clock in checks: 1130s.

- Base: `origin/main`, merge base `b56bdf5aec`
- Preset: `linux-clang-release-ninja`
- Changed files: 51
- Classes: `cpp`, `docs`, `services`, `sql`, `unknown` (ran everything)

## Every check

| Check | Result | Time | What it proves |
|-------+--------+------+----------------|
| `tangle-templates` | pass | 7.4s | The templates tangle to the committed files |
| `component-drift` | pass | 331.5s | The components this branch changed match their models |
| `tangle-shell` | pass | 35.0s | The shell recipes tangle to the committed scripts |
| `tangle-http` | pass | 1.9s | The HTTP recipes tangle to the committed files |
| `site-page` | pass | 10.6s | The changed pages build and their links resolve |
| `component-docs` | pass | 1.1s | Every component has an overview and a diagram |
| `cmake-sources` | pass | 1.1s | Every component_files.cmake list matches the tree |
| `model-drift` | pass | 0.1s | Every model matches its schema |
| `recipe-inventories` | pass | 0.2s | The shell and HTTP recipe inventories match the tree |
| `physical-inventory` | pass | 0.1s | The physical-space inventories match the templates |
| `web` | pass | 114.6s | ores.web typechecks, formats and tests |
| `backlog-indexes` | pass | 0.2s | The backlog bucket indexes match the captures |
| `lint` | pass | 5.1s | Filetags, generator markers and id links are valid |
| `no-pr-triggers` | pass | 0.1s | No workflow triggers on a pull request |
| `org-links` | pass | 5.9s | Every org link resolves |
| `skill-namespace` | pass | 0.3s | Every skill uses the compass namespace |
| `skill-catalogue` | pass | 0.2s | The skills catalogue matches the skill tree |
| `skill-type-tables` | pass | 0.2s | The skill type tables match the catalogue |
| `skill-levels` | pass | 0.1s | Every skill declares its level |
| `skill-recipes` | pass | 0.1s | Every skill links its recipes both ways |
| `structure-notes` | pass | 1.1s | Every structure note is linked from and to its neighbours |
| `pattern-uses` | pass | 2.2s | Pattern links run both ways |
| `prototypes` | pass | 0.1s | The prototype inventory matches the tree |
| `drift-sweep` | pass | 317.8s | A render of every catalogue component matches the tree |
| `paste-blocks` | pass | 33.3s | Every pasted block names a symbol that exists |
| `er-diagram` | pass | 5.5s | The committed ER diagrams match the schema |
| `money-decimal-columns` | pass | 1.3s | A money quantity is an exact decimal, not a binary float |
| `model-sql-types` | pass | 1.0s | A column's model type is the type the table is created with |
| `sql-hygiene` | pass | 48.8s | The SQL tree has no unterminated comment or repeated index |
| `file-types-seed` | pass | 1.1s | The seeded file type names are the names the ORE samples use |
| `marketdata-identity-columns` | pass | 0.1s | The series identity projection matches the codec schema |
| `generated-column-refs` | pass | 0.9s | Generated code names no column its entity does not have |
| `protocol-twins` | pass | 0.3s | Every protocol header has its TypeScript twin |
| `subject-conformance` | pass | 15.4s | Every declared subject is inside the entity protocol |
| `handler-permissions` | pass | 0.6s | Every handler permission code is seeded |
| `open-reads` | pass | 0.6s | Every open read is declared |
| `shell-menus` | pass | 0.5s | No two shell menu entries collide |
| `route-auth` | pass | 0.8s | Every route declares its authorisation |
| `registry-exceptions` | pass | 0.2s | Every registry exception is justified |
| `reachability` | pass | 15.9s | No test case sits inside a conditional compilation block |
| `event-registrars` | pass | 0.1s | Every generated event registrar is composed |
| `populate-refs` | pass | 13.3s | Every populate script names a reference something defines |
| `sql-reachability` | pass | 1.1s | Every SQL file is reached by a schema entry point |
| `rls-widenings` | pass | 0.4s | No row-level security policy widens the system tenant |
| `entity-writes` | pass | 9.7s | Only the repositories write an entity table |
| `enum-attributes` | pass | 24.2s | No visibility macro decorates an enum |
| `exported-types` | pass | 42.0s | Every exported type is compiled |
| `boost-deps` | pass | 3.0s | No Boost header is included where it is not declared |
| `format` | pass | 0.8s | The changed C++ files are clang-formatted |
| `dsh-environment` | pass | 1.1s | The DSH environment plugin passes its own checks |
| `codegen-tests` | pass | 67.8s | The codegen suite passes |
| `compass-tests` | pass | 3.5s | The compass suite passes |

## Why these checks

- `cpp` — compiled sources, headers and the build description: `projects/ores.analytics/core/src/messaging/registrar.cpp`, `projects/ores.assets/core/src/messaging/registrar.cpp`, `projects/ores.compute/core/src/messaging/registrar.cpp`, `projects/ores.compute/core/src/messaging/report_submit_handler.cpp` (+27)
- `docs` — documentation, the site corpus and the agile record: `doc/agile/product_backlog/discarded.org`, `doc/agile/product_backlog/discarded/every_workflow_step_subject_needs_a_stream.org`, `doc/agile/product_backlog/inbox.org`, `doc/agile/product_backlog/inbox/every_workflow_step_subject_needs_a_stream.org` (+9)
- `services` — a service, its launcher and what starts it: `projects/ores.iam/service/src/app/application.cpp`, `projects/ores.nats/include/ores.nats/service/client.hpp`, `projects/ores.nats/include/ores.nats/service/jetstream_admin.hpp`, `projects/ores.nats/src/service/client.cpp` (+11)
- `sql` — the schema, its populate scripts and the migrations: `.audit/workflow_check.sql`
- `unknown`: a path no rule claims, so every check ran.
