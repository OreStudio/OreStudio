# Entity review after the credit simulation round trip

Reviewed at `a544f410ce`. Every table this branch adds, checked against
the rules the story set for itself. Ordered by what matters.

## 1. The database drops the netting set ids (data loss)

`<NettingSetIds>` is real content in the corpus documents
(`external/ore/examples/CreditRisk/Input/CreditPortfolioModel/creditsimulation.xml:120`),
and the mapper carries it: `mapped.netting_set_ids = v.NettingSetIds`.

**No analytics model has a column for it.** The four credit simulation
tables are config, entity, matrix and matrix row, and none of them stores
netting set ids. So:

- the in-memory round trip keeps them, because the struct carries them and
  the comparison at `xml_creditsimulation_mapper_roundtrip_tests.cpp:173`
  checks them;
- the database round trip **loses them silently**, and the database test
  never mentions them, so it cannot fail.

This is the vacuous-comparison failure again in a new place: a leg that
passes while dropping a field, because nothing on that leg looks at the
field. Fix before merge: model netting set ids, most likely a child table
of the configuration, and assert them in the database test the way the
in-memory test does.

## 2. What still makes sense

- The **reporting spine** is sound: `configuration_types` and
  `configurations` are lookups, `report_configurations` joins a definition
  to configurations through three validated foreign keys, and the
  parameter trail (`parameter_value_domains` → `parameter_definitions` →
  `configuration_parameters`) types a value by foreign key rather than by
  its shape.
- The **matrix row type** is right: one row per source rating with the
  target ratings as columns, which mirrors how the document writes the
  matrix and puts the fixed scale in the type where it belongs.
- The **naming rule** holds: every slug is short enough that the truncating
  fallback is never reached.

## 3. Inconsistencies with the rules this story set

Each of these is a place where the model says something the rules forbid,
or says the same thing two ways.

| Where | What is wrong |
|---|---|
| `credit_simulation_config`: `market`, `credit`, `credit_mode`, `loan_exposure_mode` | Discriminators as free text. The rule is a foreign key to a seeded lookup. These are ORE enumerations. |
| `credit_simulation_entity_config.factor_loadings` | A list stored as text. The rule is that a list is rows, one per member. |
| `credit_simulation_entity_config.initial_state` | An integer state index, while `matrix_row_config.from_rating` is a rating **code**. One concept, two representations, in adjacent tables. |
| `credit_simulation_matrix_row_config.from_rating` | Unconstrained text now that the lookup is gone. The eight columns fix the targets; nothing stops `AAA` or `AaaX` as a source. |
| `configurations.owning_component` | Free text discriminator, same as the first row. |
| `parameter_definitions.scope`, `.subtype` | Free text. `subtype` is the analytic kind, for which a seeded lookup of the 35 observed types is already planned. |
| `credit_simulation_config.name` | The mapper sets it to the constant `"CreditSimulation"` (`credit_simulation_mapper.cpp:107`), so every imported configuration carries the same string. A column that is always one value is not yet a name. |

## 4. Smaller points worth a decision, not necessarily a change

- `matrix_config.name` can be empty. The Legacy documents leave their
  matrix unnamed and the mapper leans on the binding's default. Identity is
  the id, so this is tolerable, but nothing records that it is intentional.
- `report_configurations` declares no natural key, so nothing prevents the
  same report binding one slot twice. That may be intended; if not, it is a
  unique constraint.
- `configuration_parameters.value` is not checked against its domain's
  `storage_kind`. The domain is descriptive today rather than validating,
  which is worth stating in the model so it is not mistaken for enforcement.
