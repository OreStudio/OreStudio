# Plan: ER Diagram Code Generation

## Objective

Automate generation of PlantUML ER diagrams from SQL schema files, following the existing `ores.codegen` patterns (mustache templates, JSON models).

## Design Principles

1. **Naming convention**: `plantuml_er_` prefix for ER-specific components, `plantuml_` for generic PlantUML utilities
2. **Validation as parsing side-effect**: Convention checks happen naturally during parsing, not as separate step
3. **Single source of truth**: All parsing/validation logic in codegen, thin wrapper in ores.sql for standalone validation

## Architecture Overview

```
SQL Files                         JSON Model                      PlantUML Diagram
┌─────────────────┐              ┌─────────────────┐             ┌─────────────────┐
│ create/**/*.sql │──parse+──────▶│ plantuml_er_    │──render────▶│ ores_schema.puml│
│ drop/**/*.sql   │  validate    │ model.json      │             └─────────────────┘
└─────────────────┘              │ (includes       │
                                 │  warnings[])    │
                                 └────────┬────────┘
                                          │
                                          ▼
                                 Warnings to stderr
                                 (--strict fails build)
```

## Components

### 1. SQL Parser Script (`plantuml_er_parse_sql.py`)

**Purpose:** Parse SQL CREATE/DROP statements, generate JSON model, and validate conventions.

**Location:** `projects/ores.codegen/src/plantuml_er_parse_sql.py`

**Input:**
- `projects/ores.sql/create/**/*_create.sql`
- `projects/ores.sql/drop/**/*_drop.sql`

**Output:** `projects/ores.codegen/models/plantuml_er_model.json`

**Flags:**
- `--warn` - Print warnings to stderr (default: on)
- `--strict` - Exit with code 1 if any warnings
- `--validate-only` - Skip model output, just validate

**Extraction targets:**
- Table name and component (from naming pattern)
- Columns: name, type, nullable, default
- Primary keys (simple and composite)
- Foreign key relationships (from trigger functions)
- Indexes (unique, regular, partial)
- Constraints (CHECK, EXCLUDE USING gist)
- Table classification: temporal, artefact, junction, current-state

**Pattern detection:**
```python
# Table classification
TEMPORAL = has columns ['valid_from', 'valid_to'] and EXCLUDE USING gist
ARTEFACT = has column 'dataset_id' and no temporal columns
JUNCTION = only FK columns + simple PK
CURRENT_STATE = none of the above
```

### 2. JSON Model Schema (`plantuml_er_model.json`)

```json
{
  "generated_at": "2026-01-24T12:00:00Z",
  "source_dir": "projects/ores.sql/create",
  "packages": [
    {
      "name": "iam",
      "description": "Identity & Access Management",
      "color": "#E8F4FD",
      "tables": [
        {
          "name": "iam_accounts_tbl",
          "component": "iam",
          "entity": "accounts",
          "classification": "temporal",
          "stereotype": "<<temporal>>",
          "color": "#C6F0D8",
          "description": "User accounts with authentication credentials",
          "primary_key": {
            "columns": ["id"],
            "composite": false
          },
          "columns": [
            {
              "name": "id",
              "type": "uuid",
              "nullable": false,
              "is_pk": true,
              "is_fk": false
            },
            {
              "name": "change_reason_code",
              "type": "text",
              "nullable": false,
              "is_pk": false,
              "is_fk": true,
              "references": {
                "table": "dq_change_reasons_tbl",
                "column": "code"
              }
            }
          ],
          "indexes": [
            {
              "name": "iam_accounts_username_uniq_idx",
              "columns": ["username"],
              "unique": true,
              "partial": "valid_to = ores.utility_infinity_timestamp_fn()"
            }
          ],
          "source_file": "create/iam/iam_accounts_create.sql"
        }
      ]
    }
  ],
  "relationships": [
    {
      "from_table": "iam_accounts_tbl",
      "to_table": "iam_login_info_tbl",
      "cardinality": "one-to-one",
      "label": "has",
      "from_column": "id",
      "to_column": "account_id"
    }
  ],
  "warnings": [
    {
      "file": "create/foo/foo_bar_create.sql",
      "line": 25,
      "code": "NAMING_001",
      "message": "Table name 'foo_bars' missing '_tbl' suffix"
    }
  ]
}
```

### 3. Mustache Template (`plantuml_er.mustache`)

**Location:** `projects/ores.codegen/library/templates/plantuml_er.mustache`

```plantuml
{{enhanced_license}}
@startuml

title ORES Database Schema
hide circle
skinparam linetype ortho

note as SchemaNote
ORES Database Schema
Generated: {{generated_at}}

All temporal tables use valid_from/valid_to
for bitemporal data management.
end note

{{#packages}}
'
' ==================== {{name}} ====================
'
package "{{name}}" {{color}} {
{{#tables}}
    entity {{name}} {{stereotype}} {{color}} {
{{#primary_key.columns}}
        * {{.}} : {{type}} <<PK>>
{{/primary_key.columns}}
        --
{{#columns}}
{{^is_pk}}
        {{#nullable}} {{/nullable}}{{^nullable}}* {{/nullable}}{{name}} : {{type}}{{#is_fk}} <<FK>>{{/is_fk}}
{{/is_pk}}
{{/columns}}
    }

{{#description}}
    note right of {{name}}
    {{description}}
    end note
{{/description}}

{{/tables}}
}

{{/packages}}

'
' ==================== Relationships ====================
'
{{#relationships}}
{{from_table}} {{notation}} {{to_table}} : "{{label}}"
{{/relationships}}

' Local Variables:
' compile-command: "plantuml ores_schema.puml"
' End:
@enduml
```

### 4. Generator Script (`plantuml_er_generate.py`)

**Purpose:** Render the template with the model to produce PlantUML.

**Location:** `projects/ores.codegen/src/plantuml_er_generate.py`

**Input:**
- `projects/ores.codegen/models/plantuml_er_model.json`
- `projects/ores.codegen/library/templates/plantuml_er.mustache`

**Output:** `projects/ores.sql/modeling/ores_schema.puml`

**Reuses:** Existing `generator.py` infrastructure (load_model, render_template, etc.)

### 5. Convention Validator (integrated into parser)

Check against `doc/skills/sql-schema-creator/SKILL.org` rules:

**Naming Conventions:**

| Code | Rule | Example Violation |
|------|------|-------------------|
| `NAMING_001` | Table suffix `_tbl` | `iam_accounts` instead of `iam_accounts_tbl` |
| `NAMING_002` | Component prefix required | `accounts_tbl` missing `iam_` prefix |
| `NAMING_003` | Entity name must be plural | `iam_account_tbl` instead of `iam_accounts_tbl` |
| `NAMING_004` | Index suffix `_idx` or `_uniq_idx` | `accounts_username` missing suffix |
| `NAMING_005` | Insert trigger suffix `_insert_trg` | `iam_accounts_trigger` wrong suffix |
| `NAMING_006` | Notify trigger suffix `_notify_trg` | Wrong notify trigger naming |
| `NAMING_007` | Insert function suffix `_insert_fn` | Wrong function naming |
| `NAMING_008` | Delete rule suffix `_delete_rule` | Wrong rule naming |
| `NAMING_009` | GiST index suffix `_gist_idx` | Missing `_gist_idx` for GIST indexes |
| `NAMING_010` | Valid component prefix | Unknown prefix (not iam_, refdata_, dq_, etc.) |

**Temporal Table Requirements:**

| Code | Rule | Example Violation |
|------|------|-------------------|
| `TEMPORAL_001` | Must have `valid_from` column | Temporal table missing valid_from |
| `TEMPORAL_002` | Must have `valid_to` column | Temporal table missing valid_to |
| `TEMPORAL_003` | Must have EXCLUDE USING gist | Missing temporal exclusion constraint |
| `TEMPORAL_004` | Must have `version` column | Missing optimistic locking column |
| `TEMPORAL_005` | Must have `modified_by` column | Missing audit user column |
| `TEMPORAL_006` | Must have `change_reason_code` | Missing change tracking column |
| `TEMPORAL_007` | Must have `change_commentary` | Missing change commentary column |
| `TEMPORAL_008` | PK must include temporal columns | Primary key missing valid_from, valid_to |

**Column Conventions:**

| Code | Rule | Example Violation |
|------|------|-------------------|
| `COLUMN_001` | UUID PK should be named `id` or `{entity}_id` | PK named `uuid_key` |
| `COLUMN_002` | FK should be `{referenced_entity}_id` | FK named `ref_id` instead of `account_id` |
| `COLUMN_003` | Timestamps should use `_at` suffix | `created` instead of `created_at` |
| `COLUMN_004` | Empty string CHECK for text PKs | Missing `CHECK (code <> '')` |
| `COLUMN_005` | Nil UUID CHECK for UUID PKs | Missing nil UUID check |

**Constraint Patterns:**

| Code | Rule | Example Violation |
|------|------|-------------------|
| `CONSTRAINT_001` | `valid_from < valid_to` CHECK | Missing temporal validity check |
| `CONSTRAINT_002` | Version uniqueness index | Missing version_uniq_idx for current records |

**Drop Script Completeness:**

| Code | Rule | Example Violation |
|------|------|-------------------|
| `DROP_001` | Table in create but not in drop | Table created but no drop statement |
| `DROP_002` | Function in create but not in drop | Function created but not dropped |
| `DROP_003` | Trigger in create but not in drop | Trigger created but not dropped |
| `DROP_004` | Index in create but not in drop | Index created but not dropped |
| `DROP_005` | Rule in create but not in drop | Delete rule created but not dropped |
| `DROP_006` | Materialized view not dropped | View created but not dropped |

**File Organization:**

| Code | Rule | Example Violation |
|------|------|-------------------|
| `FILE_001` | Create file naming | Should be `{component}_{entity}_create.sql` |
| `FILE_002` | Drop file naming | Should be `{component}_{entity}_drop.sql` |
| `FILE_003` | Notify trigger file naming | Should be `{component}_{entity}_notify_trigger_create.sql` |
| `FILE_004` | File in wrong component directory | `iam_accounts_create.sql` in `refdata/` |

**Warning Output:**

Warnings are:
1. Included in the JSON model under `warnings` array
2. Printed to stderr during parsing
3. Optionally fail the build with `--strict` flag

```bash
# Normal mode - warnings printed but generation continues
python3 plantuml_er_parse_sql.py --create-dir create/ --drop-dir drop/ \
    --output plantuml_er_model.json --warn

# Strict mode - exit 1 if any warnings
python3 plantuml_er_parse_sql.py --create-dir create/ --drop-dir drop/ \
    --output plantuml_er_model.json --warn --strict

# Validation only - no model output
python3 plantuml_er_parse_sql.py --create-dir create/ --drop-dir drop/ \
    --validate-only --warn --strict
```

Example warning output:
```
WARNING [NAMING_003] create/foo/foo_bar_create.sql:26
  Entity name should be plural: 'foo_bar_tbl' -> 'foo_bars_tbl'

WARNING [TEMPORAL_006] create/iam/iam_test_create.sql:45
  Temporal table missing change_reason_code column

WARNING [DROP_002] create/geo/geo_ip2country_create.sql:46
  Function 'geo_ip2country_lookup_fn' created but not found in drop script

=== Validation Summary ===
Tables parsed: 56
Warnings: 3 (1 naming, 1 temporal, 1 drop)
```

### 6. Shell Wrappers

#### Main Entry Point (`projects/ores.codegen/plantuml_er_generate.sh`)

```bash
#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SQL_DIR="${SCRIPT_DIR}/../ores.sql"

echo "=== ER Diagram Generation ==="

# Step 1: Parse SQL and generate model (includes validation)
echo "Parsing SQL schema..."
python3 "${SCRIPT_DIR}/src/plantuml_er_parse_sql.py" \
    --create-dir "${SQL_DIR}/create" \
    --drop-dir "${SQL_DIR}/drop" \
    --output "${SCRIPT_DIR}/models/plantuml_er_model.json" \
    --warn

# Step 2: Generate PlantUML from model
echo "Generating PlantUML..."
python3 "${SCRIPT_DIR}/src/plantuml_er_generate.py" \
    --model "${SCRIPT_DIR}/models/plantuml_er_model.json" \
    --template "${SCRIPT_DIR}/library/templates/plantuml_er.mustache" \
    --output "${SQL_DIR}/modeling/ores_schema.puml"

# Step 3: Render PNG (optional)
if command -v plantuml &> /dev/null; then
    echo "Rendering PNG..."
    plantuml "${SQL_DIR}/modeling/ores_schema.puml"
fi

echo "=== Done ==="
```

#### Validation-Only Wrapper (`projects/ores.sql/utility/validate_schemas.sh`)

Thin wrapper for standalone validation without generating diagrams:

```bash
#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SQL_DIR="${SCRIPT_DIR}/.."
CODEGEN_DIR="${SCRIPT_DIR}/../../ores.codegen"

# Parse with validation only - no model output
python3 "${CODEGEN_DIR}/src/plantuml_er_parse_sql.py" \
    --create-dir "${SQL_DIR}/create" \
    --drop-dir "${SQL_DIR}/drop" \
    --validate-only \
    --warn \
    "$@"  # Pass through --strict if provided
```

Usage:
```bash
# Check conventions (warnings only)
./projects/ores.sql/utility/validate_schemas.sh

# Strict mode for CI (fail on warnings)
./projects/ores.sql/utility/validate_schemas.sh --strict
```

## Directory Structure

```
projects/ores.codegen/
├── src/
│   ├── generator.py                  # Existing - reuse
│   ├── plantuml_er_parse_sql.py      # NEW - SQL parser + validator
│   └── plantuml_er_generate.py       # NEW - diagram generator
├── library/
│   └── templates/
│       └── plantuml_er.mustache      # NEW - ER diagram template
├── models/
│   └── plantuml_er_model.json        # NEW - generated model
└── plantuml_er_generate.sh           # NEW - entry point

projects/ores.sql/
└── scripts/
    └── validate_schema.sh            # NEW - thin wrapper for validation
```

## Implementation Steps

### Phase 1: Core Parser
1. Create `plantuml_er_parse_sql.py` with basic table extraction
2. Extract: table name, columns, types, nullable, constraints
3. Detect table classification (temporal, artefact, junction)
4. Generate minimal JSON model
5. Add basic naming convention warnings (table suffix, component prefix)

### Phase 2: Relationship Detection
1. Parse trigger functions for FK references
2. Extract explicit FK patterns from `WHERE ... = NEW.column`
3. Detect cardinality from column constraints
4. Cross-reference create and drop scripts for completeness warnings

### Phase 3: Template and Generator
1. Create `plantuml_er.mustache` template
2. Create `plantuml_er_generate.py` using existing infrastructure
3. Test end-to-end generation

### Phase 4: Full Convention Validation
1. Add temporal pattern checks (valid_from/to, EXCLUDE, audit columns)
2. Add column naming convention checks
3. Add file organization checks
4. Add `--strict` flag for CI

### Phase 5: Integration
1. Create main shell wrapper (`plantuml_er_generate.sh`)
2. Create validation wrapper (`validate_schema.sh`)
3. Add CMake target for diagram generation
4. Update documentation

## Future: C++ Class Diagram Commonality

The architecture separates concerns for reuse:

| Component | ER Diagram | Class Diagram | Shared |
|-----------|------------|---------------|--------|
| Parser | `plantuml_er_parse_sql.py` | `plantuml_class_parse_cpp.py` | Pattern extraction utilities |
| Model | `plantuml_er_model.json` | `plantuml_class_model.json` | JSON schema patterns |
| Template | `plantuml_er.mustache` | `plantuml_class.mustache` | PlantUML syntax helpers |
| Generator | `plantuml_er_generate.py` | `plantuml_class_generate.py` | `generator.py` infrastructure |

**Potential shared utilities (`plantuml_utils.py`):**
- PlantUML syntax escaping
- Stereotype formatting
- Color scheme management
- License header injection
- JSON model validation
- Warning/error formatting

## Success Criteria

1. Generated diagram matches current hand-maintained diagram
2. All 50+ tables represented with correct relationships
3. Convention warnings identify real issues
4. Generation completes in < 5 seconds
5. No manual editing required after generation
