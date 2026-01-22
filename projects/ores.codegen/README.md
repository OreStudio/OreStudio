# OreStudio Code Generator

A simple code generator that uses JSON models and Mustache templates to generate code.

## Setup

1. Install dependencies:
```bash
python3 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
```

## Usage

### Direct Usage
Run the code generator using the provided script:

```bash
./run_generator.sh <model_path> [output_dir]
```

Examples:
```bash
# Using default output directory (output/)
./run_generator.sh models/slovaris/catalogs.json

# Using custom output directory
./run_generator.sh models/slovaris/catalogs.json custom_output/
```

### Overall Models (Batch Execution)
You can define an overall `model.json` that references multiple files. The generator will automatically process all dependent models first.

```bash
./run_generator.sh models/slovaris/model.json
```

### Slovaris Generation
A dedicated script is provided to generate all Solvaris artefacts and place them in the correct location in the `ores.sql` project:

```bash
./generate_slovaris.sh
```

### FPML Reference Data Generation

Generate SQL schema and populate scripts from FPML Genericode XML files:

```bash
# Generate all FPML reference data (parses XML + generates SQL)
./generate_fpml_refdata.sh

# Generate only specific entities
./generate_fpml_refdata.sh --entities 'party-roles person-roles'

# Skip parsing, just regenerate SQL from existing models
./generate_fpml_refdata.sh --skip-parse

# Show help
./generate_fpml_refdata.sh --help
```

This script:
1. Parses FPML XML files from `projects/ores.sql/populate/data/`
2. Generates JSON entity models to `output/models/`
3. Generates SQL schema files to `projects/ores.sql/schema/`
4. Generates SQL populate files to `projects/ores.sql/populate/`

**Output files per entity** (e.g., `party_roles`):

| File | Location |
|------|----------|
| `refdata_party_roles_create.sql` | `projects/ores.sql/schema/` |
| `refdata_party_roles_notify_trigger.sql` | `projects/ores.sql/schema/` |
| `dq_party_roles_artefact_create.sql` | `projects/ores.sql/schema/` |
| `refdata_party_roles_populate.sql` | `projects/ores.sql/populate/` |

Plus the shared coding schemes file: `fpml_coding_schemes_populate.sql`

## Features

- **Overall Models**: Support for `model.json` files that orchestrate the generation of multiple artefacts.
- **Dynamic Prefixing**: Use the `model_name` property in an overall model to prefix all output files (e.g., `solvaris_`).
- **Automatic Sibling Loading**: Sibling JSON models in the same directory are automatically loaded and available for cross-referencing in templates.
- **Enhanced Data Context**: Identifies specific datasets by subject area (e.g., `currencies_dataset`, `countries_dataset`) for easy template access.
- **License & Modelines**: Automatically generates license headers with proper editor modelines.

## Architecture

- `src/generator.py` - Main code generator (JSON models + Mustache templates → SQL)
- `src/fpml_parser.py` - FPML Genericode XML parser (XML → JSON models)
- `library/data/` - Static data files (licenses, modelines, etc.)
- `library/templates/` - Mustache templates
- `models/` - JSON model files
- `output/` - Default directory for generated files

## Model-Template Mapping

The generator maps model files to templates based on their filenames:

**Standard mappings:**
- `model.json` → `sql_batch_execute.mustache`
- `catalogs.json` → `sql_catalog_populate.mustache`
- `country_currency.json` → `sql_flag_populate.mustache`, `sql_currency_populate.mustache`, `sql_country_populate.mustache`
- `datasets.json` → `sql_dataset_populate.mustache`, `sql_dataset_dependency_populate.mustache`
- `methodologies.json` → `sql_methodology_populate.mustache`
- `tags.json` → `sql_tag_populate.mustache`

**Entity schema mappings** (`*_entity.json` files):
- `sql_schema_table_create.mustache` → `{component}_{entity}_create.sql`
- `sql_schema_notify_trigger.mustache` → `{component}_{entity}_notify_trigger.sql`
- `sql_schema_artefact_create.mustache` → `dq_{entity}_artefact_create.sql`

**Entity populate mappings** (`*_data.json` files):
- `sql_populate_refdata.mustache` → `{component}_{entity}_populate.sql`

## Extending

To add a new model-template pair:
1. Add your JSON model to the `models/` directory
2. Add your Mustache template to `library/templates/`
3. Update the mapping in `src/generator.py` in the `get_template_mappings()` function
