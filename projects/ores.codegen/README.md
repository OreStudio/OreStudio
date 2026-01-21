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

## Features

- **Overall Models**: Support for `model.json` files that orchestrate the generation of multiple artefacts.
- **Dynamic Prefixing**: Use the `model_name` property in an overall model to prefix all output files (e.g., `solvaris_`).
- **Automatic Sibling Loading**: Sibling JSON models in the same directory are automatically loaded and available for cross-referencing in templates.
- **Enhanced Data Context**: Identifies specific datasets by subject area (e.g., `currencies_dataset`, `countries_dataset`) for easy template access.
- **License & Modelines**: Automatically generates license headers with proper editor modelines.

## Architecture

- `src/` - Python source code for the generator
- `library/data/` - Static data files (licenses, modelines, etc.)
- `library/templates/` - Mustache templates
- `models/` - JSON model files
- `output/` - Default directory for generated files

## Model-Template Mapping

The generator maps model files to templates based on their filenames:
- `model.json` → `sql_batch_execute.mustache`
- `catalogs.json` → `sql_catalog_populate.mustache`
- `country_currency.json` → `sql_flag_populate.mustache`, `sql_currency_populate.mustache`, `sql_country_populate.mustache`
- `datasets.json` → `sql_dataset_populate.mustache`, `sql_dataset_dependency_populate.mustache`
- `methodologies.json` → `sql_methodology_populate.mustache`
- `tags.json` → `sql_tag_populate.mustache`

## Extending

To add a new model-template pair:
1. Add your JSON model to the `models/` directory
2. Add your Mustache template to `library/templates/`
3. Update the mapping in `src/generator.py` in the `get_template_mappings()` function
