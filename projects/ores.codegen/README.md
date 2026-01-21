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

Run the code generator using the provided script:

```bash
./run_generator.sh <model_path>
```

Example:
```bash
./run_generator.sh models/slovaris/catalogs.json
```

## Architecture

- `src/` - Python source code for the generator
- `library/data/` - Static data files (licenses, modelines, etc.)
- `library/templates/` - Mustache templates
- `models/` - JSON model files
- `output/` - Generated files

## Model-Template Mapping

The generator maps model files to templates based on their filenames:
- `catalogs.json` → `sql_catalog_populate.mustache`

## Extending

To add a new model-template pair:
1. Add your JSON model to the `models/` directory
2. Add your Mustache template to `library/templates/`
3. Update the mapping in `src/generator.py` in the `get_template_mappings()` function