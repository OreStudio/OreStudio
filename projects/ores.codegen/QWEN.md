# OreStudio Code Generator (ores.codegen) - Knowledge Base

## Project Overview
The OreStudio Code Generator is a Python-based tool that uses JSON models and Mustache templates to generate code. It's designed to be a simple code generator that takes structured data and applies templates to produce output files.

## Directory Structure
```
projects/ores.codegen/
├── library/
│   ├── data/                 # Static data files (licenses, modelines, etc.)
│   │   ├── licence-GPL-v3.txt
│   │   └── modeline.json
│   └── templates/            # Mustache template files
│       └── sql_catalog_populate.mustache
├── models/                   # JSON model files that provide data for generation
│   └── slovaris/
│       ├── catalogs.json
│       ├── country_currency.json
│       └── datasets.json
├── output/                   # Where generated files are placed
├── src/                      # Python source code
│   ├── __init__.py
│   └── generator.py
├── venv/                     # Python virtual environment
├── README.md                 # Project documentation
├── requirements.txt          # Python dependencies
└── run_generator.sh          # Execution script
```

## Key Components

### Main Generator (`src/generator.py`)
The main Python file contains several key functions:

- `load_data(data_dir)` - Loads JSON and text files from the data directory
- `format_comment_block(text, lang)` - Formats text as language-specific comment blocks
- `generate_license_with_header(license_text, modeline_info, lang)` - Creates license headers with modelines and copyright
- `render_template(template_path, data)` - Renders Mustache templates with provided data
- `get_template_mappings()` - Defines mapping between model filenames and templates
- `generate_from_model()` - Main function that orchestrates the generation process

### Template System
- Uses Mustache templating engine via `pystache` library
- Templates are stored in `library/templates/` directory
- Current example: `sql_catalog_populate.mustache`

### Data Files
- `library/data/licence-GPL-v3.txt` - Contains the GPL v3 license text
- `library/data/modeline.json` - Contains editor modeline configurations for different languages

### Model Files
- JSON files in `models/` directory provide data for code generation
- Example: `models/slovaris/catalogs.json` contains catalog information

## Configuration and Mappings

### Template Mappings
Currently defined mapping in `get_template_mappings()`:
- `"catalogs.json"` → `["sql_catalog_populate.mustache"]`

### Modeline Configurations
From `modeline.json`:
- SQL: `"sql-product: postgres; tab-width: 4; indent-tabs-mode: nil"`
- C++: `"mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4"`

## Dependencies
- `pystache>=0.6.0` - Mustache templating for Python

## Usage

### Via Shell Script
```bash
./run_generator.sh <model_path> [output_dir]
```

### Direct Python Execution
```bash
python src/generator.py <model_path> [output_dir]
```

## Features

### License Generation
Automatically generates license headers with:
- Editor modelines
- Copyright information with current year
- Proper comment formatting for different languages

### Multi-language Comment Support
Supports different comment formats:
- SQL: `/* ... */` with ` * ` prefix for lines
- C++: `/* ... */` with ` * ` prefix for lines
- Python: `""" ... """` with `# ` prefix for lines
- JavaScript: `/** ... */` with ` * ` prefix for lines

### Flexible Output
- Default output directory: `output/`
- Custom output directory support via command line parameter
- Automatic creation of output directory if it doesn't exist

## Current Use Case
The project appears to be set up for generating SQL files for a data quality system called "Slovaris", with proper licensing and modeline support. The example generates SQL statements to populate DQ catalogs in a PostgreSQL database.

## Example Model Structure
From `models/slovaris/catalogs.json`:
```json
[
    {
        "name": "Slovaris",
        "description": "Imaginary world to test all system functions.",
        "owner": "Testing Team"
    }
]
```

## Example Template Usage
The `sql_catalog_populate.mustache` template generates SQL code that:
1. Includes the enhanced license header
2. Sets the schema to 'ores'
3. Generates SQL calls to `ores.upsert_dq_catalogs()` function
4. Includes summary queries

## Extension Points
To add new model-template pairs:
1. Add your JSON model to the `models/` directory
2. Add your Mustache template to `library/templates/`
3. Update the mapping in `src/generator.py` in the `get_template_mappings()` function