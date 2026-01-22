#!/bin/bash

# Script to generate all FPML reference data schema and populate files
#
# This script:
# 1. Parses FPML XML files to generate JSON models
# 2. Generates SQL schema files to projects/ores.sql/schema/
# 3. Generates SQL populate files to projects/ores.sql/populate/

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

# Check virtual environment
if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH"
    echo "Please run: python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
    exit 1
fi

source "$VENV_PATH/bin/activate"
cd "$SCRIPT_DIR"

# Directories
FPML_DATA_DIR="$SCRIPT_DIR/../ores.sql/populate/data"
OUTPUT_DIR="$SCRIPT_DIR/output"
SCHEMA_DIR="$SCRIPT_DIR/../ores.sql/schema"
POPULATE_DIR="$SCRIPT_DIR/../ores.sql/populate"

# Hard-coded list of FPML entities to generate
# NOTE: currencies are NOT included - they have special handling elsewhere
FPML_ENTITIES=(
    "account-types"
    "asset-classes"
    "asset-measures"
    "benchmark-rates"
    "business-centres"
    "business-processes"
    "cashflow-types"
    "entity-classifications"
    "local-jurisdictions"
    "party-relationships"
    "party-roles"
    "person-roles"
    "regulatory-corporate-sectors"
    "reporting-regimes"
    "supervisory-bodies"
)

# Parse command line arguments
ENTITIES=""
SKIP_PARSE=false

while [[ $# -gt 0 ]]; do
    case $1 in
        --entities)
            ENTITIES="$2"
            shift 2
            ;;
        --skip-parse)
            SKIP_PARSE=true
            shift
            ;;
        --help)
            echo "Usage: $0 [options]"
            echo ""
            echo "Options:"
            echo "  --entities 'entity1 entity2'  Process only specific entities (space-separated)"
            echo "  --skip-parse                  Skip FPML parsing, use existing output/"
            echo "  --help                        Show this help"
            echo ""
            echo "Examples:"
            echo "  $0                                    # Process all entities"
            echo "  $0 --entities 'party-roles'           # Process only party-roles"
            echo "  $0 --skip-parse                       # Use existing models, just regenerate SQL"
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            exit 1
            ;;
    esac
done

echo "=============================================="
echo "FPML Reference Data Code Generator"
echo "=============================================="
echo ""

# Step 1: Parse FPML XML files
if [ "$SKIP_PARSE" = false ]; then
    echo "Step 1: Parsing FPML XML files..."
    echo "----------------------------------------------"

    if [ -n "$ENTITIES" ]; then
        python "$SCRIPT_DIR/src/fpml_parser.py" "$FPML_DATA_DIR" "$OUTPUT_DIR" --entities $ENTITIES
    else
        python "$SCRIPT_DIR/src/fpml_parser.py" "$FPML_DATA_DIR" "$OUTPUT_DIR" --entities ${FPML_ENTITIES[@]}
    fi
    echo ""
else
    echo "Step 1: Skipping FPML parsing (using existing output/)"
    echo ""
fi

# Step 2: Generate coding schemes SQL
echo "Step 2: Copying coding schemes SQL..."
echo "----------------------------------------------"
if [ -f "$OUTPUT_DIR/fpml_coding_schemes_populate.sql" ]; then
    cp "$OUTPUT_DIR/fpml_coding_schemes_populate.sql" "$POPULATE_DIR/"
    echo "  -> $POPULATE_DIR/fpml_coding_schemes_populate.sql"
fi
echo ""

# Step 3: Generate schema files for each entity
echo "Step 3: Generating schema files..."
echo "----------------------------------------------"
for model_file in "$OUTPUT_DIR/models/"*_entity.json; do
    if [ -f "$model_file" ]; then
        entity_name=$(basename "$model_file" _entity.json)
        echo "  Processing: $entity_name"
        python "$SCRIPT_DIR/src/generator.py" "$model_file" "$SCHEMA_DIR/"
    fi
done
echo ""

# Step 4: Generate populate files for each entity
echo "Step 4: Generating populate files..."
echo "----------------------------------------------"
for data_file in "$OUTPUT_DIR/data/"*_data.json; do
    if [ -f "$data_file" ]; then
        entity_name=$(basename "$data_file" _data.json)
        echo "  Processing: $entity_name"
        python "$SCRIPT_DIR/src/generator.py" "$data_file" "$POPULATE_DIR/"
    fi
done
echo ""

echo "=============================================="
echo "Generation complete!"
echo "=============================================="
echo ""
echo "Schema files:   $SCHEMA_DIR/"
echo "Populate files: $POPULATE_DIR/"
echo ""
echo "Generated files:"
echo "  - fpml_coding_schemes_populate.sql"

# List generated schema files
for model_file in "$OUTPUT_DIR/models/"*_entity.json; do
    if [ -f "$model_file" ]; then
        entity_name=$(basename "$model_file" _entity.json)
        echo "  - refdata_${entity_name}_create.sql"
        echo "  - refdata_${entity_name}_notify_trigger.sql"
        echo "  - dq_${entity_name}_artefact_create.sql"
        echo "  - refdata_${entity_name}_populate.sql"
    fi
done
