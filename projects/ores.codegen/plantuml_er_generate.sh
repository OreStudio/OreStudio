#!/bin/bash
#
# Generate PlantUML ER diagram from SQL schema
#
# This script:
# 1. Parses SQL CREATE/DROP files to generate a JSON model
# 2. Renders the model using a Mustache template to produce PlantUML
# 3. Optionally renders the PNG using plantuml
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
SQL_DIR="${SCRIPT_DIR}/../ores.sql"
VENV_PATH="$SCRIPT_DIR/venv"

# Intermediate model lives under build/output/ (gitignored), not models/.
# The file is regenerated on every run by Step 1 and consumed by Step 2.
ER_MODEL_DIR="${PROJECT_ROOT}/build/output/codegen"
ER_MODEL="${ER_MODEL_DIR}/plantuml_er_model.json"
mkdir -p "${ER_MODEL_DIR}"

# Check virtual environment
if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH"
    echo "Please run: python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
    exit 1
fi

source "$VENV_PATH/bin/activate"
cd "$SCRIPT_DIR"

echo "=== ER Diagram Generation ==="

# Step 1: Parse SQL and generate model (includes validation)
echo "Parsing SQL schema..."
python3 "${SCRIPT_DIR}/src/plantuml_er_parse_sql.py" \
    --create-dir "${SQL_DIR}/create" \
    --drop-dir "${SQL_DIR}/drop" \
    --output "${ER_MODEL}" \
    --ignore-file "${SQL_DIR}/utility/validation_ignore.txt" \
    --warn \
    "$@"

# Step 2: Generate PlantUML from model
echo ""
echo "Generating PlantUML..."
python3 "${SCRIPT_DIR}/src/plantuml_er_generate.py" \
    --model "${ER_MODEL}" \
    --template "${SCRIPT_DIR}/library/templates/plantuml_er.mustache" \
    --output "${SQL_DIR}/modeling/ores_schema.puml"

# Step 3: Render PNG (optional)
if command -v plantuml &> /dev/null; then
    echo ""
    echo "Rendering PNG..."
    PLANTUML_LIMIT_SIZE=131072 plantuml "${SQL_DIR}/modeling/ores_schema.puml"
elif [ -f /usr/share/plantuml/plantuml.jar ]; then
    echo ""
    echo "Rendering PNG..."
    java -Djava.awt.headless=true \
         -DPLANTUML_LIMIT_SIZE=131072 \
         -jar /usr/share/plantuml/plantuml.jar \
         "${SQL_DIR}/modeling/ores_schema.puml"
else
    echo ""
    echo "Note: plantuml not found, skipping PNG generation"
fi

echo ""
echo "=== Done ==="
