#!/bin/bash
#
# Generate PlantUML ER diagram from SQL schema
#
# This script:
# 1. Parses SQL CREATE/DROP files to generate a JSON model
# 2. Renders the model using Mustache templates: one diagram per group,
#    and an index page that names the groups
# 3. Optionally draws every diagram as SVG using plantuml
#
# Usage:
#   ./plantuml_er_generate.sh          regenerate the .puml files, then draw them
#   ./plantuml_er_generate.sh --check  exit non-zero if any committed .puml
#                                      is stale, without drawing anything
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

# The parser (stage 1) has no --check; only the renderer does. Split the flag
# out so every other argument still reaches stage 1 as before.
CHECK=0
PARSE_ARGS=()
for arg in "$@"; do
    if [ "$arg" = "--check" ]; then
        CHECK=1
    else
        PARSE_ARGS+=("$arg")
    fi
done

# Step 1: Parse SQL and generate model (includes validation)
echo "Parsing SQL schema..."
python3 "${SCRIPT_DIR}/src/plantuml_er_parse_sql.py" \
    --create-dir "${SQL_DIR}/create" \
    --drop-dir "${SQL_DIR}/drop" \
    --output "${ER_MODEL}" \
    --ignore-file "${SQL_DIR}/utility/validation_ignore.txt" \
    --warn \
    "${PARSE_ARGS[@]}"

# Step 2: Generate PlantUML from model
echo ""
echo "Generating PlantUML..."
GENERATE_ARGS=(
    --model "${ER_MODEL}"
    --template "${SCRIPT_DIR}/library/templates/plantuml_er.mustache"
    --index-template "${SCRIPT_DIR}/library/templates/plantuml_er_index.mustache"
    --output "${SQL_DIR}/modeling/ores_schema.puml"
)
if [ "$CHECK" -eq 1 ]; then
    GENERATE_ARGS+=(--check)
fi
python3 "${SCRIPT_DIR}/src/plantuml_er_generate.py" "${GENERATE_ARGS[@]}"

# Step 3: Draw the diagrams (optional). SVG, because the schema has no
# raster budget left: at 463 tables the PNG overflowed a 32-bit pixel
# count. Every group file is drawn, not only the index. The images are not
# committed: PlantUML bakes the installed build's layout into them.
DIAGRAMS=("${SQL_DIR}/modeling/ores_schema"*.puml)

if [ "$CHECK" -eq 1 ]; then
    echo ""
    echo "Check mode: skipping the drawing step"
elif command -v plantuml &> /dev/null; then
    echo ""
    echo "Drawing ${#DIAGRAMS[@]} diagram(s) as SVG..."
    plantuml -tsvg "${DIAGRAMS[@]}"
elif [ -f /usr/share/plantuml/plantuml.jar ]; then
    echo ""
    echo "Drawing ${#DIAGRAMS[@]} diagram(s) as SVG..."
    java -Djava.awt.headless=true \
         -jar /usr/share/plantuml/plantuml.jar \
         -tsvg "${DIAGRAMS[@]}"
else
    echo ""
    echo "Note: plantuml not found, skipping the drawing step"
fi

echo ""
echo "=== Done ==="
