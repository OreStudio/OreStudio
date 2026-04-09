#!/bin/bash

# Script to generate C++ domain and repository types for trading instrument families.
#
# Generates domain types (ores.trading.api) and repository layer (ores.trading.core)
# for all rates instrument types using ores.codegen.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENV_PATH="$SCRIPT_DIR/venv"

if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH"
    echo "Please run: python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt"
    exit 1
fi

source "$VENV_PATH/bin/activate"
cd "$SCRIPT_DIR"

REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=============================================="
echo "Trading Instruments Code Generator"
echo "=============================================="
echo ""

INSTRUMENTS=(
    fra_instrument
    vanilla_swap_instrument
    cap_floor_instrument
    swaption_instrument
    balance_guaranteed_swap_instrument
    callable_swap_instrument
    knock_out_swap_instrument
    inflation_swap_instrument
    rpa_instrument
)

for instrument in "${INSTRUMENTS[@]}"; do
    model="$SCRIPT_DIR/models/trading/${instrument}_domain_entity.json"
    if [ ! -f "$model" ]; then
        echo "Warning: model not found: $model"
        continue
    fi
    echo "Processing: $instrument"
    python "$SCRIPT_DIR/src/generator.py" "$model" "$REPO_ROOT/" --profile domain
    python "$SCRIPT_DIR/src/generator.py" "$model" "$REPO_ROOT/" --profile repository
done

echo ""
echo "=============================================="
echo "Generation complete!"
echo "=============================================="
echo ""
echo "Domain types:    projects/ores.trading.api/"
echo "Repository layer: projects/ores.trading.core/"
