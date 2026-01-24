#!/bin/bash

# =============================================================================
# IP to Country Reference Data Generator
# =============================================================================
#
# Generates SQL metadata files for IP to Country data:
#   - ip2country_catalog_populate.sql
#   - ip2country_methodology_populate.sql
#   - ip2country_dataset_populate.sql
#   - ip2country_dataset_tag_populate.sql
#   - ip2country.sql (master include)
#
# Usage:
#   ./generate_ip2country_refdata.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=============================================="
echo "IP to Country Reference Data Code Generator"
echo "=============================================="
echo ""

# Activate virtual environment
VENV_PATH="$SCRIPT_DIR/venv"
if [ -d "$VENV_PATH" ]; then
    source "$VENV_PATH/bin/activate"
fi

# Change to repo root for proper path resolution
cd "$REPO_ROOT"

echo "Generating IP to Country metadata from manifest..."
echo "----------------------------------------------"
python3 "$SCRIPT_DIR/src/ip2country_generate_metadata_sql.py"

echo ""
echo "=============================================="
echo "Generation complete!"
echo "=============================================="
echo ""
echo "Generated files (in projects/ores.sql/populate/ip2country/):"
echo "  - ip2country.sql (master include)"
echo "  - ip2country_catalog_populate.sql"
echo "  - ip2country_methodology_populate.sql"
echo "  - ip2country_dataset_populate.sql"
echo "  - ip2country_dataset_tag_populate.sql"
