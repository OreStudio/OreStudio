#!/bin/bash
#
# ores.seeder CLI: database-level synthetic test-data generation.
#
# Reads a dataset's model.json (the batch driver) and renders each
# (template, output, source) tuple via ores.codegen.generator. SQL
# populate scripts are written under projects/ores.sql/populate/<name>/.
#
# Usage:
#   ./projects/ores.seeder/seeder.sh generate <dataset>
#
# Example:
#   ./projects/ores.seeder/seeder.sh generate slovaris
#
# See projects/ores.seeder/modeling/component_overview.org for the design.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CODEGEN_DIR="$SCRIPT_DIR/../ores.codegen"
VENV_PATH="$CODEGEN_DIR/venv"

if [ ! -d "$VENV_PATH" ]; then
    echo "Error: Virtual environment not found at $VENV_PATH" >&2
    echo "Please run: cd $CODEGEN_DIR && python3 -m venv venv && source venv/bin/activate && pip install -r requirements.txt" >&2
    exit 1
fi

source "$VENV_PATH/bin/activate"

REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_ROOT"

cmd="${1:-}"

case "$cmd" in
    generate)
        dataset="${2:-}"
        if [ -z "$dataset" ]; then
            echo "usage: seeder.sh generate <dataset>" >&2
            exit 1
        fi

        model="$SCRIPT_DIR/datasets/$dataset/model.json"
        if [ ! -f "$model" ]; then
            echo "Error: dataset model not found: $model" >&2
            exit 1
        fi

        # By convention populate-script outputs go under
        # projects/ores.sql/populate/<dataset>/ — the model.json
        # entries are file names relative to that directory.
        out_dir="$REPO_ROOT/projects/ores.sql/populate/$dataset"
        mkdir -p "$out_dir"

        echo "=============================================="
        echo "ores.seeder — generating dataset '$dataset'"
        echo "=============================================="
        python "$CODEGEN_DIR/src/generator.py" "$model" "$out_dir"
        echo
        echo "Done. Output: $out_dir"
        ;;
    ""|-h|--help)
        cat <<EOF
ores.seeder — database test-data generator.

Subcommands:
  generate <dataset>   Render all artefacts declared in
                       datasets/<dataset>/model.json into
                       projects/ores.sql/populate/<dataset>/.

Examples:
  ./projects/ores.seeder/seeder.sh generate slovaris
EOF
        ;;
    *)
        echo "unknown subcommand: $cmd" >&2
        echo "run with --help for usage" >&2
        exit 1
        ;;
esac
