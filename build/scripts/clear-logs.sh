#!/usr/bin/env bash
# clear-logs.sh - Delete all service log files for a given build preset.
#
# Usage:
#   ./build/scripts/clear-logs.sh [--preset PRESET]
#
# Options:
#   --preset PRESET   CMake preset name (overrides ORES_PRESET from .env)
#
# The build preset defaults to ORES_PRESET in .env (set by init-environment.sh).
# Use --preset to clear a specific preset's logs without re-running
# init-environment.sh.
#
# Removes every *.log and *.err file under
# build/output/<preset>/publish/log/ so the next run starts from clean logs.
# Logs are disposable — services recreate them on startup — so this does not
# prompt for confirmation.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

PRESET_ARG=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --preset) [[ -z "${2:-}" || "$2" == -* ]] && { echo "error: $1 requires a value" >&2; exit 1; }; PRESET_ARG="$2"; shift 2 ;;
        -h|--help)
            sed -n '/^# /s/^# \?//p' "$0" | head -12
            exit 0
            ;;
        *) echo "error: unknown option: $1"; exit 1 ;;
    esac
done

ENV_FILE="$PROJECT_DIR/.env"
if [[ ! -f "$ENV_FILE" ]]; then
    echo "error: .env not found at $ENV_FILE"
    echo "       run: ./build/scripts/init-environment.sh --preset <preset>"
    exit 1
fi
set -a
# shellcheck disable=SC1090
source "$ENV_FILE"
set +a

# --preset flag takes priority; fall back to ORES_PRESET from .env.
PRESET="${PRESET_ARG:-${ORES_PRESET:-}}"
if [[ -z "$PRESET" ]]; then
    echo "error: no preset — pass --preset <preset> or set ORES_PRESET via init-environment.sh"
    exit 1
fi

LOG_DIR="$PROJECT_DIR/build/output/$PRESET/publish/log"
if [[ ! -d "$LOG_DIR" ]]; then
    echo "Nothing to clear: log directory does not exist ($LOG_DIR)."
    exit 0
fi

shopt -s nullglob
files=("$LOG_DIR"/*.log "$LOG_DIR"/*.err)

if [[ ${#files[@]} -eq 0 ]]; then
    echo "No log files under $LOG_DIR."
    exit 0
fi

for f in "${files[@]}"; do
    rm -f "$f"
done

echo "Cleared ${#files[@]} log file(s) from $LOG_DIR ($PRESET)."
