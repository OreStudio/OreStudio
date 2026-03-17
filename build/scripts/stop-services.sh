#!/usr/bin/env bash
# stop-services.sh - Stop all ORE Studio backend services for a given build preset.
#
# Usage:
#   ./build/scripts/stop-services.sh [--preset PRESET]
#
# Options:
#   --preset PRESET   CMake preset name (default: linux-clang-debug-ninja)
#
# Environment:
#   ORES_PRESET       Overrides the default preset (--preset takes priority)
#
# Reads PIDs from publish/run/ and sends SIGTERM to each running service,
# then waits briefly for a graceful shutdown.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

PRESET="${ORES_PRESET:-linux-clang-debug-ninja}"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --preset) PRESET="$2"; shift 2 ;;
        -h|--help)
            sed -n '/^# /s/^# \?//p' "$0" | head -12
            exit 0
            ;;
        *) echo "error: unknown option: $1"; exit 1 ;;
    esac
done

RUN_DIR="$PROJECT_DIR/build/output/$PRESET/publish/run"

if [[ ! -d "$RUN_DIR" ]]; then
    echo "No services started for preset '$PRESET' (no run directory found)."
    exit 0
fi

shopt -s nullglob
pid_files=("$RUN_DIR"/*.pid)

if [[ ${#pid_files[@]} -eq 0 ]]; then
    echo "No PID files found in $RUN_DIR — nothing to stop."
    exit 0
fi

echo "Stopping ORE Studio services ($PRESET)"
echo ""

stopped_pids=()
stopped=0
gone=0

for pid_file in "${pid_files[@]}"; do
    name="$(basename "$pid_file" .pid)"
    pid="$(cat "$pid_file")"

    if kill -0 "$pid" 2>/dev/null; then
        kill -TERM "$pid"
        printf "  stop    %-38s PID %d\n" "$name" "$pid"
        stopped_pids+=("$pid")
        stopped=$((stopped + 1))
    else
        printf "  gone    %-38s PID %d\n" "$name" "$pid"
        gone=$((gone + 1))
    fi

    rm -f "$pid_file"
done

echo ""

# Wait up to 10 seconds for all stopped processes to exit.
if [[ $stopped -gt 0 ]]; then
    printf "Waiting for %d service(s) to exit..." "$stopped"
    for _i in $(seq 1 20); do
        all_done=true
        for pid in "${stopped_pids[@]}"; do
            if kill -0 "$pid" 2>/dev/null; then
                all_done=false
                break
            fi
        done
        $all_done && break
        sleep 0.5
        printf "."
    done
    echo " done"
    echo ""
fi

echo "Stopped $stopped service(s)${gone:+, $gone already gone}."
