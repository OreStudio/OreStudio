#!/usr/bin/env bash
# stop-services.sh - Stop all ORE Studio backend services for a given build preset.
#
# Usage:
#   ./build/scripts/stop-services.sh [--preset PRESET]
#
# Options:
#   --preset PRESET   CMake preset name (overrides ORES_PRESET from .env)
#
# The build preset defaults to ORES_PRESET in .env (set by init-environment.sh).
# Use --preset to stop a specific preset without re-running init-environment.sh.
#
# Stop order:
#   1. Controller  — its stop_all() gracefully shuts down all managed services
#                    (IAM, domain services, HTTP, WT, compute wrappers)
#   2. Fallback    — SIGTERM any remaining ores.* processes from this bin dir
#   3. NATS server — stopped last; only if started by start-services.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

PRESET_ARG=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --preset) [[ -z "${2:-}" || "$2" == -* ]] && { echo "error: $1 requires a value" >&2; exit 1; }; PRESET_ARG="$2"; shift 2 ;;
        -h|--help)
            sed -n '/^# /s/^# \?//p' "$0" | head -17
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

BIN_DIR="$PROJECT_DIR/build/output/$PRESET/publish/bin"
RUN_DIR="$PROJECT_DIR/build/output/$PRESET/publish/run"

echo "Stopping ORE Studio services ($PRESET)"
echo ""

shopt -s nullglob

total_stopped=0
total_gone=0

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Sets STOP_PID to the PID of the stopped process, or "" if nothing was done.
STOP_PID=""
stop_service() {
    STOP_PID=""
    local name="$1"
    local pid_file="$RUN_DIR/$name.pid"
    [[ -f "$pid_file" ]] || return 0

    local pid
    pid="$(cat "$pid_file")"
    rm -f "$pid_file"

    if kill -0 "$pid" 2>/dev/null; then
        kill -TERM "$pid"
        printf "  stop    %-38s PID %d\n" "$name" "$pid"
        STOP_PID="$pid"
        total_stopped=$((total_stopped + 1))
    else
        printf "  gone    %-38s PID %d\n" "$name" "$pid"
        total_gone=$((total_gone + 1))
    fi
}

# Wait up to N seconds for pids to exit; SIGKILL any that survive.
wait_for_pids() {
    local label="$1"; local secs="${2:-15}"; shift 2
    local pids=("$@")
    [[ ${#pids[@]} -eq 0 ]] && return 0

    printf "Waiting for %s to exit..." "$label"
    local ticks=$(( secs * 2 ))
    for _i in $(seq 1 "$ticks"); do
        local all_done=true
        for pid in "${pids[@]}"; do
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

    for pid in "${pids[@]}"; do
        if kill -0 "$pid" 2>/dev/null; then
            kill -KILL "$pid" 2>/dev/null || true
            printf "  killed  PID %d (did not exit within grace period)\n" "$pid"
        fi
    done
    echo ""
}

# ---------------------------------------------------------------------------
# Step 1: Controller — triggers graceful stop_all() for every managed service
# ---------------------------------------------------------------------------

echo "[Controller]"
ctrl_pid_file="$RUN_DIR/ores.controller.service.pid"
if [[ ! -f "$ctrl_pid_file" ]]; then
    echo "error: no PID file for ores.controller.service ($ctrl_pid_file)" >&2
    echo "       is the controller running? start with: ./build/scripts/start-services.sh" >&2
    exit 1
fi
ctrl_pids=()
stop_service "ores.controller.service"
[[ -n "$STOP_PID" ]] && ctrl_pids+=("$STOP_PID")
echo ""
# Give the controller up to 30 s: it must stop all children before it exits.
wait_for_pids "controller" 30 "${ctrl_pids[@]}"

# Clean up any leftover PID files for dead processes.
for f in "$RUN_DIR"/*.pid; do
    [[ -f "$f" ]] || continue
    pid="$(cat "$f" 2>/dev/null || true)"
    [[ -z "$pid" ]] && continue
    kill -0 "$pid" 2>/dev/null || rm -f "$f"
done

# ---------------------------------------------------------------------------
# Step 3: NATS server (stopped last — only if started by start-services.sh)
# ---------------------------------------------------------------------------

CHECKOUT_LABEL="${ORES_CHECKOUT_LABEL:-local1}"
NATS_PID_FILE="$PROJECT_DIR/build/nats/$CHECKOUT_LABEL/nats-server.pid"

echo "[NATS server]"
if [[ -f "$NATS_PID_FILE" ]]; then
    nats_pid="$(cat "$NATS_PID_FILE")"
    rm -f "$NATS_PID_FILE"
    if kill -0 "$nats_pid" 2>/dev/null; then
        kill -TERM "$nats_pid"
        printf "  stop    %-38s PID %d\n" "nats-server" "$nats_pid"
        total_stopped=$((total_stopped + 1))
        echo ""
        wait_for_pids "NATS server" 10 "$nats_pid"
    else
        printf "  gone    %-38s PID %d\n" "nats-server" "$nats_pid"
        total_gone=$((total_gone + 1))
        echo ""
    fi
else
    echo "  skip    nats-server (no PID file — not managed by start-services.sh)"
    echo ""
fi

if [[ $((total_stopped + total_gone)) -eq 0 ]]; then
    echo "No services found for preset '$PRESET'."
    exit 0
fi

echo "Stopped $total_stopped service(s)${total_gone:+, $total_gone already gone}."
