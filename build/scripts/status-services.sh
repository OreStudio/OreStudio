#!/usr/bin/env bash
# status-services.sh - Show the status of all ORE Studio backend services.
#
# Usage:
#   ./build/scripts/status-services.sh [--preset PRESET]
#
# Options:
#   --preset PRESET   CMake preset name (overrides ORES_PRESET from .env)
#
# The build preset defaults to ORES_PRESET in .env (set by init-environment.sh).
# Use --preset to check a specific preset without re-running init-environment.sh.
#
# Shows status of NATS, the controller, and all services managed by the
# controller (discovered from PID files written by the process supervisor).
#
# For each service:
#   running  - process is alive and log contains "Service ready"
#   starting - process is alive but "Service ready" not yet seen in log
#   stopped  - PID file exists but process is gone
#   missing  - no PID file (service was never started or PID file was lost)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

PRESET_ARG=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --preset) [[ -z "${2:-}" || "$2" == -* ]] && { echo "error: $1 requires a value" >&2; exit 1; }; PRESET_ARG="$2"; shift 2 ;;
        -h|--help)
            sed -n '/^# /s/^# \?//p' "$0" | head -20
            exit 0
            ;;
        *) echo "error: unknown option: $1"; exit 1 ;;
    esac
done

ENV_FILE="$PROJECT_DIR/.env"
if [[ ! -f "$ENV_FILE" ]]; then
    echo "error: .env not found at $ENV_FILE" >&2
    echo "       run: ./build/scripts/init-environment.sh --preset <preset>" >&2
    exit 1
fi
set -a
# shellcheck disable=SC1090
source "$ENV_FILE"
set +a

# --preset flag takes priority; fall back to ORES_PRESET from .env.
PRESET="${PRESET_ARG:-${ORES_PRESET:-}}"
if [[ -z "$PRESET" ]]; then
    echo "error: no preset — pass --preset <preset> or set ORES_PRESET via init-environment.sh" >&2
    exit 1
fi

LOG_DIR="$PROJECT_DIR/build/output/$PRESET/publish/log"
RUN_DIR="$PROJECT_DIR/build/output/$PRESET/publish/run"

CHECKOUT_LABEL="${ORES_CHECKOUT_LABEL:-local1}"
NATS_PORT="${ORES_NATS_PORT:-4222}"
NATS_PID_FILE="$PROJECT_DIR/build/nats/$CHECKOUT_LABEL/nats-server.pid"
NATS_LOG="$LOG_DIR/nats-server.log"

echo "ORE Studio service status ($PRESET)"
echo ""
printf "  %-10s %-40s %s\n" "STATUS" "SERVICE" "DETAIL"
printf "  %-10s %-40s %s\n" "----------" "----------------------------------------" "------"

running=0
starting=0
stopped_count=0
missing=0

# ---------------------------------------------------------------------------
# Helper: check one service by name (log file = LOG_DIR/name.log)
# ---------------------------------------------------------------------------
check_service() {
    local svc="$1"
    local pid_file="$RUN_DIR/$svc.pid"
    local log_file="$LOG_DIR/$svc.log"

    if [[ ! -f "$pid_file" ]]; then
        printf "  %-10s %-40s %s\n" "missing" "$svc" "(no PID file)"
        missing=$((missing + 1))
        return
    fi

    local pid
    pid="$(cat "$pid_file")"

    if ! kill -0 "$pid" 2>/dev/null; then
        printf "  %-10s %-40s %s\n" "stopped" "$svc" "PID $pid (dead)"
        stopped_count=$((stopped_count + 1))
        return
    fi

    if [[ -f "$log_file" ]] && grep -q "Service ready" "$log_file" 2>/dev/null; then
        printf "  %-10s %-40s %s\n" "running" "$svc" "PID $pid"
        running=$((running + 1))
    else
        local last_line=""
        if [[ -f "$log_file" ]]; then
            last_line="$(tail -1 "$log_file" 2>/dev/null || true)"
            last_line="${last_line##*\"] }"
        fi
        printf "  %-10s %-40s %s\n" "starting" "$svc" "PID $pid  ${last_line:+(${last_line:0:60})}"
        starting=$((starting + 1))
    fi
}

# ---------------------------------------------------------------------------
# NATS server (infrastructure)
# ---------------------------------------------------------------------------
if [[ ! -f "$NATS_PID_FILE" ]]; then
    printf "  %-10s %-40s %s\n" "missing" "nats-server" "(no PID file — start via start-services.sh or prodigy)"
elif ! kill -0 "$(cat "$NATS_PID_FILE")" 2>/dev/null; then
    printf "  %-10s %-40s %s\n" "stopped" "nats-server" "PID $(cat "$NATS_PID_FILE") (dead)"
elif [[ -f "$NATS_LOG" ]] && grep -q "Server is ready" "$NATS_LOG" 2>/dev/null; then
    printf "  %-10s %-40s %s\n" "running" "nats-server" "PID $(cat "$NATS_PID_FILE")  port $NATS_PORT"
else
    printf "  %-10s %-40s %s\n" "starting" "nats-server" "PID $(cat "$NATS_PID_FILE")  port $NATS_PORT"
fi

# ---------------------------------------------------------------------------
# Controller (the supervisor — manages all other services)
# ---------------------------------------------------------------------------
check_service "ores.controller.service"

# ---------------------------------------------------------------------------
# Managed services — discovered from PID files written by the process supervisor.
# The supervisor writes: service_name.pid (replica 0), service_name.N.pid (replica N).
# Skip the controller PID file (already shown above).
# ---------------------------------------------------------------------------
shopt -s nullglob
for pid_file in "$RUN_DIR"/*.pid; do
    svc="$(basename "$pid_file" .pid)"
    [[ "$svc" == "ores.controller.service" ]] && continue

    pid="$(cat "$pid_file")"
    log_file="$LOG_DIR/$svc.log"

    if ! kill -0 "$pid" 2>/dev/null; then
        printf "  %-10s %-40s %s\n" "stopped" "$svc" "PID $pid (dead)"
        stopped_count=$((stopped_count + 1))
        continue
    fi

    if [[ -f "$log_file" ]] && grep -q "Service ready" "$log_file" 2>/dev/null; then
        printf "  %-10s %-40s %s\n" "running" "$svc" "PID $pid"
        running=$((running + 1))
    else
        last_line=""
        if [[ -f "$log_file" ]]; then
            last_line="$(tail -1 "$log_file" 2>/dev/null || true)"
            last_line="${last_line##*\"] }"
        fi
        printf "  %-10s %-40s %s\n" "starting" "$svc" "PID $pid  ${last_line:+(${last_line:0:60})}"
        starting=$((starting + 1))
    fi
done

echo ""
echo "services: running=$running  starting=$starting  stopped=$stopped_count  missing=$missing"
echo ""
echo "Logs : $LOG_DIR"
echo ""
