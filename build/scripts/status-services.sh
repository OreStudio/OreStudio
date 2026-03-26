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
# For each service tracked by a PID file, shows:
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
            sed -n '/^# /s/^# \?//p' "$0" | head -17
            exit 0
            ;;
        *) echo "error: unknown option: $1"; exit 1 ;;
    esac
done

# Collect the expected service list from .env (mirrors start-services.sh logic).
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

BIN_DIR="$PROJECT_DIR/build/output/$PRESET/publish/bin"
LOG_DIR="$PROJECT_DIR/build/output/$PRESET/publish/log"
RUN_DIR="$PROJECT_DIR/build/output/$PRESET/publish/run"

declare -A EXPECTED_SERVICES
EXPECTED_SERVICES["ores.iam.service"]=1

while IFS='=' read -r key _value; do
    [[ "$key" =~ ^ORES_(.+)_SERVICE_DB_USER$ ]] || continue
    component="${BASH_REMATCH[1],,}"
    component="${component//_/.}"
    svc="ores.$component.service"
    [[ "$svc" == "ores.iam.service" ]] && continue
    [[ -x "$BIN_DIR/$svc" ]] && EXPECTED_SERVICES["$svc"]=1
done < "$ENV_FILE"

EXPECTED_SERVICES["ores.http.server"]=1
EXPECTED_SERVICES["ores.wt.service"]=1

# Add compute wrapper nodes if the binary exists and host IDs are configured.
if [[ -x "$BIN_DIR/ores.compute.wrapper" ]]; then
    for n in 1 2 3 4 5; do
        host_id_var="ORES_GRID_NODE_${n}_HOST_ID"
        if [[ -n "${!host_id_var:-}" ]]; then
            EXPECTED_SERVICES["ores.compute.wrapper.node${n}"]=1
        fi
    done
fi

echo "ORE Studio service status ($PRESET)"
echo ""
printf "  %-10s %-38s %s\n" "STATUS" "SERVICE" "DETAIL"
printf "  %-10s %-38s %s\n" "----------" "--------------------------------------" "------"

running=0
starting=0
stopped_count=0
missing=0
nodes_running=0
nodes_starting=0
nodes_stopped=0
nodes_missing=0

shopt -s nullglob

for svc in $(printf '%s\n' "${!EXPECTED_SERVICES[@]}" | sort); do
    pid_file="$RUN_DIR/$svc.pid"
    log_file="$LOG_DIR/$svc.log"

    is_node=false
    [[ "$svc" == ores.compute.wrapper.node* ]] && is_node=true

    if [[ ! -f "$pid_file" ]]; then
        printf "  %-10s %-38s %s\n" "missing" "$svc" "(no PID file)"
        if $is_node; then nodes_missing=$((nodes_missing + 1)); else missing=$((missing + 1)); fi
        continue
    fi

    pid="$(cat "$pid_file")"

    if ! kill -0 "$pid" 2>/dev/null; then
        printf "  %-10s %-38s %s\n" "stopped" "$svc" "PID $pid (dead)"
        if $is_node; then nodes_stopped=$((nodes_stopped + 1)); else stopped_count=$((stopped_count + 1)); fi
        continue
    fi

    # Determine the readiness string for this service.
    # NATS domain services log "Service ready"; HTTP and WT use different markers.
    ready_pattern="Service ready"

    # Process is alive — check whether it logged the readiness marker.
    if [[ -f "$log_file" ]] && grep -q "$ready_pattern" "$log_file" 2>/dev/null; then
        printf "  %-10s %-38s %s\n" "running" "$svc" "PID $pid"
        if $is_node; then nodes_running=$((nodes_running + 1)); else running=$((running + 1)); fi
    else
        # Show the last log line as a hint (strip timestamp prefix for brevity).
        last_line=""
        if [[ -f "$log_file" ]]; then
            last_line="$(tail -1 "$log_file" 2>/dev/null || true)"
            # Trim leading timestamp/thread noise — keep the quoted message part.
            last_line="${last_line##*\"] }"
        fi
        printf "  %-10s %-38s %s\n" "starting" "$svc" "PID $pid  ${last_line:+(${last_line:0:60})}"
        if $is_node; then nodes_starting=$((nodes_starting + 1)); else starting=$((starting + 1)); fi
    fi
done

echo ""
echo "services: running=$running  starting=$starting  stopped=$stopped_count  missing=$missing"
echo "nodes:    running=$nodes_running  starting=$nodes_starting  stopped=$nodes_stopped  missing=$nodes_missing"
echo ""
