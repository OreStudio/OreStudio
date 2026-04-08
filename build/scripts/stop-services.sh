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
# Services are stopped in reverse dependency order to allow clean NATS draining:
#   0. Compute wrapper nodes       (outermost layer)
#   1. WT service and HTTP server  (front-end servers)
#   2. Domain services             (NATS workers, except IAM)
#   3. IAM service                 (JWKS provider)
#   4. NATS server                 (stopped last — only if started by this script)
#
# Waits between phases for graceful NATS drain to complete.

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
# Signal escalation: SIGTERM (graceful) only; SIGKILL is applied by
# wait_for_pids_kill if the process outlives the grace period.
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

# Wait up to 10 s for pids to exit; send SIGKILL to any that survive.
wait_for_pids() {
    local label="$1"; shift
    local pids=("$@")
    [[ ${#pids[@]} -eq 0 ]] && return 0

    printf "Waiting for %s to exit..." "$label"
    for _i in $(seq 1 20); do
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

    # Escalate: SIGKILL any process that did not exit within the grace period.
    for pid in "${pids[@]}"; do
        if kill -0 "$pid" 2>/dev/null; then
            kill -KILL "$pid" 2>/dev/null || true
            printf "  killed  PID %d (did not exit within grace period)\n" "$pid"
        fi
    done
    echo ""
}

# ---------------------------------------------------------------------------
# Discover domain services from .env (same logic as start-services.sh)
# ---------------------------------------------------------------------------

domain_services=()
if [[ -f "$ENV_FILE" ]]; then
    while IFS='=' read -r key _value; do
        [[ "$key" =~ ^ORES_(.+)_SERVICE_DB_USER$ ]] || continue
        component="${BASH_REMATCH[1],,}"
        component="${component//_/.}"
        svc="ores.$component.service"
        [[ "$svc" == "ores.iam.service" ]] && continue
        domain_services+=("$svc")
    done < "$ENV_FILE"
fi

# ---------------------------------------------------------------------------
# Phase 0: compute wrapper nodes (stopped first — outermost layer)
# ---------------------------------------------------------------------------

echo "[Phase 0: compute wrapper nodes]"
phase0_pids=()
for n in 1 2 3 4 5; do
    stop_service "ores.compute.wrapper.node${n}"
    [[ -n "$STOP_PID" ]] && phase0_pids+=("$STOP_PID")
done
echo ""
wait_for_pids "compute wrapper nodes" "${phase0_pids[@]}"

# ---------------------------------------------------------------------------
# Phase 1: front-end servers (WT and HTTP)
# ---------------------------------------------------------------------------

echo "[Phase 1: front-end servers]"
phase1_pids=()
for name in ores.wt.service ores.http.server; do
    stop_service "$name"
    [[ -n "$STOP_PID" ]] && phase1_pids+=("$STOP_PID")
done
echo ""
wait_for_pids "front-end servers" "${phase1_pids[@]}"

# ---------------------------------------------------------------------------
# Phase 2: domain services (all except IAM)
# ---------------------------------------------------------------------------

echo "[Phase 2: domain services]"
phase2_pids=()
for name in "${domain_services[@]}"; do
    stop_service "$name"
    [[ -n "$STOP_PID" ]] && phase2_pids+=("$STOP_PID")
done
echo ""
wait_for_pids "domain services" "${phase2_pids[@]}"

# ---------------------------------------------------------------------------
# Phase 3: IAM (stopped last — it is the JWKS provider)
# ---------------------------------------------------------------------------

echo "[Phase 3: IAM]"
phase3_pids=()
stop_service "ores.iam.service"
[[ -n "$STOP_PID" ]] && phase3_pids+=("$STOP_PID")
echo ""
wait_for_pids "IAM" "${phase3_pids[@]}"

# ---------------------------------------------------------------------------
# Fall back: any untracked ores.* processes still running from this bin dir
# ---------------------------------------------------------------------------

if [[ -d "$BIN_DIR" ]]; then
    untracked_pids=()
    while IFS= read -r pid; do
        exe="$(readlink -f /proc/"$pid"/exe 2>/dev/null || true)"
        [[ "$exe" == "$BIN_DIR"/ores.* ]] || continue
        name="$(basename "$exe")"
        kill -TERM "$pid" 2>/dev/null || continue
        printf "  stop    %-38s PID %d (untracked)\n" "$name" "$pid"
        untracked_pids+=("$pid")
        total_stopped=$((total_stopped + 1))
    done < <(pgrep -f "$BIN_DIR/ores\." 2>/dev/null || true)
    wait_for_pids "untracked services" "${untracked_pids[@]}"
fi

# ---------------------------------------------------------------------------
# Phase 4: NATS server (stopped last — all services must drain first)
# Only stopped if start-services.sh started it (PID file present).
# If NATS was started via prodigy or manually, this phase is a no-op.
# ---------------------------------------------------------------------------

CHECKOUT_LABEL="${ORES_CHECKOUT_LABEL:-local1}"
NATS_PID_FILE="$PROJECT_DIR/build/nats/$CHECKOUT_LABEL/nats-server.pid"

echo "[Phase 4: NATS server]"
if [[ -f "$NATS_PID_FILE" ]]; then
    nats_pid="$(cat "$NATS_PID_FILE")"
    rm -f "$NATS_PID_FILE"
    if kill -0 "$nats_pid" 2>/dev/null; then
        kill -TERM "$nats_pid"
        printf "  stop    %-38s PID %d\n" "nats-server" "$nats_pid"
        total_stopped=$((total_stopped + 1))
        echo ""
        wait_for_pids "NATS server" "$nats_pid"
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
