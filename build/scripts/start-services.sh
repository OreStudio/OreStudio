#!/usr/bin/env bash
# start-services.sh - Start all ORE Studio backend services for a given build preset.
#
# Usage:
#   ./build/scripts/start-services.sh [--preset PRESET] [--log-level LEVEL]
#
# Options:
#   --preset PRESET      CMake preset name (default: linux-clang-debug-ninja)
#   --log-level LEVEL    Log level: trace|debug|info|warn|error (default: trace)
#
# Environment:
#   ORES_PRESET          Overrides the default preset (--preset takes priority)
#
# Services are started in dependency order: IAM first (JWKS provider), then
# all other domain services in parallel, then HTTP and WT servers.
# PIDs are written to publish/run/ for use by stop-services.sh.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# --- Defaults ---
PRESET="${ORES_PRESET:-linux-clang-debug-ninja}"
LOG_LEVEL="trace"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --preset)    PRESET="$2";    shift 2 ;;
        --log-level) LOG_LEVEL="$2"; shift 2 ;;
        -h|--help)
            sed -n '/^# /s/^# \?//p' "$0" | head -15
            exit 0
            ;;
        *) echo "error: unknown option: $1"; exit 1 ;;
    esac
done

# --- Paths ---
BUILD_DIR="$PROJECT_DIR/build/output/$PRESET"
BIN_DIR="$BUILD_DIR/publish/bin"
LOG_DIR="$BUILD_DIR/publish/log"
RUN_DIR="$BUILD_DIR/publish/run"

if [[ ! -d "$BIN_DIR" ]]; then
    echo "error: binary directory not found: $BIN_DIR"
    echo "       cmake --build --preset $PRESET"
    exit 1
fi

# --- Load environment ---
ENV_FILE="$PROJECT_DIR/.env"
if [[ ! -f "$ENV_FILE" ]]; then
    echo "error: .env not found at $ENV_FILE"
    echo "       ./build/scripts/init-environment.sh"
    exit 1
fi

# Export all variables from .env into this shell. Values that contain literal
# \n (e.g. the JWT private key) keep them as two characters; we override the
# JWT key below using the actual key file so the IAM service receives real
# newlines.
set -a
# shellcheck disable=SC1090
source "$ENV_FILE"
set +a

# Override the JWT signing key from the PEM file (real newlines, not '\n').
KEY_FILE="$PROJECT_DIR/build/keys/iam-rsa-private.pem"
if [[ -f "$KEY_FILE" ]]; then
    ORES_IAM_SERVICE_JWT_PRIVATE_KEY="$(cat "$KEY_FILE")"
    export ORES_IAM_SERVICE_JWT_PRIVATE_KEY
fi

mkdir -p "$LOG_DIR" "$RUN_DIR"

# --- Derived configuration ---
CHECKOUT_LABEL="${ORES_CHECKOUT_LABEL:-local1}"
NATS_URL="${ORES_NATS_URL:-nats://localhost:4222}"
NATS_PREFIX="${ORES_NATS_SUBJECT_PREFIX:-ores.dev.$CHECKOUT_LABEL}"

# Port bases match ores-prodigy.el ores/port-bases.
declare -A PORT_BASES=(
    [remote]=50000 [local1]=51000 [local2]=52000
    [local3]=53000 [local4]=54000 [local5]=55000
)
BASE_PORT="${PORT_BASES[$CHECKOUT_LABEL]:-51000}"

# debug: offsets 0 (http) and 2 (wt); release: offsets 1 and 3.
if [[ "$PRESET" == *release* ]]; then
    HTTP_PORT=$((BASE_PORT + 1))
    WT_PORT=$((BASE_PORT + 3))
else
    HTTP_PORT=$((BASE_PORT + 0))
    WT_PORT=$((BASE_PORT + 2))
fi

# WT needs resources from vcpkg_installed in the build output directory.
export WT_RESOURCES_DIR="$BUILD_DIR/vcpkg_installed/x64-linux/share/Wt/resources"

# --- Helpers ---

is_running() {
    local pid_file="$RUN_DIR/$1.pid"
    [[ -f "$pid_file" ]] && kill -0 "$(cat "$pid_file")" 2>/dev/null
}

launch() {
    local name="$1"; shift
    local pid_file="$RUN_DIR/$name.pid"

    if is_running "$name"; then
        printf "  skip    %-38s PID %d\n" "$name" "$(cat "$pid_file")"
        return
    fi

    # cd into bin so that relative paths (../log) resolve correctly, then exec
    # replaces the subshell so $! refers to the actual service process.
    (cd "$BIN_DIR" && exec "./$name" "$@") &
    local pid=$!
    echo "$pid" > "$pid_file"
    printf "  start   %-38s PID %d\n" "$name" "$pid"
}

launch_nats_service() {
    local name="$1"
    launch "$name" \
        --log-enabled \
        --log-level "$LOG_LEVEL" \
        --log-directory ../log \
        --log-to-console \
        --nats-url "$NATS_URL" \
        --nats-subject-prefix "$NATS_PREFIX"
}

wait_for_ready() {
    local name="$1"
    local log_file="$LOG_DIR/$name.log"
    printf "  wait    %s" "$name"
    local i
    for i in $(seq 1 60); do
        if grep -q "Service ready" "$log_file" 2>/dev/null; then
            echo " ... ready"
            return 0
        fi
        sleep 0.5
        [[ $((i % 4)) -eq 0 ]] && printf "."
    done
    echo " ... timeout (check $log_file)"
    return 1
}

# Discover NATS domain services from .env (matches ores/nats-domain-services in
# ores-prodigy.el). Scans for ORES_*_SERVICE_DB_USER entries and derives binary
# names from the matched component (e.g. ASSETS -> ores.assets.service).
discover_nats_services() {
    local services=()
    while IFS='=' read -r key _value; do
        [[ "$key" =~ ^ORES_(.+)_SERVICE_DB_USER$ ]] || continue
        local component="${BASH_REMATCH[1]}"
        # lowercase and replace _ with . to get binary name component
        component="${component,,}"
        component="${component//_/.}"
        services+=("ores.$component.service")
    done < "$ENV_FILE"
    printf '%s\n' "${services[@]}"
}

# ============================================================
echo "Starting ORE Studio services"
echo "  Preset : $PRESET"
echo "  NATS   : $NATS_URL (prefix: $NATS_PREFIX)"
echo "  Ports  : HTTP=$HTTP_PORT  WT=$WT_PORT"
echo ""

# 1. IAM first — all other services fetch JWKS from it on startup.
echo "[IAM]"
launch_nats_service ores.iam.service
wait_for_ready ores.iam.service
echo ""

# 2. All other NATS domain services (discovered from .env).
echo "[Domain services]"
while IFS= read -r svc; do
    [[ "$svc" == "ores.iam.service" ]] && continue
    [[ -x "$BIN_DIR/$svc" ]] || { echo "  miss    $svc (binary not found, skipping)"; continue; }
    launch_nats_service "$svc"
done < <(discover_nats_services)
echo ""

# 3. HTTP server
echo "[HTTP server]"
launch ores.http.server \
    --log-enabled \
    --log-level "$LOG_LEVEL" \
    --log-directory ../log \
    --port "$HTTP_PORT"
echo ""

# 4. WT server
echo "[WT server]"
launch ores.wt.service \
    --log-enabled \
    --log-level "$LOG_LEVEL" \
    --log-directory ../log \
    --http-address 0.0.0.0 \
    --docroot . \
    --http-port "$WT_PORT"
echo ""

echo "Logs : $LOG_DIR"
echo "Stop : ./build/scripts/stop-services.sh --preset $PRESET"
