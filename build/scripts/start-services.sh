#!/usr/bin/env bash
# start-services.sh - Start all ORE Studio backend services for a given build preset.
#
# Usage:
#   ./build/scripts/start-services.sh [--preset PRESET] [--log-level LEVEL]
#
# Options:
#   --preset PRESET      CMake preset name (overrides ORES_PRESET from .env)
#   --log-level LEVEL    Log level: trace|debug|info|warn|error (default: trace)
#
# The build preset defaults to ORES_PRESET in .env (set by init-environment.sh).
# Use --preset to switch presets without re-running init-environment.sh.
#
# Start order: NATS → controller (which spawns IAM then all domain services)
#              → HTTP server → WT server → compute wrapper nodes.
# PIDs are written to publish/run/ for use by stop-services.sh.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# --- Defaults ---
PRESET_ARG=""
LOG_LEVEL="trace"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --preset)    [[ -z "${2:-}" || "$2" == -* ]] && { echo "error: $1 requires a value" >&2; exit 1; }; PRESET_ARG="$2"; shift 2 ;;
        --log-level) [[ -z "${2:-}" || "$2" == -* ]] && { echo "error: $1 requires a value" >&2; exit 1; }; LOG_LEVEL="$2";  shift 2 ;;
        -h|--help)
            sed -n '/^# /s/^# \?//p' "$0" | head -13
            exit 0
            ;;
        *) echo "error: unknown option: $1"; exit 1 ;;
    esac
done

# --- Load environment ---
ENV_FILE="$PROJECT_DIR/.env"
if [[ ! -f "$ENV_FILE" ]]; then
    echo "error: .env not found at $ENV_FILE"
    echo "       run: ./build/scripts/init-environment.sh --preset <preset>"
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

# --preset flag takes priority; fall back to ORES_PRESET from .env.
PRESET="${PRESET_ARG:-${ORES_PRESET:-}}"
if [[ -z "$PRESET" ]]; then
    echo "error: no preset — pass --preset <preset> or set ORES_PRESET via init-environment.sh"
    exit 1
fi

# Override the JWT signing key from the PEM file (real newlines, not '\n').
KEY_FILE="$PROJECT_DIR/build/keys/iam-rsa-private.pem"
if [[ -f "$KEY_FILE" ]]; then
    ORES_IAM_SERVICE_JWT_PRIVATE_KEY="$(cat "$KEY_FILE")"
    export ORES_IAM_SERVICE_JWT_PRIVATE_KEY
fi

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

mkdir -p "$LOG_DIR" "$RUN_DIR"

# --- Locate nats-server (Debian installs it in /usr/sbin) ---
NATS_SERVER_BIN=""
for _candidate in \
        "$(command -v nats-server 2>/dev/null || true)" \
        /usr/sbin/nats-server \
        /sbin/nats-server \
        /usr/local/sbin/nats-server \
        /usr/local/bin/nats-server; do
    [[ -x "$_candidate" ]] && NATS_SERVER_BIN="$_candidate" && break
done
if [[ -z "$NATS_SERVER_BIN" ]]; then
    echo "error: nats-server not found (tried PATH, /usr/sbin, /sbin, /usr/local/sbin)"
    exit 1
fi
NATS_SERVER_VERSION="$("$NATS_SERVER_BIN" --version 2>&1 | head -1 || true)"
echo "  nats-server: $NATS_SERVER_BIN ($NATS_SERVER_VERSION)"

# --- Derived configuration ---
CHECKOUT_LABEL="${ORES_CHECKOUT_LABEL:-local1}"
NATS_PORT="${ORES_NATS_PORT:-4222}"
NATS_URL="${ORES_NATS_URL:-nats://localhost:$NATS_PORT}"
NATS_PREFIX="${ORES_NATS_SUBJECT_PREFIX:-ores.dev.$CHECKOUT_LABEL}"
NATS_CONFIG="$PROJECT_DIR/build/config/nats-$CHECKOUT_LABEL.conf"
NATS_PID_FILE="$PROJECT_DIR/build/nats/$CHECKOUT_LABEL/nats-server.pid"

# mTLS: optional; all three must be set together or all left empty.
NATS_TLS_CA="${ORES_NATS_TLS_CA:-}"
KEYS_DIR="$PROJECT_DIR/build/keys/nats"

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

# Like launch() but uses $binary as the executable while $name is used for
# PID file and display only. Needed when multiple instances share one binary.
launch_binary() {
    local name="$1"; local binary="$2"; shift 2
    local pid_file="$RUN_DIR/$name.pid"

    if is_running "$name"; then
        printf "  skip    %-38s PID %d\n" "$name" "$(cat "$pid_file")"
        return
    fi

    (cd "$BIN_DIR" && exec "./$binary" "$@") &
    local pid=$!
    echo "$pid" > "$pid_file"
    printf "  start   %-38s PID %d\n" "$name" "$pid"
}

launch_nats_service() {
    local name="$1"
    # Derive the service cert/key from the binary name (e.g. ores.iam.service).
    local tls_args=()
    if [[ -n "$NATS_TLS_CA" ]]; then
        tls_args+=(
            --nats-tls-ca   "$KEYS_DIR/ca.crt"
            --nats-tls-cert "$KEYS_DIR/${name}.crt"
            --nats-tls-key  "$KEYS_DIR/${name}.key"
        )
    fi
    launch "$name" \
        --log-enabled \
        --log-level "$LOG_LEVEL" \
        --log-directory ../log \
        --nats-url "$NATS_URL" \
        --nats-subject-prefix "$NATS_PREFIX" \
        "${tls_args[@]}"
}

launch_wrapper_node() {
    local n="$1"
    local host_id_var="ORES_GRID_NODE_${n}_HOST_ID"
    local host_id="${!host_id_var:-}"
    if [[ -z "$host_id" ]]; then
        echo "  skip    ores.compute.wrapper.node${n} (no host ID in .env)"
        return
    fi
    local work_dir="$RUN_DIR/wrappers/node_${n}"
    mkdir -p "$work_dir"
    local name="ores.compute.wrapper.node${n}"
    local wrapper_tls_args=()
    if [[ -n "$NATS_TLS_CA" ]]; then
        wrapper_tls_args+=(
            --nats-tls-ca   "$KEYS_DIR/ca.crt"
            --nats-tls-cert "$KEYS_DIR/ores.compute.wrapper.crt"
            --nats-tls-key  "$KEYS_DIR/ores.compute.wrapper.key"
        )
    fi
    launch_binary "$name" "ores.compute.wrapper" \
        --log-enabled \
        --log-level "$LOG_LEVEL" \
        --log-directory ../log \
        --log-filename "${name}.log" \
        --nats-url "$NATS_URL" \
        --nats-subject-prefix "$NATS_PREFIX" \
        --host-id "$host_id" \
        --tenant-id "$NATS_PREFIX" \
        --work-dir "$work_dir" \
        --http-base-url "http://localhost:$HTTP_PORT" \
        "${wrapper_tls_args[@]}"
}

wait_for_nats() {
    local port="$1"
    printf "  wait    nats-server (port %d)" "$port"
    local i
    for i in $(seq 1 60); do
        if (echo > /dev/tcp/localhost/"$port") 2>/dev/null; then
            echo " ... ready"
            return 0
        fi
        sleep 0.5
        [[ $((i % 4)) -eq 0 ]] && printf "."
    done
    echo " ... timeout (check nats-server logs)"
    return 1
}

wait_for_ready() {
    local name="$1"
    local log_file="$LOG_DIR/$name.log"
    # Snapshot size before launch. If the service recreates (truncates) its log,
    # the new size will be smaller — reset to 0 so we search from the beginning.
    local start_pos=0
    [[ -f "$log_file" ]] && start_pos=$(wc -c < "$log_file")
    printf "  wait    %s" "$name"
    local i
    for i in $(seq 1 60); do
        # Detect log truncation (service recreated file on startup)
        local cur_size=0
        [[ -f "$log_file" ]] && cur_size=$(wc -c < "$log_file")
        [[ "$cur_size" -lt "$start_pos" ]] && start_pos=0
        if tail -c "+$((start_pos + 1))" "$log_file" 2>/dev/null | grep -q "Service ready"; then
            echo " ... ready"
            return 0
        fi
        sleep 0.5
        [[ $((i % 4)) -eq 0 ]] && printf "."
    done
    echo " ... timeout (check $log_file)"
    return 1
}

# ============================================================
echo "Starting ORE Studio services"
echo "  Preset : $PRESET"
TLS_STATUS="${NATS_TLS_CA:+enabled}"
echo "  NATS   : $NATS_URL (prefix: $NATS_PREFIX, mTLS: ${TLS_STATUS:-disabled})"
echo "  Ports  : HTTP=$HTTP_PORT  WT=$WT_PORT"
echo ""

# 0. NATS server — start if not already running.
echo "[NATS server]"
if [[ -f "$NATS_PID_FILE" ]] && kill -0 "$(cat "$NATS_PID_FILE")" 2>/dev/null; then
    printf "  skip    %-38s PID %d\n" "nats-server" "$(cat "$NATS_PID_FILE")"
else
    if [[ ! -f "$NATS_CONFIG" ]]; then
        echo "  error: NATS config not found: $NATS_CONFIG"
        echo "         run: ./build/scripts/init-nats.sh"
        exit 1
    fi
    mkdir -p "$(dirname "$NATS_PID_FILE")"
    NATS_LOG="$LOG_DIR/nats-server.log"
    "$NATS_SERVER_BIN" --config "$NATS_CONFIG" -l "$NATS_LOG" &
    echo "$!" > "$NATS_PID_FILE"
    printf "  start   %-38s PID %d\n" "nats-server" "$!"
fi
wait_for_nats "$NATS_PORT"
echo ""

# 1. Controller — spawns IAM and all domain services.
echo "[Controller]"
controller_tls_args=()
if [[ -n "$NATS_TLS_CA" ]]; then
    controller_tls_args+=(
        --nats-tls-ca   "$KEYS_DIR/ca.crt"
        --nats-tls-cert "$KEYS_DIR/ores.controller.service.crt"
        --nats-tls-key  "$KEYS_DIR/ores.controller.service.key"
    )
fi
launch ores.controller.service \
    --log-enabled \
    --log-level "$LOG_LEVEL" \
    --log-directory ../log \
    --nats-url "$NATS_URL" \
    --nats-subject-prefix "$NATS_PREFIX" \
    "${controller_tls_args[@]}"
echo ""

# 3. HTTP server
echo "[HTTP server]"
http_tls_args=()
if [[ -n "$NATS_TLS_CA" ]]; then
    http_tls_args+=(
        --nats-tls-ca   "$KEYS_DIR/ca.crt"
        --nats-tls-cert "$KEYS_DIR/ores.http.server.crt"
        --nats-tls-key  "$KEYS_DIR/ores.http.server.key"
    )
fi
launch ores.http.server \
    --log-enabled \
    --log-level "$LOG_LEVEL" \
    --log-directory ../log \
    --port "$HTTP_PORT" \
    --nats-url "$NATS_URL" \
    --nats-subject-prefix "$NATS_PREFIX" \
    --storage-dir ../storage \
    "${http_tls_args[@]}"
echo ""

# 4. WT server
echo "[WT server]"
wt_tls_args=()
if [[ -n "$NATS_TLS_CA" ]]; then
    wt_tls_args+=(
        --nats-tls-ca   "$KEYS_DIR/ca.crt"
        --nats-tls-cert "$KEYS_DIR/ores.wt.service.crt"
        --nats-tls-key  "$KEYS_DIR/ores.wt.service.key"
    )
fi
launch ores.wt.service \
    --log-enabled \
    --log-level "$LOG_LEVEL" \
    --log-directory ../log \
    "${wt_tls_args[@]}" \
    -- \
    --http-address 0.0.0.0 \
    --docroot . \
    --http-port "$WT_PORT"
echo ""

# 5. Compute wrapper nodes (test environment grid)
if [[ -x "$BIN_DIR/ores.compute.wrapper" ]]; then
    # Provision JetStream streams before launching nodes.
    provision_tls_args=()
    [[ -n "$NATS_TLS_CA" ]] && provision_tls_args+=(--nats-tls-ca "$NATS_TLS_CA")
    "$SCRIPT_DIR/provision-nats.sh" --nats-url "$NATS_URL" --nats-prefix "$NATS_PREFIX" "${provision_tls_args[@]}"

    echo "[Compute wrapper nodes]"
    for n in 1 2 3 4 5; do
        launch_wrapper_node "$n"
    done
    echo ""
fi

echo "Logs : $LOG_DIR"
echo "Stop : ./build/scripts/stop-services.sh"
