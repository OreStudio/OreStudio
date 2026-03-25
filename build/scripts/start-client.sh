#!/usr/bin/env bash
# start-client.sh - Start the ORE Studio Qt client.
#
# Usage:
#   ./build/scripts/start-client.sh [--preset PRESET] [--colour COLOUR] [--name NAME] [--log-level LEVEL]
#
# Options:
#   --preset PRESET    CMake preset name (overrides ORES_PRESET from .env)
#   --colour COLOUR    Instance colour: red, green, blue, or a 6-digit hex value (e.g. FF5733)
#   --name NAME        Instance display name (defaults to colour name, or hex value if custom colour)
#   --log-level LEVEL  Log level: trace|debug|info|warn|error (default: debug)
#
# Named colours: red=F44336  green=4CAF50  blue=2196F3
# Multiple instances can be started simultaneously with different colours.
#
# The build preset defaults to ORES_PRESET in .env (set by init-environment.sh).
# Use --preset to start against a specific preset without re-running init-environment.sh.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

# --- Defaults ---
PRESET_ARG=""
COLOUR=""
INSTANCE_NAME=""
LOG_LEVEL="debug"

while [[ $# -gt 0 ]]; do
    case "$1" in
        --preset)         PRESET_ARG="$2";    shift 2 ;;
        --colour|--color) COLOUR="$2";        shift 2 ;;
        --name)           INSTANCE_NAME="$2"; shift 2 ;;
        --log-level)      LOG_LEVEL="$2";     shift 2 ;;
        -h|--help)
            sed -n '/^# /s/^# \?//p' "$0" | head -17
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

# --- Paths ---
BUILD_DIR="$PROJECT_DIR/build/output/$PRESET"
BIN_DIR="$BUILD_DIR/publish/bin"
RUN_DIR="$BUILD_DIR/publish/run"

if [[ ! -f "$BIN_DIR/ores.qt" ]]; then
    echo "error: ores.qt not found in $BIN_DIR"
    echo "       cmake --build --preset $PRESET"
    exit 1
fi

mkdir -p "$RUN_DIR"

# --- Resolve colour ---
# COLOUR_HEX: the hex value passed to --instance-color (no '#')
# COLOUR_TAG:  used to name the PID file and log file
COLOUR_HEX=""
COLOUR_TAG=""

case "${COLOUR,,}" in
    "")
        ;;
    red)
        COLOUR_HEX="F44336"
        COLOUR_TAG="red"
        [[ -z "$INSTANCE_NAME" ]] && INSTANCE_NAME="Red"
        ;;
    green)
        COLOUR_HEX="4CAF50"
        COLOUR_TAG="green"
        [[ -z "$INSTANCE_NAME" ]] && INSTANCE_NAME="Green"
        ;;
    blue)
        COLOUR_HEX="2196F3"
        COLOUR_TAG="blue"
        [[ -z "$INSTANCE_NAME" ]] && INSTANCE_NAME="Blue"
        ;;
    [0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])
        COLOUR_HEX="${COLOUR^^}"
        COLOUR_TAG="${COLOUR,,}"
        [[ -z "$INSTANCE_NAME" ]] && INSTANCE_NAME="$COLOUR_HEX"
        ;;
    *)
        echo "error: unknown colour '$COLOUR'"
        echo "       use: red, green, blue, or a 6-digit hex value (e.g. FF5733)"
        exit 1
        ;;
esac

# --- PID and log file names ---
if [[ -n "$COLOUR_TAG" ]]; then
    PID_NAME="ores.qt.${COLOUR_TAG}"
    LOG_FILE="ores.qt.${COLOUR_TAG}.log"
else
    PID_NAME="ores.qt"
    LOG_FILE="ores.qt.log"
fi

PID_FILE="$RUN_DIR/$PID_NAME.pid"

# --- Check already running ---
if [[ -f "$PID_FILE" ]] && kill -0 "$(cat "$PID_FILE")" 2>/dev/null; then
    printf "  skip    %-38s PID %d\n" "$PID_NAME" "$(cat "$PID_FILE")"
    exit 0
fi

# --- Build argument list ---
ARGS=(
    --log-enabled
    --log-level "$LOG_LEVEL"
    --log-directory ../log
    --log-filename "$LOG_FILE"
    --compression-enabled
)

if [[ -n "$INSTANCE_NAME" ]]; then
    ARGS+=(--instance-name "$INSTANCE_NAME")
fi
if [[ -n "$COLOUR_HEX" ]]; then
    ARGS+=(--instance-color "$COLOUR_HEX")
fi

# --- Launch ---
(cd "$BIN_DIR" && exec "./ores.qt" "${ARGS[@]}") &
PID=$!
echo "$PID" > "$PID_FILE"
printf "  start   %-38s PID %d\n" "$PID_NAME" "$PID"
echo ""
echo "Logs : $BUILD_DIR/publish/log/$LOG_FILE"
echo "Stop : kill \$(cat $PID_FILE)"
