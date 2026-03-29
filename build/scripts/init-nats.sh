#!/usr/bin/env bash
# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil; sh-basic-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# init-nats.sh - Per-environment NATS initialisation.
#
# Generates a per-environment nats-server configuration file from the
# template at build/config/nats.conf.template, creates the JetStream store
# directory, and optionally starts nats-server and provisions streams.
#
# All settings are read from the .env file (written by init-environment.sh).
# The generated config is written to build/config/nats-<label>.conf and is
# gitignored — it contains absolute paths specific to this checkout.
#
# Dependencies: bash 3.2+, sed, nats-server (for --start), nats CLI (for --provision)
#
# Usage:
#   ./build/scripts/init-nats.sh                    # generate config + store dir
#   ./build/scripts/init-nats.sh --provision        # + wait for NATS + provision streams
#   ./build/scripts/init-nats.sh --start --daemon   # generate + start nats-server in bg
#   ./build/scripts/init-nats.sh --start --daemon --provision  # full one-shot init
#
# Flags:
#   --provision        Provision required JetStream streams (requires NATS running)
#   --start            Start nats-server with the generated config
#   --daemon           With --start: run nats-server in background
#   --wait-timeout N   Seconds to wait for NATS to become ready (default: 30)
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV_FILE="${CHECKOUT_ROOT}/.env"

if [[ -f "${ENV_FILE}" ]]; then
    set -o allexport
    # shellcheck source=/dev/null
    source "${ENV_FILE}"
    set +o allexport
fi

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
DO_PROVISION=0
DO_START=0
DO_DAEMON=0
WAIT_TIMEOUT=30

while [[ $# -gt 0 ]]; do
    case "$1" in
        --provision)          DO_PROVISION=1 ;;
        --start)              DO_START=1 ;;
        --daemon)             DO_DAEMON=1 ;;
        --wait-timeout)       WAIT_TIMEOUT="$2"; shift ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
    shift
done

# ---------------------------------------------------------------------------
# Resolve settings from .env (with sensible fallbacks)
# ---------------------------------------------------------------------------
LABEL="${ORES_CHECKOUT_LABEL:-$(basename "${CHECKOUT_ROOT}")}"
NATS_PORT="${ORES_NATS_PORT:-4222}"
NATS_MONITOR_PORT="${ORES_NATS_MONITOR_PORT:-8222}"
NATS_STORE_DIR="${ORES_NATS_STORE_DIR:-${CHECKOUT_ROOT}/build/nats/${LABEL}/jetstream}"
NATS_URL="${ORES_NATS_URL:-nats://localhost:${NATS_PORT}}"
NATS_PREFIX="${ORES_NATS_SUBJECT_PREFIX:-ores.dev.${LABEL}}"
NATS_TLS_CA="${ORES_NATS_TLS_CA:-}"
KEYS_DIR="${CHECKOUT_ROOT}/build/keys/nats"

CONFIG_TEMPLATE="${CHECKOUT_ROOT}/build/config/nats.conf.template"
CONFIG_OUT="${CHECKOUT_ROOT}/build/config/nats-${LABEL}.conf"

echo "=== NATS init for environment '${LABEL}' ==="
echo "  Port:         ${NATS_PORT}"
echo "  Monitor port: ${NATS_MONITOR_PORT}"
echo "  Store dir:    ${NATS_STORE_DIR}"
echo "  Config:       ${CONFIG_OUT}"
echo ""

# ---------------------------------------------------------------------------
# Phase 1: Generate per-environment nats-server config
# ---------------------------------------------------------------------------
echo "--- Generating ${CONFIG_OUT} ---"

if [[ ! -f "${CONFIG_TEMPLATE}" ]]; then
    echo "Error: template not found: ${CONFIG_TEMPLATE}" >&2
    exit 1
fi

sed \
    -e "s|{{LABEL}}|${LABEL}|g" \
    -e "s|{{NATS_PORT}}|${NATS_PORT}|g" \
    -e "s|{{NATS_MONITOR_PORT}}|${NATS_MONITOR_PORT}|g" \
    -e "s|{{NATS_STORE_DIR}}|${NATS_STORE_DIR}|g" \
    -e "s|{{CHECKOUT_ROOT}}|${CHECKOUT_ROOT}|g" \
    "${CONFIG_TEMPLATE}" > "${CONFIG_OUT}"

echo "  Written: ${CONFIG_OUT}"

# ---------------------------------------------------------------------------
# Phase 2: Create JetStream store directory
# ---------------------------------------------------------------------------
echo "--- Creating JetStream store directory ---"
mkdir -p "${NATS_STORE_DIR}"
echo "  Ready: ${NATS_STORE_DIR}"

# ---------------------------------------------------------------------------
# Phase 3: Start nats-server (optional)
# ---------------------------------------------------------------------------
if [[ "${DO_START}" -eq 1 ]]; then
    if ! command -v nats-server &>/dev/null; then
        echo "Error: nats-server not found in PATH." >&2
        exit 1
    fi

    echo "--- Starting nats-server ---"
    if [[ "${DO_DAEMON}" -eq 1 ]]; then
        nats-server --config "${CONFIG_OUT}" --pid "${CHECKOUT_ROOT}/build/nats/${LABEL}/nats-server.pid" &
        echo "  nats-server started in background (PID $!)"
    else
        echo "  Starting nats-server in foreground (Ctrl-C to stop)..."
        exec nats-server --config "${CONFIG_OUT}"
    fi
fi

# ---------------------------------------------------------------------------
# Phase 4: Provision JetStream streams (optional)
# ---------------------------------------------------------------------------
if [[ "${DO_PROVISION}" -eq 1 ]]; then
    if ! command -v nats &>/dev/null; then
        echo "Error: nats CLI not found. Install from https://github.com/nats-io/natscli" >&2
        exit 1
    fi

    # Build base nats CLI command with optional mTLS
    NATS_CMD="nats --server ${NATS_URL}"
    if [[ -n "${NATS_TLS_CA}" ]]; then
        NATS_CMD+=" --tlsca ${NATS_TLS_CA}"
        NATS_CMD+=" --tlscert ${KEYS_DIR}/ores.http.server.crt"
        NATS_CMD+=" --tlskey ${KEYS_DIR}/ores.http.server.key"
    fi

    # Wait for NATS to become ready
    echo "--- Waiting for NATS to be ready (timeout: ${WAIT_TIMEOUT}s) ---"
    elapsed=0
    while ! ${NATS_CMD} server ping &>/dev/null; do
        if [[ ${elapsed} -ge ${WAIT_TIMEOUT} ]]; then
            echo "Error: NATS did not become ready within ${WAIT_TIMEOUT}s" >&2
            exit 1
        fi
        sleep 1
        elapsed=$((elapsed + 1))
    done
    echo "  NATS is ready."

    # Derive a safe stream name suffix from the prefix (dots/hyphens → underscores, uppercase)
    STREAM_SUFFIX="$(echo "${NATS_PREFIX}" | tr '.,-' '_' | tr '[:lower:]' '[:upper:]')"

    echo "--- Provisioning JetStream streams ---"

    ensure_stream() {
        local name="$1"
        local subjects="$2"
        if ${NATS_CMD} stream info "${name}" &>/dev/null; then
            printf "  ok      stream %-40s (already exists)\n" "${name}"
            return
        fi
        ${NATS_CMD} stream add "${name}" \
            --subjects="${subjects}" \
            --storage=file \
            --retention=limits \
            --max-age=24h \
            --max-msgs=-1 \
            --max-bytes=-1 \
            --replicas=1 \
            --defaults \
            2>/dev/null
        printf "  created stream %-40s subjects: %s\n" "${name}" "${subjects}"
    }

    ensure_stream \
        "ORES_COMPUTE_ASSIGNMENTS_${STREAM_SUFFIX}" \
        "${NATS_PREFIX}.compute.v1.work.assignments.>"

    echo ""
fi

echo "=== NATS init complete for '${LABEL}' ==="
if [[ "${DO_START}" -eq 0 ]]; then
    echo ""
    echo "Next: start nats-server with:"
    echo "  nats-server --config ${CONFIG_OUT}"
    echo "  or: ./build/scripts/init-nats.sh --start --daemon --provision"
fi
