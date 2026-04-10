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
        --wait-timeout)
            if [[ $# -lt 2 || "$2" =~ ^- ]]; then
                echo "Error: --wait-timeout requires a numeric value." >&2; exit 1
            fi
            WAIT_TIMEOUT="$2"; shift ;;
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
        echo "Error: nats-server not found (tried PATH, /usr/sbin, /sbin, /usr/local/sbin)" >&2
        exit 1
    fi

    echo "--- Starting nats-server ($NATS_SERVER_BIN) ---"
    if [[ "${DO_DAEMON}" -eq 1 ]]; then
        "$NATS_SERVER_BIN" --config "${CONFIG_OUT}" --pid "${CHECKOUT_ROOT}/build/nats/${LABEL}/nats-server.pid" &
        echo "  nats-server started in background (PID $!)"
    else
        echo "  Starting nats-server in foreground (Ctrl-C to stop)..."
        exec "$NATS_SERVER_BIN" --config "${CONFIG_OUT}"
    fi
fi

# ---------------------------------------------------------------------------
# Phase 4: JetStream streams
# ---------------------------------------------------------------------------
# Streams are self-provisioned by each service on startup (idempotent).
# No external provisioning step is required or supported.
if [[ "${DO_PROVISION}" -eq 1 ]]; then
    echo "--- JetStream streams: self-provisioned by services on startup ---"
    echo "    No manual provisioning required."
    echo ""
fi

echo "=== NATS init complete for '${LABEL}' ==="
if [[ "${DO_START}" -eq 0 ]]; then
    echo ""
    echo "Next: start nats-server with:"
    echo "  nats-server --config ${CONFIG_OUT}"
    echo "  or: ./build/scripts/init-nats.sh --start --daemon --provision"
fi
