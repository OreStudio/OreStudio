#!/usr/bin/env bash
# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil; sh-basic-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# provision-nats.sh - Create or update required JetStream streams.
#
# Must be run after NATS is already running. Idempotent: existing streams
# are left unchanged. Called from start-services.sh before compute wrapper
# nodes are launched.
#
# Usage:
#   ./build/scripts/provision-nats.sh [--nats-url URL]
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

NATS_URL="${ORES_NATS_URL:-nats://localhost:4222}"
NATS_PREFIX="${ORES_NATS_SUBJECT_PREFIX:-ores.dev.local1}"
NATS_TLS_CA="${ORES_NATS_TLS_CA:-}"
KEYS_DIR="${CHECKOUT_ROOT}/build/keys/nats"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --nats-url)     NATS_URL="$2";     shift 2 ;;
        --nats-prefix)  NATS_PREFIX="$2";  shift 2 ;;
        --nats-tls-ca)  NATS_TLS_CA="$2";  shift 2 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

# Derive a safe stream name suffix from the prefix (dots → underscores, uppercase)
STREAM_SUFFIX="$(echo "${NATS_PREFIX}" | tr '.' '_' | tr '[:lower:]' '[:upper:]')"

# Verify nats CLI is available
if ! command -v nats &>/dev/null; then
    echo "Error: nats CLI not found. Install from https://github.com/nats-io/natscli" >&2
    exit 1
fi

# Build nats CLI command with optional mTLS flags.
# provision-nats.sh uses the CLI cert (same as ores.http.server) as a generic
# management client cert when mTLS is required.
NATS_CMD="nats --server ${NATS_URL}"
if [[ -n "$NATS_TLS_CA" ]]; then
    NATS_CMD+=" --tlsca ${NATS_TLS_CA}"
    NATS_CMD+=" --tlscert ${KEYS_DIR}/ores.http.server.crt"
    NATS_CMD+=" --tlskey ${KEYS_DIR}/ores.http.server.key"
fi

echo "[NATS provisioning]"

# ---------------------------------------------------------------------------
# ensure_stream NAME SUBJECTS
# Creates the stream if it does not already exist. Idempotent.
# ---------------------------------------------------------------------------
ensure_stream() {
    local name="$1"
    local subjects="$2"

    if ${NATS_CMD} stream info "${name}" &>/dev/null; then
        printf "  ok      stream %-30s (already exists)\n" "${name}"
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

    printf "  create  stream %-30s subjects: %s\n" "${name}" "${subjects}"
}

# ---------------------------------------------------------------------------
# Compute work assignment stream (per environment)
# The NATS client prepends the subject prefix to all subjects, so the actual
# subscribed subject is <prefix>.compute.v1.work.assignments.<tenant>.
# Stream name is environment-scoped to avoid collisions between checkouts.
# ---------------------------------------------------------------------------
ensure_stream \
    "ORES_COMPUTE_ASSIGNMENTS_${STREAM_SUFFIX}" \
    "${NATS_PREFIX}.compute.v1.work.assignments.>"

echo ""
