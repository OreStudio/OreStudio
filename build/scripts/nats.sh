#!/usr/bin/env bash
# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil; sh-basic-offset: 4 -*-
#
# nats.sh - NATS CLI wrapper for this environment.
#
# Pre-fills the server URL and mTLS certificates from .env so you can run
# nats commands without repeating connection arguments every time.
#
# Usage (from any directory):
#   ./build/scripts/nats.sh stream ls
#   ./build/scripts/nats.sh stream delete STREAM_NAME
#   ./build/scripts/nats.sh pub subject "payload"
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

exec nats \
    --server "${ORES_NATS_URL}" \
    --tlsca  "${ORES_NATS_TLS_CA}" \
    --tlscert "${ORES_NATS_TLS_CERT}" \
    --tlskey  "${ORES_NATS_TLS_KEY}" \
    "$@"
