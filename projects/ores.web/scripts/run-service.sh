#!/usr/bin/env bash

# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

# Start a C++ service against the local NATS, with that service's own client
# certificate.
#
# The repository root comes from this script's location, so the checkout that
# owns the script is the checkout that runs. Each service presents a distinct
# certificate, so the certificate is read from build/keys/nats rather than from
# .env; that file names the shell client's. The NATS URL, the subject prefix
# and the database credentials do come from .env.
#
# Usage:
#   scripts/run-service.sh <service> [extra args...]
#
# Example:
#   scripts/run-service.sh iam --tenant ffffffff-ffff-ffff-ffff-ffffffffffff
#   scripts/run-service.sh refdata
set -euo pipefail

SERVICE="${1:?usage: run-service.sh <service> [args...]}"
shift

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$WORKSPACE/../.." && pwd)"
ENV_FILE="$REPO_ROOT/.env"
LOG_DIR="${LOG_DIR:-$WORKSPACE/.runtime/logs}"

env_value() {
  sed -n "s/^$1=//p" "$ENV_FILE" | head -1
}

PRESET="$(env_value ORES_PRESET)"
PRESET="${PRESET:-linux-clang-debug-make}"
KEYS="$REPO_ROOT/build/keys/nats"
BIN="$REPO_ROOT/build/output/$PRESET/publish/bin"

NATS_URL="$(env_value ORES_NATS_URL)"
NATS_URL="${NATS_URL:-nats://localhost:4222}"
NATS_SUBJECT_PREFIX="$(env_value ORES_NATS_SUBJECT_PREFIX)"
if [[ -z "$NATS_SUBJECT_PREFIX" ]]; then
  echo "no ORES_NATS_SUBJECT_PREFIX in $ENV_FILE" >&2
  exit 1
fi

BINARY="$BIN/ores.$SERVICE.service"
if [[ ! -x "$BINARY" ]]; then
  echo "no such service binary: $BINARY" >&2
  exit 1
fi

CERT="$KEYS/ores.$SERVICE.service.crt"
KEY="$KEYS/ores.$SERVICE.service.key"
if [[ ! -r "$CERT" || ! -r "$KEY" ]]; then
  echo "missing certificate for $SERVICE: $CERT" >&2
  exit 1
fi

# The mapper prefix is the service name upper-cased with dashes as
# underscores, then `_SERVICE`: the IAM service reads ORES_IAM_SERVICE_DB_*
# and refdata reads ORES_REFDATA_SERVICE_DB_*.
PREFIX="$(printf '%s' "$SERVICE" | tr '[:lower:]-' '[:upper:]_')_SERVICE"
DB_USER="$(env_value "ORES_${PREFIX}_DB_USER")"
DB_PASSWORD="$(env_value "ORES_${PREFIX}_DB_PASSWORD")"
DB_DATABASE="$(env_value "ORES_${PREFIX}_DB_DATABASE")"
DB_HOST="$(env_value "ORES_${PREFIX}_DB_HOST")"
DB_PORT="$(env_value "ORES_${PREFIX}_DB_PORT")"

if [[ -z "$DB_USER" ]]; then
  echo "no database user for $SERVICE in $ENV_FILE" >&2
  exit 1
fi

export "ORES_${PREFIX}_DB_USER=$DB_USER"
export "ORES_${PREFIX}_DB_PASSWORD=$DB_PASSWORD"
export "ORES_${PREFIX}_DB_DATABASE=$DB_DATABASE"
export "ORES_${PREFIX}_DB_HOST=$DB_HOST"
export "ORES_${PREFIX}_DB_PORT=$DB_PORT"

# The IAM service signs tokens; its key is one quoted line with escaped
# newlines that has to be unquoted and expanded.
if [[ "$SERVICE" == "iam" ]]; then
  JWT_RAW="$(env_value ORES_IAM_SERVICE_JWT_PRIVATE_KEY)"
  export ORES_IAM_SERVICE_JWT_PRIVATE_KEY="$(
    printf '%s' "$JWT_RAW" | sed -e 's/^"//' -e 's/"$//' | sed 's/\\n/\n/g'
  )"
fi

mkdir -p "$LOG_DIR"

echo "$SERVICE: $NATS_URL prefix $NATS_SUBJECT_PREFIX msgpack db=$DB_USER"
exec "$BINARY" \
  --nats-url "$NATS_URL" \
  --nats-subject-prefix "$NATS_SUBJECT_PREFIX" \
  --nats-wire-format msgpack \
  --nats-tls-ca "$KEYS/ca.crt" \
  --nats-tls-cert "$CERT" \
  --nats-tls-key "$KEY" \
  --log-enabled \
  --log-to-console \
  --log-directory "$LOG_DIR" \
  --log-filename "ores.$SERVICE.service.log" \
  "$@"
