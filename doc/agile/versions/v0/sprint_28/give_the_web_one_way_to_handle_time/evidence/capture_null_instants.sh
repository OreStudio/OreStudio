#!/usr/bin/env bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
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
# Captures the wire bytes that prove an absent instant is null, end to end,
# against the running fleet. Run it from the repository root after
# `compass services start` and the Acme provisioning script:
#
#   ./doc/agile/versions/v0/sprint_28/give_the_web_one_way_to_handle_time/evidence/capture_null_instants.sh
#
# It writes one file per endpoint beside itself, and prints the same bytes, so a
# reviewer compares what the fleet answered with what this script claims.

set -euo pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
base="http://127.0.0.1:${ORES_WEB_PORT:-20402}"
user="${ORES_SHELL_LOGIN_USERNAME:-tenant_admin@acme_corporation}"
password="${ORES_SHELL_LOGIN_PASSWORD:-Secure-Password-123}"
jar=$(mktemp)
trap 'rm -f "$jar"' EXIT

# Signs in and chooses a party, because every read below is scoped to one. The
# session cookie rotates, so every call carries the jar in both directions.
sign_in() {
    curl -sS -c "$jar" -b "$jar" -H 'Content-Type: application/json' \
        -d "{\"username\":\"${user}\",\"password\":\"${password}\"}" \
        "${base}/api/session" > "${here}/session.json"
    local party
    party=$(python3 - "${here}/session.json" <<'PY'
import json, sys
session = json.load(open(sys.argv[1]))
party = session.get('party')
if party is not None:
    print('')
    raise SystemExit
parties = session.get('availableParties') or []
print(parties[0]['id'] if parties else '')
PY
)
    if [ -n "$party" ]; then
        curl -sS -c "$jar" -b "$jar" -H 'Content-Type: application/json' \
            -d "{\"partyId\":\"${party}\"}" \
            "${base}/api/session/party" > "${here}/session.json"
    fi
}

# Writes the answer to one path beside this script and prints it.
capture() {
    local name=$1
    local path=$2
    curl -sS -c "$jar" -b "$jar" "${base}${path}" | tee "${here}/${name}.json"
    printf '\n'
}

sign_in

printf '=== feed binding activity: a source that has written is an instant, one that has not is null\n'
capture bindings_activity /api/bindings/activity

printf '=== the audit window with no bound: both bounds are null on the wire\n'
capture auth_events_all '/api/auth-events?period=all'

printf '=== session statistics with no bound: the function is called with SQL NULL, not empty text\n'
capture session_statistics_all '/api/session-statistics?period=all'

printf '=== login records: an account that never signed in states null\n'
capture login_info '/api/login-info?limit=100'
