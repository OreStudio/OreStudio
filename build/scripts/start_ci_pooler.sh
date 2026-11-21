#!/usr/bin/env bash
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# This program is free software; you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Starts a pgbouncer in transaction mode against the environment's database, so a
# suite runs through a pooler exactly as it would on a host that shares its
# PostgreSQL between environments.
#
# The configuration is generated, never written by hand: the credential file is
# built from the database users the environment already holds, which is what lets
# every service log in as itself instead of through one shared account.
#
# Usage:
#   ./start_ci_pooler.sh start [PORT]
#   ./start_ci_pooler.sh stop
#   ./start_ci_pooler.sh print-config [PORT]
#
# PORT defaults to 6432. The running instance's port is written to
# build/pgbouncer/<label>.port, so a second call to stop needs no arguments.

set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "${here}/../.." && pwd)"
env_file="${root}/.env"

die() {
    echo "start_ci_pooler: $*" >&2
    exit 1
}

[ -f "${env_file}" ] || die "no .env at ${env_file}; run compass env configure first"
command -v pgbouncer >/dev/null 2>&1 || die "pgbouncer is not installed"

label="$(sed -n 's/^ORES_ENV_NAME=//p' "${env_file}" | head -1)"
[ -n "${label}" ] || label="ci"
out="${root}/build/pgbouncer"
mkdir -p "${out}"

# Every user the environment holds a password for, so the pooler authenticates
# each service with its own role. A user list is a credential file: it is written
# under build/, which is not tracked, and read by the pooler alone.
users() {
    local key user pass
    for key in $(sed -n 's/^\([A-Z0-9_]*_DB_USER\)=.*/\1/p' "${env_file}"); do
        user="$(sed -n "s/^${key}=//p" "${env_file}" | head -1)"
        pass="$(sed -n "s/^${key%_USER}_PASSWORD=//p" "${env_file}" | head -1)"
        [ -n "${user}" ] && [ -n "${pass}" ] && printf '"%s" "%s"\n' "${user}" "${pass}"
    done
}

write_config() {
    local port="$1"
    users | sort -u > "${out}/${label}.users"
    chmod 600 "${out}/${label}.users"
    cat > "${out}/${label}.ini" <<EOF
[databases]
* = host=127.0.0.1 port=5432

[pgbouncer]
listen_addr = 127.0.0.1
listen_port = ${port}
auth_type = plain
auth_file = ${out}/${label}.users
pool_mode = transaction
max_prepared_statements = 200
default_pool_size = 20
max_db_connections = 100
max_client_conn = 500
logfile = ${out}/${label}.log
pidfile = ${out}/${label}.pid
EOF
}

start() {
    local port="${1:-6432}"
    command -v pgbouncer >/dev/null 2>&1 || die "pgbouncer is not installed"
    write_config "${port}"
    pgbouncer -d "${out}/${label}.ini" || die "pgbouncer did not start"
    echo "${port}" > "${out}/${label}.port"
    # A pooler that is not listening yet is not up, and a suite that connects at
    # once would fail for a reason that has nothing to do with the suite.
    local waited=0
    while [ "${waited}" -lt 50 ]; do
        if (exec 3<>"/dev/tcp/127.0.0.1/${port}") 2>/dev/null; then
            echo "pgbouncer is listening on 127.0.0.1:${port} (${out}/${label}.ini)"
            return 0
        fi
        sleep 0.1
        waited=$((waited + 1))
    done
    die "pgbouncer did not start listening on ${port}; see ${out}/${label}.log"
}

stop() {
    local pid_file="${out}/${label}.pid"
    if [ -f "${pid_file}" ]; then
        kill "$(cat "${pid_file}")" 2>/dev/null || true
        rm -f "${pid_file}"
    fi
    rm -f "${out}/${label}.port"
}

case "${1:-}" in
    start) start "${2:-6432}" ;;
    stop) stop ;;
    print-config) write_config "${2:-6432}"; cat "${out}/${label}.ini" ;;
    *) die "usage: $0 {start [port]|stop|print-config [port]}" ;;
esac
