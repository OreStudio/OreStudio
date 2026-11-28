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

# A systemd user unit gets a PATH without /usr/sbin, so a bare "command -v
# pgbouncer" says it is missing on a host that has it. Find it once, here.
pgbouncer_bin="$(command -v pgbouncer 2>/dev/null || true)"
if [ -z "${pgbouncer_bin}" ]; then
    for candidate in /usr/sbin/pgbouncer /usr/bin/pgbouncer /usr/local/sbin/pgbouncer; do
        if [ -x "${candidate}" ]; then
            pgbouncer_bin="${candidate}"
            break
        fi
    done
fi
# The binary is needed to run a pooler, not to write its configuration, so a
# deploy machine that only generates a container configuration needs none.

label="$(sed -n 's/^ORES_ENV_NAME=//p' "${env_file}" | head -1)"
[ -n "${label}" ] || label="ci"
# The port PostgreSQL listens on. A remote environment keeps its database off
# the default, and the deploy passes that port in PGPORT when it generates this
# configuration for the remote host.
db_port="${PGPORT:-$(sed -n 's/^PGPORT=//p' "${env_file}" | head -1)}"
db_port="${db_port:-5432}"
out="${root}/build/pgbouncer"
mkdir -p "${out}"

# Every user the pooler authenticates, with the secret it authenticates them
# with. The pooler runs scram-sha-256, so it accepts a SCRAM verifier in place of
# a password, and a service's password never reaches this file.
#
# The verifier comes from the server whenever it can be reached, because that is
# the only copy that cannot be out of date: a recreate and a provisioning run
# both rewrite role passwords, and a secret written before either of them is a
# pooler that refuses a password the server accepts. A host that builds a
# container's configuration away from the database has no server to ask, and
# falls back to the verifiers provisioning wrote under build/keys and to the
# passwords the environment file carries for the deployment's own accounts.
#
# The pooler reads this file rather than asking the server for a verifier: an
# account that answers that question may read every verifier in the instance,
# and a rotation restarts the fleet anyway, because each service resolves its
# password at start.
users() {
    local key user pass verifier_dir secret_of
    verifier_dir="${root}/build/keys"
    local -A from_server=()
    while read -r user secret_of; do
        [ -n "${user}" ] && from_server["${user}"]="${secret_of}"
    done <<< "$(server_verifiers)"
    for key in $(sed -n 's/^\([A-Z0-9_]*_DB_USER\)=.*/\1/p' "${env_file}"); do
        user="$(sed -n "s/^${key}=//p" "${env_file}" | head -1)"
        [ -n "${user}" ] || continue
        pass="${from_server[${user}]:-}"
        if [ -z "${pass}" ]; then
            pass="$(sed -n "s/^${key%_USER}_PASSWORD=//p" "${env_file}" | head -1)"
        fi
        if [ -z "${pass}" ]; then
            # ORES_DQ_SERVICE_DB_USER names the verifier of dq_service.
            local stem
            stem="$(printf '%s' "${key}" | sed -e 's/^ORES_//' -e 's/_DB_USER$//' | tr 'A-Z' 'a-z')"
            [ -f "${verifier_dir}/${stem}.db-verifier" ] && \
                pass="$(cat "${verifier_dir}/${stem}.db-verifier")"
        fi
        [ -n "${pass}" ] && printf '"%s" "%s"\n' "${user}" "${pass}"
    done
}

# The verifier of every user the environment names, read from the server. An
# empty answer means the server could not be asked, and the caller falls back.
server_verifiers() {
    command -v psql > /dev/null 2>&1 || return 0
    local listed="" user
    for user in $(sed -n 's/^[A-Z0-9_]*_DB_USER=//p' "${env_file}"); do
        listed="${listed:+${listed},}'${user//\'/\'\'}'"
    done
    [ -n "${listed}" ] || return 0
    PGPASSWORD="${PGPASSWORD:-$(sed -n 's/^PGPASSWORD=//p' "${env_file}" | head -1)}" \
        psql -h 127.0.0.1 -p "${db_port}" -U postgres -t -A -F' ' \
        -c "select usename, passwd from pg_shadow
            where usename in (${listed}) and passwd is not null" 2> /dev/null || true
}

# Writes the configuration. A host writes it under build/pgbouncer, log and all.
# A container is given its own directory: there the log goes to stderr, which is
# what podman captures, because a file inside the image is a file nobody reads.
write_config() {
    local port="${1}"
    local dir="${2:-${out}}"
    local db_host_override="${3:-}"
    local base="${label}"
    mkdir -p "${dir}"
    # A container is told where its configuration lives by the image, so it gets
    # the name the entrypoint names. Its credential file is read by pgbouncer
    # after it drops to its own user, so it must be readable by that user; the
    # directory holding it is the deploy's, and that is what keeps it private.
    local auth_path="${dir}/${label}.users"
    if [ "${dir}" != "${out}" ]; then
        base="pgbouncer"
        # The image mounts the configuration here, so the file it reads names
        # this path and not the one it was written to.
        auth_path="/etc/pgbouncer/${base}.users"
    fi
    users | sort -u > "${dir}/${base}.users"
    # The credential file is read by the pooler and by root and by nobody else.
    # The directory is traversable so a container's root can reach the file and
    # not listable, so a local user of the host cannot even see the names in it.
    chmod 600 "${dir}/${base}.users"
    chmod 711 "${dir}"
    local log_line="logfile = ${out}/${label}.log"
    local pid_line="pidfile = ${dir}/${label}.pid"
    local listen_addr="127.0.0.1"
    local db_host="127.0.0.1"
    if [ "${dir}" != "${out}" ]; then
        # In a container, loopback is the container itself, so the pooler must
        # listen on every interface for the fleet's containers to reach it, and
        # the database is on the host that runs them.
        log_line=""
        # A container cannot write beside a mounted configuration.
        pid_line=""
        listen_addr="0.0.0.0"
        db_host="${db_host_override:-host.containers.internal}"
    fi
    cat > "${dir}/${base}.ini" <<EOF
[databases]
* = host=${db_host} port=${db_port}

[pgbouncer]
listen_addr = ${listen_addr}
listen_port = ${port}
auth_type = scram-sha-256
auth_file = ${auth_path}
pool_mode = transaction
max_prepared_statements = 200
default_pool_size = 20
max_db_connections = 100
max_client_conn = 500
${log_line}
${pid_line}
EOF
}

# The account a root container hands the pooler to. Every image this runs in
# carries one of these, and the run keeps the same account throughout, so the
# runtime directory can be given to it before the pooler reads anything.
pooler_account() {
    local candidate
    for candidate in postgres nobody; do
        if id -u "${candidate}" >/dev/null 2>&1; then
            echo "${candidate}"
            return 0
        fi
    done
    return 1
}

start() {
    local port="${1:-6432}"
    [ -n "${pgbouncer_bin}" ] || die "pgbouncer is not installed"
    write_config "${port}"
    # PgBouncer refuses to run as root, so a container that starts it as root
    # hands it to an unprivileged account. The runtime directory goes with it:
    # the pooler writes its log and pid file there and reads its credential
    # file from there, so a directory it cannot write is a pooler that exits
    # without a word.
    local run_as="" launcher=""
    if [ "$(id -u)" -eq 0 ]; then
        run_as="$(pooler_account)" ||
            die "PgBouncer refuses to run as root, and this image has no account to hand it to"
        chown -R "${run_as}" "${out}" ||
            die "could not give ${out} to ${run_as}"
        if command -v runuser >/dev/null 2>&1; then
            launcher="runuser"
        elif command -v setpriv >/dev/null 2>&1; then
            launcher="setpriv"
        else
            die "PgBouncer refuses to run as root, and neither runuser nor setpriv is installed"
        fi
    fi
    if [ -z "${run_as}" ]; then
        "${pgbouncer_bin}" -d "${out}/${label}.ini" || die "pgbouncer did not start"
    elif [ "${launcher}" = "runuser" ]; then
        runuser -u "${run_as}" -- "${pgbouncer_bin}" -d "${out}/${label}.ini" ||
            die "pgbouncer did not start"
    else
        setpriv --reuid="${run_as}" --regid="${run_as}" --init-groups -- \
            "${pgbouncer_bin}" -d "${out}/${label}.ini" || die "pgbouncer did not start"
    fi
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
    print-config)
        write_config "${2:-6432}" "${3:-}" "${4:-}"
        if [ -n "${3:-}" ]; then
            cat "${3}/pgbouncer.ini"
        else
            cat "${out}/${label}.ini"
        fi
        ;;
    *) die "usage: $0 {start [port]|stop|print-config [port] [dir] [db-host]}" ;;
esac
