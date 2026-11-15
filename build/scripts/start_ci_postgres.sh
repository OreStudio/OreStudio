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
# Starts the PostgreSQL cluster of the CI image and gives the postgres role a
# password, so the build jobs connect over TCP as they did with the
# action-setup-postgres action. The image loads timescaledb at start.
#
# Usage:
#   PGPASSWORD=... ./start_ci_postgres.sh MAJOR DATABASE
#
# Runs as root in the container; pg_ctlcluster and su run the server and psql
# as the postgres user.
set -euo pipefail

major="$1"
database="$2"
: "${PGPASSWORD:?set PGPASSWORD to the password of the postgres role}"

# Root in the container is not the postgres user, so peer authentication on
# the socket fails. The CI database is throwaway, so trust local connections,
# as the action-setup-postgres configuration of the nightly did.
hba="/etc/postgresql/${major}/main/pg_hba.conf"
sed -i '1i local all all trust' "${hba}"

pg_ctlcluster "${major}" main start
su postgres -c "psql -v ON_ERROR_STOP=1 -q" <<SQL
ALTER ROLE postgres PASSWORD '${PGPASSWORD}';
CREATE DATABASE ${database};
SQL

psql -h localhost -U postgres -d "${database}" -tAc "SHOW shared_preload_libraries"
