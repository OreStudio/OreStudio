#!/bin/bash
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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
# Street, Fifth Floor, Boston, MA 02110-1301, USA.
#
# Installs the PostgreSQL extension required by OreStudio:
#
#   - timescaledb (apt package, time-series storage)
#
# Must be run after PostgreSQL is installed.  Use --configure to update
# postgresql.conf with shared_preload_libraries, and --restart to restart
# the PostgreSQL service so the changes take effect.
#
# In CI the postgres action handles shared_preload_libraries via its
# postgres-conf input, so --configure / --restart are not needed.
#
# Usage:
#   ./setup_postgres_extensions.sh [OPTIONS]
#
# Options:
#   --pg-version N    PostgreSQL major version (auto-detected via pg_lsclusters
#                     if omitted)
#   --configure       Write shared_preload_libraries to postgresql.conf
#   --restart         Restart the PostgreSQL service after configuration
#
# Example (local full install, postgres already running):
#   ./setup_postgres_extensions.sh --configure --restart
#
# Example (CI, postgres not yet started):
#   ./setup_postgres_extensions.sh --pg-version 18

set -e

configure=0
restart_pg=0
pg_major=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --pg-version) pg_major="$2"; shift 2 ;;
        --configure)  configure=1;   shift   ;;
        --restart)    restart_pg=1;  shift   ;;
        *)
            echo "Unknown argument: $1" >&2
            echo "Usage: $0 [--pg-version N] [--configure] [--restart]" >&2
            exit 1
            ;;
    esac
done

# ---------------------------------------------------------------------------
# Detect PostgreSQL version
# ---------------------------------------------------------------------------
if [[ -z "$pg_major" ]]; then
    pg_major=$(pg_lsclusters 2>/dev/null | awk 'NR==2 {print $1}')
fi

if [[ -z "$pg_major" ]]; then
    echo "Error: Could not detect PostgreSQL version." >&2
    echo "Pass --pg-version N to specify it explicitly." >&2
    exit 1
fi

echo "Setting up PostgreSQL ${pg_major} extensions..."
echo ""

# ---------------------------------------------------------------------------
# Prerequisites: base tools needed to add repos
# ---------------------------------------------------------------------------
sudo apt-get install -y -q gnupg curl lsb-release

# ---------------------------------------------------------------------------
# PGDG apt repository (needed on CI runners that only have the
# distro-default PostgreSQL version pre-installed)
# ---------------------------------------------------------------------------
if [[ ! -f /etc/apt/sources.list.d/pgdg.list ]]; then
    echo "Adding PostgreSQL PGDG apt repository for PostgreSQL ${pg_major}..."
    curl -fsSL https://www.postgresql.org/media/keys/ACCC4CF8.asc \
        | sudo gpg --dearmor -o /usr/share/keyrings/postgresql-keyring.gpg
    echo "deb [signed-by=/usr/share/keyrings/postgresql-keyring.gpg] \
https://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" \
        | sudo tee /etc/apt/sources.list.d/pgdg.list > /dev/null
    sudo apt-get update -q
fi

# ---------------------------------------------------------------------------
# timescaledb: time-series storage (Debian sid native package)
# postgresql-N-timescaledb is available directly from the Debian sid repo;
# the timescaledb packagecloud repo is not needed and does not support sid.
# ---------------------------------------------------------------------------
echo "Installing timescaledb for PostgreSQL ${pg_major}..."

sudo apt-get install -y "postgresql-${pg_major}-timescaledb" || \
    echo "Warning: timescaledb install failed — sessions will use regular tables."

# ---------------------------------------------------------------------------
# postgresql.conf: shared_preload_libraries
# ---------------------------------------------------------------------------
if [[ $configure -eq 1 ]]; then
    pg_conf=$(find /etc/postgresql/"${pg_major}" -name postgresql.conf 2>/dev/null | head -1)
    if [[ -z "$pg_conf" ]]; then
        echo "Warning: Could not find postgresql.conf — skipping shared_preload_libraries config."
    else
        echo "Configuring shared_preload_libraries in ${pg_conf}..."
        required_libs=("timescaledb")
        if grep -q "^shared_preload_libraries" "$pg_conf"; then
            # Keep the existing libraries except pg_cron, which nothing uses
            # and which stops PostgreSQL starting once its package is gone,
            # then append the required ones that are missing.
            current=$(grep -m1 "^shared_preload_libraries" "$pg_conf" \
                | sed -e 's/#.*//' \
                      -e 's/^shared_preload_libraries[[:space:]]*=[[:space:]]*//' \
                      -e "s/['\"]//g" \
                | tr ',' '\n' | tr -d ' \t')
            kept=()
            while IFS= read -r lib; do
                [[ -z "$lib" || "$lib" == "pg_cron" ]] && continue
                kept+=("$lib")
            done <<< "$current"
            for lib in "${required_libs[@]}"; do
                if [[ ! " ${kept[*]} " == *" ${lib} "* ]]; then
                    kept+=("$lib")
                fi
            done
            new_val=$(IFS=,; echo "${kept[*]}")
            sudo sed -i \
                "s|^shared_preload_libraries.*|shared_preload_libraries = '${new_val}'|" \
                "$pg_conf"
        else
            echo "shared_preload_libraries = 'timescaledb'" | sudo tee -a "$pg_conf" > /dev/null
        fi
        echo "shared_preload_libraries set."
    fi
fi

# ---------------------------------------------------------------------------
# Restart PostgreSQL
# ---------------------------------------------------------------------------
if [[ $restart_pg -eq 1 ]]; then
    echo "Restarting PostgreSQL ${pg_major}..."
    sudo systemctl restart "postgresql@${pg_major}-main" 2>/dev/null || \
        sudo systemctl restart postgresql
    echo "PostgreSQL restarted."
fi

echo ""
echo "======================================================================="
echo "  PostgreSQL ${pg_major} extension installed."
echo "======================================================================="
echo ""
echo "  Installed: timescaledb"
echo ""
if [[ $configure -eq 0 ]]; then
    echo "  NOTE: shared_preload_libraries not configured automatically."
    echo "  For local dev (postgres already installed), run with --configure --restart"
    echo "  In CI, pass shared_preload_libraries via the postgres action's postgres-conf."
    echo ""
fi
echo "  Next step: compass db recreate to create the ORES database."
echo "======================================================================="
