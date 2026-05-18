#!/bin/bash
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
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program; if not, write to the Free Software Foundation, Inc., 51
# Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
#

set -e
set -o pipefail

# Source .env if present (local development). In CI, env vars are exported directly.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV_FILE="${CHECKOUT_ROOT}/.env"
if [[ -f "${ENV_FILE}" ]]; then
    set -o allexport
    # shellcheck source=/dev/null
    source "${ENV_FILE}"
    set +o allexport
fi

# Validate .env version before touching anything.
# shellcheck source=utility/validate_env_version.sh
source "${SCRIPT_DIR}/utility/validate_env_version.sh"

usage() {
    cat <<EOF
Usage: $(basename "$0") [OPTIONS]

Returns the entire system to pre-bootstrap state so the SystemProvisionerWizard
fires on the next application startup.

WARNING: This is a DESTRUCTIVE operation. All non-system tenants are purged
(hard-deleted) and all system admin accounts are removed. This cannot be undone.

What will be reset:
  - ALL non-system tenants (purged — hard-deleted, cannot be recovered)
  - System admin accounts (soft-deleted)
  - Active sessions for the system tenant
  - System bootstrap mode (re-enabled)

What will be preserved:
  - System tenant record
  - Seeded roles, permissions, reference data
  - Database schema and all stored functions

After this script completes, start the application. The SystemProvisionerWizard
will fire, prompting creation of a new system administrator account.

Optional arguments:
    -y, --yes                       Assume yes to all prompts (skip confirmation)
    -H, --help                      Show this help message

Example:
    # Reset system with confirmation prompt
    $(basename "$0")

    # Reset without prompting (useful in scripts)
    $(basename "$0") -y

EOF
    exit 1
}

ASSUME_YES=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -y|--yes)        ASSUME_YES="1";     shift   ;;
        -H|--help)       usage ;;
        --)              shift; break ;;
        *)
            echo "Error: Invalid option $1" >&2
            usage
            ;;
    esac
done

# Validate required env vars
MISSING_VARS=()
[[ -z "${PGPASSWORD:-}" ]] && MISSING_VARS+=("PGPASSWORD")
[[ -z "${ORES_TEST_DB_DATABASE:-}" ]] && MISSING_VARS+=("ORES_TEST_DB_DATABASE")

if [[ ${#MISSING_VARS[@]} -gt 0 ]]; then
    echo "Error: Missing required environment variables:" >&2
    for v in "${MISSING_VARS[@]}"; do
        echo "  - ${v}" >&2
    done
    echo "" >&2
    echo "Run ./build/scripts/init-environment.sh to generate a .env file." >&2
    exit 1
fi

_current_phase="(startup)"
trap 'echo "" >&2; echo "=== FATAL: reset_system.sh failed in phase: ${_current_phase} ===" >&2' ERR

# ============================================================================
# Banner
# ============================================================================
echo ""
echo "=== ORE Studio System Bootstrap Reset ==="
echo ""
echo "  Database: ${ORES_TEST_DB_DATABASE}"
echo ""
echo "WARNING: This will purge ALL non-system tenants. This cannot be undone."
echo ""
echo "What will be reset:"
echo "  - ALL non-system tenants (hard-deleted)"
echo "  - System admin accounts (soft-deleted)"
echo "  - Active sessions for the system tenant"
echo "  - System bootstrap mode → true"
echo ""
echo "What will be preserved:"
echo "  - System tenant record"
echo "  - Seeded roles, permissions, reference data"
echo "  - Database schema and all stored functions"
echo ""
echo "After this script completes, start the application."
echo "The SystemProvisionerWizard will fire, requiring a new admin account."
echo ""

# ============================================================================
# Confirmation prompt (unless -y flag is set)
# ============================================================================
if [[ -z "${ASSUME_YES}" ]]; then
    echo "This will PERMANENTLY DELETE all non-system tenants."
    echo ""
    read -r -p "Type 'yes' to reset the system: " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
    echo ""
fi

# ============================================================================
# Execute reset via SQL function
# ============================================================================
_current_phase="system bootstrap reset"

TMP_SQL=$(mktemp "${TMPDIR:-/tmp}/reset_system_XXXXXX.sql")
trap 'rm -f "${TMP_SQL}"; echo "" >&2; echo "=== FATAL: reset_system.sh failed in phase: ${_current_phase} ===" >&2' ERR
trap 'rm -f "${TMP_SQL}"' EXIT

cat > "${TMP_SQL}" <<'ENDSQL'
\set ON_ERROR_STOP on
SELECT * FROM ores_iam_set_tenant_fn('system');
SELECT ores_iam_reset_system_fn();
ENDSQL

echo "--- Running system bootstrap reset ---"
"${SCRIPT_DIR}/run_sql.sh" -f "${TMP_SQL}"

echo ""
echo "=== System bootstrap reset complete ==="
echo ""
echo "  The system has been returned to pre-bootstrap state."
echo "  All non-system tenants have been purged."
echo "  System admin accounts have been removed."
echo ""
echo "  Start the application to trigger SystemProvisionerWizard."
echo ""
