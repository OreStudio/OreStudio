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
Usage: $(basename "$0") [OPTIONS] -t TENANT_CODE

Resets a tenant to the state right after initial tenant onboarding. On next
login, both provisioning wizards will fire automatically:
  - TenantProvisioningWizard  (sets up users, roles, permissions)
  - PartyProvisioningWizard   (sets up counterparties, org units, books, reports)

What is reset:
  - Counterparties, counterparty identifiers, contacts, party-counterparty links
  - Business units, portfolios, books
  - Report definitions, scheduler job definitions
  - Operational party status (flipped back to Inactive)
  - Tenant bootstrap mode (re-enabled so TenantProvisioningWizard fires)

What is preserved:
  - Tenant record, admin account, roles, permissions
  - Parties and account-party associations (login remains functional)
  - Base reference data (currencies, countries, etc.)

Required arguments:
    -t, --tenant TENANT_CODE        Tenant code to reset (e.g. ores.dev.local3)

Optional arguments:
    -y, --yes                       Assume yes to all prompts (skip confirmation)
    -H, --help                      Show this help message

Example:
    # Reset tenant ores.dev.local3 with confirmation prompt
    $(basename "$0") -t ores.dev.local3

    # Reset without prompting (useful in scripts)
    $(basename "$0") -t ores.dev.local3 -y

EOF
    exit 1
}

TENANT_CODE=""
ASSUME_YES=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        -t|--tenant)     TENANT_CODE="$2";   shift 2 ;;
        -y|--yes)        ASSUME_YES="1";     shift   ;;
        -H|--help)       usage ;;
        --)              shift; break ;;
        *)
            echo "Error: Invalid option $1" >&2
            usage
            ;;
    esac
done

if [[ -z "${TENANT_CODE}" ]]; then
    echo "Error: --tenant / -t is required." >&2
    echo "" >&2
    usage
fi

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
    echo "Run ./projects/ores.compass/compass.sh env init to generate a .env file." >&2
    exit 1
fi

_current_phase="(startup)"
trap 'echo "" >&2; echo "=== FATAL: reset_tenant.sh failed in phase: ${_current_phase} ===" >&2' ERR

# ============================================================================
# Banner
# ============================================================================
echo ""
echo "=== ORE Studio Tenant Bootstrap Reset ==="
echo ""
echo "  Tenant:   ${TENANT_CODE}"
echo "  Database: ${ORES_TEST_DB_DATABASE}"
echo ""
echo "What will be reset:"
echo "  - Counterparties, identifiers, contacts, party-counterparty links"
echo "  - Business units, portfolios, books"
echo "  - Report definitions, scheduler job definitions"
echo "  - Operational party status → Inactive"
echo "  - Tenant bootstrap mode    → true"
echo ""
echo "What will be preserved:"
echo "  - Tenant record, admin account, roles, permissions"
echo "  - Parties and account-party associations (login remains functional)"
echo "  - Base reference data (currencies, countries, etc.)"
echo ""
echo "On next login: TenantProvisioningWizard and PartyProvisioningWizard will fire."
echo ""

# ============================================================================
# Confirmation prompt (unless -y flag is set)
# ============================================================================
if [[ -z "${ASSUME_YES}" ]]; then
    read -r -p "Type 'yes' to reset tenant '${TENANT_CODE}': " confirm
    if [[ "${confirm}" != "yes" ]]; then
        echo "Aborted."
        exit 1
    fi
    echo ""
fi

# ============================================================================
# Execute reset via SQL function
# ============================================================================
_current_phase="tenant bootstrap reset"

TMP_SQL=$(mktemp "${TMPDIR:-/tmp}/reset_tenant_XXXXXX.sql")
trap 'rm -f "${TMP_SQL}"; echo "" >&2; echo "=== FATAL: reset_tenant.sh failed in phase: ${_current_phase} ===" >&2' ERR
trap 'rm -f "${TMP_SQL}"' EXIT

cat > "${TMP_SQL}" <<'ENDSQL'
\set ON_ERROR_STOP on
SELECT * FROM ores_iam_set_tenant_fn('system');
SELECT ores_iam_reset_tenant_bootstrap_fn(:'tenant_code');
ENDSQL

echo "--- Running tenant bootstrap reset ---"
"${SCRIPT_DIR}/run_sql.sh" -f "${TMP_SQL}" -v "tenant_code=${TENANT_CODE}"

echo ""
echo "=== Tenant bootstrap reset complete ==="
echo ""
echo "  Tenant '${TENANT_CODE}' has been reset."
echo "  Start the application and log in to trigger the provisioning wizards."
echo ""
