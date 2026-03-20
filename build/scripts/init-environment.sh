#!/usr/bin/env bash
# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil; sh-basic-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# init-environment.sh - Initialise the checkout environment.
#
# Generates a .env file at the checkout root with all required credentials
# for running ORE Studio services locally and in CI.
#
# Dependencies: bash 3.2+, openssl
#
# Usage:
#   ./build/scripts/init-environment.sh           # interactive (prompts for postgres pw)
#   PGPASSWORD=secret ./build/scripts/init-environment.sh   # non-interactive
#   ORES_DATABASE_NAME=ores_ci ./build/scripts/init-environment.sh -y  # CI mode
#
# Flags:
#   -y, --yes    Skip the overwrite confirmation prompt
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV_FILE="${CHECKOUT_ROOT}/.env"

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
ASSUME_YES=0
while [[ $# -gt 0 ]]; do
    case "$1" in
        -y|--yes) ASSUME_YES=1 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
    shift
done

# ---------------------------------------------------------------------------
# Derive checkout identity from directory name
# (can be overridden by ORES_DATABASE_NAME / ORES_NATS_SUBJECT_PREFIX env vars)
# ---------------------------------------------------------------------------
DIR_NAME="$(basename "${CHECKOUT_ROOT}")"
if [[ "${DIR_NAME}" == OreStudio.* ]]; then
    LABEL="${DIR_NAME#OreStudio.}"
else
    LABEL="${DIR_NAME}"
fi

DB_NAME="${ORES_DATABASE_NAME:-ores_dev_${LABEL}}"
NATS_PREFIX="${ORES_NATS_SUBJECT_PREFIX:-ores.dev.${LABEL}}"
NATS_URL="nats://localhost:4222"

# ---------------------------------------------------------------------------
# IAM RSA key — stored in build/keys/ (independent of build preset)
# ---------------------------------------------------------------------------
KEYS_DIR="${CHECKOUT_ROOT}/build/keys"
IAM_KEY="${KEYS_DIR}/iam-rsa-private.pem"

if [[ ! -f "${IAM_KEY}" ]]; then
    echo "Generating IAM RSA-2048 signing key..."
    mkdir -p "${KEYS_DIR}"
    openssl genrsa -out "${IAM_KEY}" 2048
    chmod 600 "${IAM_KEY}"
    echo "  Written to ${IAM_KEY}"
fi

# ---------------------------------------------------------------------------
# Overwrite guard
# ---------------------------------------------------------------------------
if [[ -f "${ENV_FILE}" && "${ASSUME_YES}" -eq 0 ]]; then
    printf ".env already exists at %s.\nOverwrite? [y/N] " "${ENV_FILE}"
    read -r answer
    [[ "${answer}" != "y" && "${answer}" != "Y" ]] && { echo "Aborted."; exit 1; }
fi

# ---------------------------------------------------------------------------
# Postgres superuser password
# Read from PGPASSWORD env var if set (CI / scripted use); otherwise prompt.
# ---------------------------------------------------------------------------
if [[ -n "${PGPASSWORD:-}" ]]; then
    PGPASSWORD_VAL="${PGPASSWORD}"
else
    printf "Enter the postgres superuser password: "
    read -r -s PGPASSWORD_VAL
    printf "\n"
    if [[ -z "${PGPASSWORD_VAL}" ]]; then
        echo "Error: postgres password cannot be empty." >&2
        exit 1
    fi
fi

# ---------------------------------------------------------------------------
# Password generator — bash 3.2 compatible, guaranteed non-empty
# ---------------------------------------------------------------------------
gen_password() {
    local result=""
    while [[ ${#result} -lt 32 ]]; do
        result+="$(openssl rand -base64 32 | tr -dc 'a-zA-Z0-9')"
    done
    printf '%s' "${result:0:32}"
}

# ---------------------------------------------------------------------------
# UUID generator — produces a canonical UUID v4-formatted string
# ---------------------------------------------------------------------------
gen_uuid() {
    local h; h="$(openssl rand -hex 16)"
    printf '%s-%s-%s-%s-%s' \
        "${h:0:8}" "${h:8:4}" "${h:12:4}" "${h:16:4}" "${h:20:12}"
}

# ---------------------------------------------------------------------------
# Discover NATS domain services from projects/ores.*.service directories
# ---------------------------------------------------------------------------
SERVICE_COMPONENTS=()
# Frontend services (wt) have their own DB user sections below and must not
# be included here, as their env-var prefix would clash with the NATS service
# credentials generated in this loop.
NATS_ONLY_EXCLUDES=("wt")
for svc_dir in "${CHECKOUT_ROOT}"/projects/ores.*.service; do
    [[ -d "${svc_dir}" ]] || continue
    dir_name="$(basename "${svc_dir}")"        # e.g. ores.iam.service
    component="${dir_name#ores.}"              # e.g. iam.service
    component="${component%.service}"          # e.g. iam
    skip=false
    for ex in "${NATS_ONLY_EXCLUDES[@]}"; do
        [[ "$component" == "$ex" ]] && skip=true && break
    done
    $skip && continue
    SERVICE_COMPONENTS+=("${component}")
done

echo "Detected services: ${SERVICE_COMPONENTS[*]}"

# ---------------------------------------------------------------------------
# Generate passwords
# ---------------------------------------------------------------------------
echo "Generating passwords..."
ORES_DB_DDL_PASSWORD="$(gen_password)"
ORES_DB_CLI_PASSWORD="$(gen_password)"
ORES_DB_WT_PASSWORD="$(gen_password)"
ORES_DB_HTTP_PASSWORD="$(gen_password)"
ORES_DB_COMMS_PASSWORD="$(gen_password)"
ORES_DB_READONLY_PASSWORD="$(gen_password)"
ORES_TEST_DB_DDL_PASSWORD="$(gen_password)"
ORES_TEST_DB_PASSWORD="$(gen_password)"


# Encode PEM as a single line with literal \n separators
JWT_KEY_ONELINE="$(awk '{printf "%s\\n", $0}' "${IAM_KEY}")"

# ---------------------------------------------------------------------------
# Write .env
# ---------------------------------------------------------------------------
echo "Writing ${ENV_FILE}..."

cat > "${ENV_FILE}" << EOF
# ORE Studio Environment: ${LABEL}
# Generated by init-environment.sh on $(date -u '+%Y-%m-%d %H:%M:%S UTC')
# DO NOT COMMIT — this file contains secrets.

# ---------------------------------------------------------------------------
# Checkout identity
# ---------------------------------------------------------------------------
ORES_CHECKOUT_LABEL=${LABEL}
ORES_DATABASE_NAME=${DB_NAME}
ORES_NATS_URL=${NATS_URL}
ORES_NATS_SUBJECT_PREFIX=${NATS_PREFIX}

# ---------------------------------------------------------------------------
# Database admin (postgres superuser — for recreate_database.sh and psql)
# ---------------------------------------------------------------------------
PGPASSWORD=${PGPASSWORD_VAL}
ORES_TEST_DB_DATABASE=${DB_NAME}
ORES_TEST_DB_HOST=localhost

# ---------------------------------------------------------------------------
# Script / DDL passwords (used by recreate_database.sh)
# ---------------------------------------------------------------------------
ORES_DB_DDL_PASSWORD=${ORES_DB_DDL_PASSWORD}
ORES_DB_CLI_PASSWORD=${ORES_DB_CLI_PASSWORD}
ORES_DB_WT_PASSWORD=${ORES_DB_WT_PASSWORD}
ORES_DB_HTTP_PASSWORD=${ORES_DB_HTTP_PASSWORD}
ORES_DB_COMMS_PASSWORD=${ORES_DB_COMMS_PASSWORD}
ORES_DB_READONLY_PASSWORD=${ORES_DB_READONLY_PASSWORD}

# ---------------------------------------------------------------------------
# Test connection credentials (read by recreate_database.sh and C++ tests)
# ---------------------------------------------------------------------------
ORES_TEST_DB_USER=ores_test_dml_user
ORES_TEST_DB_PASSWORD=${ORES_TEST_DB_PASSWORD}
ORES_TEST_DB_DDL_PASSWORD=${ORES_TEST_DB_DDL_PASSWORD}

EOF

# ---------------------------------------------------------------------------
# NATS service DB credentials (read by C++ make_mapper) — auto-detected
# ---------------------------------------------------------------------------
{
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# NATS service DB credentials (read by C++ make_mapper)"
    echo "# ---------------------------------------------------------------------------"
    for component in "${SERVICE_COMPONENTS[@]}"; do
        upper="$(echo "${component}" | tr '[:lower:]-' '[:upper:]_')"
        db_user="ores_${component}_service"
        echo ""
        echo "ORES_${upper}_SERVICE_DB_USER=${db_user}"
        echo "ORES_${upper}_SERVICE_DB_PASSWORD=$(gen_password)"
        echo "ORES_${upper}_SERVICE_DB_DATABASE=${DB_NAME}"
    done
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# CLI DB credentials (read by C++ make_mapper(\"CLI\"))"
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_CLI_DB_USER=ores_cli_user"
    echo "ORES_CLI_DB_PASSWORD=${ORES_DB_CLI_PASSWORD}"
    echo "ORES_CLI_DB_DATABASE=${DB_NAME}"
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# Comms shell DB credentials (read by C++ make_mapper(\"COMMS_SHELL\"))"
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_COMMS_SHELL_DB_USER=ores_comms_user"
    echo "ORES_COMMS_SHELL_DB_PASSWORD=${ORES_DB_COMMS_PASSWORD}"
    echo "ORES_COMMS_SHELL_DB_DATABASE=${DB_NAME}"
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# HTTP server DB credentials (read by C++ make_mapper(\"HTTP_SERVER\"))"
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_HTTP_SERVER_DB_USER=ores_http_user"
    echo "ORES_HTTP_SERVER_DB_PASSWORD=${ORES_DB_HTTP_PASSWORD}"
    echo "ORES_HTTP_SERVER_DB_DATABASE=${DB_NAME}"
    echo "ORES_HTTP_SERVER_JWT_SECRET=$(gen_password)"
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# WT service DB credentials (read by C++ make_mapper(\"WT\"))"
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_WT_DB_USER=ores_wt_user"
    echo "ORES_WT_DB_PASSWORD=${ORES_DB_WT_PASSWORD}"
    echo "ORES_WT_DB_DATABASE=${DB_NAME}"
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# IAM JWT signing key (PEM encoded as single line, \\n = newline)"
    echo "# Note: excluded from GITHUB_ENV export since services don't run in CI."
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_IAM_SERVICE_JWT_PRIVATE_KEY=\"${JWT_KEY_ONELINE}\""
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# Compute wrapper node host IDs (one per test node)"
    echo "# Each ID must match a host record in the compute.hosts table."
    echo "# Re-run recreate_database.sh after regenerating to keep IDs in sync."
    echo "# ---------------------------------------------------------------------------"
    for n in 1 2 3 4 5; do
        echo "ORES_GRID_NODE_${n}_HOST_ID=$(gen_uuid)"
    done
} >> "${ENV_FILE}"

chmod 600 "${ENV_FILE}"

echo ""
echo "=== Environment initialised for '${LABEL}' (db: ${DB_NAME}) ==="
echo ""
echo "Next steps:"
echo "  1. Create the database (first time only):"
echo "       ./projects/ores.sql/recreate_database.sh -y"
echo ""
echo "  2. In Emacs, set up SQL connections:"
echo "       M-x ores-db/setup-connections"
echo ""
echo "  3. Start services via prodigy — they will read .env automatically."
echo ""
