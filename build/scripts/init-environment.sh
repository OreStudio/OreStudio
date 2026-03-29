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
# Existing passwords are preserved when the file already exists — only missing
# variables are generated. Use --enable-logging / --disable-logging to toggle
# test logging without touching any other variable.
#
# Dependencies: bash 3.2+, openssl
#
# Usage:
#   ./build/scripts/init-environment.sh --preset linux-clang-debug-make
#   PGPASSWORD=secret ./build/scripts/init-environment.sh --preset linux-clang-debug-make
#   ORES_DATABASE_NAME=ores_ci ./build/scripts/init-environment.sh --preset linux-clang-release-ninja -y
#
# Flags:
#   -y, --yes                     Skip the overwrite confirmation prompt
#   --preset PRESET               Build preset name (required, e.g. linux-clang-debug-ninja)
#   --enable-logging [level]      Enable test logging (level: trace/debug/info/warn/error, default: debug)
#   --disable-logging             Disable test logging
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV_FILE="${CHECKOUT_ROOT}/.env"

# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------
ASSUME_YES=0
PRESET=""
LOGGING_OP=""   # "enable" | "disable" | ""
LOGGING_LEVEL="debug"

while [[ $# -gt 0 ]]; do
    case "$1" in
        -y|--yes) ASSUME_YES=1 ;;
        --preset)
            if [[ -z "${2:-}" || "${2}" == -* ]]; then
                echo "Error: --preset requires a value." >&2
                exit 1
            fi
            PRESET="$2"; shift ;;
        --enable-logging)
            LOGGING_OP="enable"
            if [[ -n "${2:-}" && "${2}" != -* ]]; then
                LOGGING_LEVEL="$2"
                shift
            fi
            ;;
        --disable-logging) LOGGING_OP="disable" ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
    shift
done

# ---------------------------------------------------------------------------
# Logging-only mode — update only the logging vars, leave everything else alone
# ---------------------------------------------------------------------------
if [[ -n "${LOGGING_OP}" ]]; then
    if [[ ! -f "${ENV_FILE}" ]]; then
        echo "Error: ${ENV_FILE} does not exist. Run init-environment.sh first." >&2
        exit 1
    fi

    # Remove any existing logging vars
    tmp="$(mktemp)"
    grep -v '^ORES_TEST_LOG_' "${ENV_FILE}" | \
        grep -v '^# Test logging' \
        > "${tmp}" || true
    # Strip trailing blank lines from tmp
    sed -i 's/[[:space:]]*$//' "${tmp}"

    if [[ "${LOGGING_OP}" == "enable" ]]; then
        printf '\n# Test logging\nORES_TEST_LOG_ENABLED=true\nORES_TEST_LOG_LEVEL=%s\nORES_TEST_LOG_CONSOLE=true\n' \
            "${LOGGING_LEVEL}" >> "${tmp}"
        echo "Test logging enabled (level=${LOGGING_LEVEL})."
    else
        echo "Test logging disabled."
    fi

    mv "${tmp}" "${ENV_FILE}"
    chmod 600 "${ENV_FILE}"
    echo "Re-run 'cmake --preset <preset>' to pick up the change."
    exit 0
fi

# --preset is required for local runs; in CI the preset comes from the workflow.
if [[ -z "${PRESET}" && -z "${CI:-}" ]]; then
    echo "Error: --preset is required." >&2
    echo "  Example: ./build/scripts/init-environment.sh --preset linux-clang-debug-ninja" >&2
    exit 1
fi

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

# Derive per-environment NATS ports from the numeric suffix of the label.
# local1→42221/8221, local2→42222/8222, …  non-numeric labels fall back to 42229/8229.
LABEL_SUFFIX="$(echo "${LABEL}" | sed 's/[^0-9]*\([0-9]*\)$/\1/')"
if [[ -n "${LABEL_SUFFIX}" ]]; then
    NATS_PORT=$((42220 + LABEL_SUFFIX))
    NATS_MONITOR_PORT=$((8220 + LABEL_SUFFIX))
else
    NATS_PORT=42229
    NATS_MONITOR_PORT=8229
fi
NATS_URL="nats://localhost:${NATS_PORT}"
NATS_MONITOR_URL="http://localhost:${NATS_MONITOR_PORT}"
NATS_STORE_DIR="${CHECKOUT_ROOT}/build/nats/${LABEL}/jetstream"

NATS_CERTS_DIR="${CHECKOUT_ROOT}/build/keys/nats"

# ---------------------------------------------------------------------------
# NATS mTLS certificates — generate if not present (idempotent: existing
# files are preserved; pass --force to regenerate).
# ---------------------------------------------------------------------------
echo "--- NATS certificates ---"
"${SCRIPT_DIR}/generate_nats_certs.sh"

NATS_TLS_CA=""
NATS_TLS_CERT=""
NATS_TLS_KEY=""
if [[ -f "${NATS_CERTS_DIR}/ca.crt" ]]; then
    NATS_TLS_CA="${NATS_CERTS_DIR}/ca.crt"
    NATS_TLS_CERT="${NATS_CERTS_DIR}/ores.qt.client.crt"
    NATS_TLS_KEY="${NATS_CERTS_DIR}/ores.qt.client.key"
fi

# Sanitise LABEL for Postgres identifier use (lowercase, hyphens/dots → underscores)
LABEL_LOWER="$(echo "${LABEL}" | tr '[:upper:]' '[:lower:]' | tr '.-' '__')"

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
# Read existing .env values so passwords are preserved on re-runs.
# Uses grep rather than an associative array so the script is compatible
# with bash 3.2 (macOS system shell).
# ---------------------------------------------------------------------------
# Read a single key from the existing .env file.
# bash 3.2 compatible — no associative arrays (macOS ships bash 3.x).
get_existing_val() {
    local key="$1"
    [[ -f "${ENV_FILE}" ]] || return 0
    grep "^${key}=" "${ENV_FILE}" | head -1 | cut -d'=' -f2- || true
}

# ---------------------------------------------------------------------------
# Overwrite guard (skipped if no existing file)
# ---------------------------------------------------------------------------
if [[ -f "${ENV_FILE}" && "${ASSUME_YES}" -eq 0 ]]; then
    printf ".env already exists at %s.\nExisting passwords will be reused.\nContinue? [y/N] " "${ENV_FILE}"
    read -r answer
    [[ "${answer}" != "y" && "${answer}" != "Y" ]] && { echo "Aborted."; exit 1; }
fi

# ---------------------------------------------------------------------------
# Postgres superuser password
# Read from PGPASSWORD env var if set (CI / scripted use); otherwise prompt.
# Reuse existing value if already present in .env.
# ---------------------------------------------------------------------------
if [[ -n "${PGPASSWORD:-}" ]]; then
    PGPASSWORD_VAL="${PGPASSWORD}"
elif [[ -n "$(get_existing_val PGPASSWORD)" ]]; then
    PGPASSWORD_VAL="$(get_existing_val PGPASSWORD)"
    echo "Reusing existing PGPASSWORD."
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
# Helper: return existing value for KEY, or generate a new password
# ---------------------------------------------------------------------------
get_or_gen() {
    local key="$1"
    local existing; existing="$(get_existing_val "$key")"
    if [[ -n "${existing}" ]]; then
        printf '%s' "${existing}"
    else
        gen_password
    fi
}

# ---------------------------------------------------------------------------
# Helper: return existing UUID for KEY, or generate a new one
# ---------------------------------------------------------------------------
get_or_gen_uuid() {
    local key="$1"
    local existing; existing="$(get_existing_val "$key")"
    if [[ -n "${existing}" ]]; then
        printf '%s' "${existing}"
    else
        gen_uuid
    fi
}

# ---------------------------------------------------------------------------
# Discover NATS domain services from projects/ores.*.service directories
# ---------------------------------------------------------------------------
SERVICE_COMPONENTS=()
NATS_ONLY_EXCLUDES=("wt")
for svc_dir in "${CHECKOUT_ROOT}"/projects/ores.*.service; do
    [[ -d "${svc_dir}" ]] || continue
    dir_name="$(basename "${svc_dir}")"
    component="${dir_name#ores.}"
    component="${component%.service}"
    skip=false
    for ex in "${NATS_ONLY_EXCLUDES[@]}"; do
        [[ "$component" == "$ex" ]] && skip=true && break
    done
    $skip && continue
    SERVICE_COMPONENTS+=("${component}")
done

echo "Detected services: ${SERVICE_COMPONENTS[*]}"

# ---------------------------------------------------------------------------
# Role and user names — env-prefixed for isolation across environments.
# These are always derived from LABEL and never read from existing .env.
# ---------------------------------------------------------------------------
ORES_DB_OWNER_ROLE="ores_${LABEL_LOWER}_owner"
ORES_DB_RW_ROLE="ores_${LABEL_LOWER}_rw"
ORES_DB_RO_ROLE="ores_${LABEL_LOWER}_ro"
ORES_DB_SERVICE_ROLE="ores_${LABEL_LOWER}_service"
ORES_DB_DDL_USER="ores_${LABEL_LOWER}_ddl_user"
ORES_DB_CLI_USER="ores_${LABEL_LOWER}_cli_user"
ORES_DB_WT_USER="ores_${LABEL_LOWER}_wt_user"
ORES_DB_HTTP_USER="ores_${LABEL_LOWER}_http_user"
ORES_DB_SHELL_USER="ores_${LABEL_LOWER}_shell_user"
ORES_DB_READONLY_USER="ores_${LABEL_LOWER}_readonly_user"
ORES_TEST_DB_DDL_USER="ores_${LABEL_LOWER}_test_ddl_user"
ORES_TEST_DB_DML_USER="ores_${LABEL_LOWER}_test_dml_user"

# ---------------------------------------------------------------------------
# Resolve ALL values before writing — must happen while the old .env is still
# in place so get_existing_val can read it.  Inline get_or_gen calls inside
# the write block would read from the already-truncated new file and generate
# fresh secrets on every run.
# ---------------------------------------------------------------------------
echo "Resolving passwords..."
ORES_DB_DDL_PASSWORD="$(get_or_gen ORES_DB_DDL_PASSWORD)"
ORES_DB_CLI_PASSWORD="$(get_or_gen ORES_DB_CLI_PASSWORD)"
ORES_DB_WT_PASSWORD="$(get_or_gen ORES_DB_WT_PASSWORD)"
ORES_DB_HTTP_PASSWORD="$(get_or_gen ORES_DB_HTTP_PASSWORD)"
ORES_DB_SHELL_PASSWORD="$(get_or_gen ORES_DB_SHELL_PASSWORD)"
ORES_DB_READONLY_PASSWORD="$(get_or_gen ORES_DB_READONLY_PASSWORD)"
ORES_TEST_DB_DDL_PASSWORD="$(get_or_gen ORES_TEST_DB_DDL_PASSWORD)"
ORES_TEST_DB_PASSWORD="$(get_or_gen ORES_TEST_DB_PASSWORD)"
ORES_HTTP_SERVER_JWT_SECRET="$(get_or_gen ORES_HTTP_SERVER_JWT_SECRET)"

# Service passwords — indexed arrays are bash 3.2 compatible
SERVICE_PW_VALS=()
for component in "${SERVICE_COMPONENTS[@]}"; do
    upper="$(echo "${component}" | tr '[:lower:]-' '[:upper:]_')"
    pw_key="ORES_${upper}_SERVICE_DB_PASSWORD"
    SERVICE_PW_VALS+=("$(get_or_gen "${pw_key}")")
done

# Grid node UUIDs
GRID_NODE_IDS=()
for n in 1 2 3 4 5; do
    key="ORES_GRID_NODE_${n}_HOST_ID"
    GRID_NODE_IDS+=("$(get_or_gen_uuid "${key}")")
done
echo "All credentials resolved."

# Encode PEM as a single line with literal \n separators
JWT_KEY_ONELINE="$(awk '{printf "%s\\n", $0}' "${IAM_KEY}")"

# ---------------------------------------------------------------------------
# Backup existing .env before overwriting
# ---------------------------------------------------------------------------
if [[ -f "${ENV_FILE}" ]]; then
    cp "${ENV_FILE}" "${ENV_FILE}.old"
    echo "Backed up existing .env to .env.old"
fi

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
EOF
# Only write ORES_PRESET when a preset was provided (not in CI).
if [[ -n "${PRESET}" ]]; then
    echo "ORES_PRESET=${PRESET}" >> "${ENV_FILE}"
fi
cat >> "${ENV_FILE}" << EOF
ORES_DATABASE_NAME=${DB_NAME}

# ---------------------------------------------------------------------------
# NATS (per-environment: port derived from label suffix)
# Run build/scripts/init-nats.sh to generate the server config and store dir.
# ---------------------------------------------------------------------------
ORES_NATS_PORT=${NATS_PORT}
ORES_NATS_URL=${NATS_URL}
ORES_NATS_MONITOR_PORT=${NATS_MONITOR_PORT}
ORES_NATS_MONITOR_URL=${NATS_MONITOR_URL}
ORES_NATS_SUBJECT_PREFIX=${NATS_PREFIX}
ORES_NATS_STORE_DIR=${NATS_STORE_DIR}
# mTLS: auto-enabled when certificates exist in build/keys/nats/.
# Run build/scripts/generate_nats_certs.sh to generate them.
# ORES_NATS_TLS_CERT/KEY are used by the Qt desktop client.
ORES_NATS_TLS_CA=${NATS_TLS_CA}
ORES_NATS_TLS_CERT=${NATS_TLS_CERT}
ORES_NATS_TLS_KEY=${NATS_TLS_KEY}

# ---------------------------------------------------------------------------
# Database admin (postgres superuser — for recreate_database.sh and psql)
# ---------------------------------------------------------------------------
PGPASSWORD=${PGPASSWORD_VAL}
ORES_TEST_DB_DATABASE=${DB_NAME}
ORES_TEST_DB_HOST=localhost

# ---------------------------------------------------------------------------
# Database roles and users (env-prefixed for isolation between environments)
# ---------------------------------------------------------------------------
ORES_DB_OWNER_ROLE=${ORES_DB_OWNER_ROLE}
ORES_DB_RW_ROLE=${ORES_DB_RW_ROLE}
ORES_DB_RO_ROLE=${ORES_DB_RO_ROLE}
ORES_DB_SERVICE_ROLE=${ORES_DB_SERVICE_ROLE}
ORES_DB_DDL_USER=${ORES_DB_DDL_USER}
ORES_DB_CLI_USER=${ORES_DB_CLI_USER}
ORES_DB_WT_USER=${ORES_DB_WT_USER}
ORES_DB_HTTP_USER=${ORES_DB_HTTP_USER}
ORES_DB_SHELL_USER=${ORES_DB_SHELL_USER}
ORES_DB_READONLY_USER=${ORES_DB_READONLY_USER}
ORES_TEST_DB_DDL_USER=${ORES_TEST_DB_DDL_USER}

# ---------------------------------------------------------------------------
# Script / DDL passwords (used by recreate_database.sh)
# ---------------------------------------------------------------------------
ORES_DB_DDL_PASSWORD=${ORES_DB_DDL_PASSWORD}
ORES_DB_CLI_PASSWORD=${ORES_DB_CLI_PASSWORD}
ORES_DB_WT_PASSWORD=${ORES_DB_WT_PASSWORD}
ORES_DB_HTTP_PASSWORD=${ORES_DB_HTTP_PASSWORD}
ORES_DB_SHELL_PASSWORD=${ORES_DB_SHELL_PASSWORD}
ORES_DB_READONLY_PASSWORD=${ORES_DB_READONLY_PASSWORD}

# ---------------------------------------------------------------------------
# Test connection credentials (read by recreate_database.sh and C++ tests)
# ---------------------------------------------------------------------------
ORES_TEST_DB_USER=${ORES_TEST_DB_DML_USER}
ORES_TEST_DB_PASSWORD=${ORES_TEST_DB_PASSWORD}
ORES_TEST_DB_DDL_PASSWORD=${ORES_TEST_DB_DDL_PASSWORD}

EOF

# ---------------------------------------------------------------------------
# NATS service DB credentials — auto-detected
# ---------------------------------------------------------------------------
{
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# NATS service DB credentials (read by C++ make_mapper)"
    echo "# ---------------------------------------------------------------------------"
    for i in "${!SERVICE_COMPONENTS[@]}"; do
        component="${SERVICE_COMPONENTS[$i]}"
        upper="$(echo "${component}" | tr '[:lower:]-' '[:upper:]_')"
        db_user="ores_${LABEL_LOWER}_${component}_service"
        echo ""
        echo "ORES_${upper}_SERVICE_DB_USER=${db_user}"
        echo "ORES_${upper}_SERVICE_DB_PASSWORD=${SERVICE_PW_VALS[$i]}"
        echo "ORES_${upper}_SERVICE_DB_DATABASE=${DB_NAME}"
    done
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# CLI DB credentials (read by C++ make_mapper(\"CLI\"))"
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_CLI_DB_USER=${ORES_DB_CLI_USER}"
    echo "ORES_CLI_DB_PASSWORD=${ORES_DB_CLI_PASSWORD}"
    echo "ORES_CLI_DB_DATABASE=${DB_NAME}"
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# Shell DB credentials (read by C++ make_mapper(\"SHELL\"))"
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_SHELL_DB_USER=${ORES_DB_SHELL_USER}"
    echo "ORES_SHELL_DB_PASSWORD=${ORES_DB_SHELL_PASSWORD}"
    echo "ORES_SHELL_DB_DATABASE=${DB_NAME}"
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# HTTP server DB credentials (read by C++ make_mapper(\"HTTP_SERVER\"))"
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_HTTP_SERVER_DB_USER=${ORES_DB_HTTP_USER}"
    echo "ORES_HTTP_SERVER_DB_PASSWORD=${ORES_DB_HTTP_PASSWORD}"
    echo "ORES_HTTP_SERVER_DB_DATABASE=${DB_NAME}"
    echo "ORES_HTTP_SERVER_JWT_SECRET=${ORES_HTTP_SERVER_JWT_SECRET}"
    echo ""
    echo "# ---------------------------------------------------------------------------"
    echo "# WT service DB credentials (read by C++ make_mapper(\"WT\"))"
    echo "# ---------------------------------------------------------------------------"
    echo "ORES_WT_DB_USER=${ORES_DB_WT_USER}"
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
    for i in "${!GRID_NODE_IDS[@]}"; do
        n=$((i + 1))
        echo "ORES_GRID_NODE_${n}_HOST_ID=${GRID_NODE_IDS[$i]}"
    done
} >> "${ENV_FILE}"

# Preserve any logging vars that were already set
echo "Checking for existing logging configuration..."
_log_level="$(get_existing_val ORES_TEST_LOG_LEVEL)"
if [[ -n "${_log_level}" ]]; then
    echo "Preserving test logging (level=${_log_level})"
    _log_console="$(get_existing_val ORES_TEST_LOG_CONSOLE)"
    {
        echo ""
        echo "# Test logging"
        echo "ORES_TEST_LOG_LEVEL=${_log_level}"
        echo "ORES_TEST_LOG_CONSOLE=${_log_console:-true}"
    } >> "${ENV_FILE}"
else
    echo "No test logging configuration found; skipping."
fi

echo "Setting file permissions..."
chmod 600 "${ENV_FILE}"
echo "Done."

# ---------------------------------------------------------------------------
# NATS setup — generate per-environment server config and JetStream store dir
# ---------------------------------------------------------------------------
echo ""
echo "--- NATS setup ---"
"${SCRIPT_DIR}/init-nats.sh"

echo ""
echo "=== Environment initialised for '${LABEL}' (db: ${DB_NAME}, NATS port: ${NATS_PORT}) ==="
echo ""
echo "Next steps:"
echo "  1. Create the database (first time only):"
echo "       ./projects/ores.sql/recreate_database.sh -y"
echo ""
echo "  2. Start NATS:"
echo "       nats-server --config build/config/nats-${LABEL}.conf"
echo ""
echo "  3. In Emacs, set up SQL connections:"
echo "       M-x ores-db/setup-connections"
echo ""
echo "  4. Start services via prodigy — they read ORES_PRESET and credentials from .env."
echo ""
echo "Logging:"
echo "  Enable:  ./build/scripts/init-environment.sh --enable-logging [level]"
echo "  Disable: ./build/scripts/init-environment.sh --disable-logging"
echo "  (Re-run 'cmake --preset <preset>' after toggling logging)"
echo ""
