#!/usr/bin/env bash
# -*- mode: shell-script; tab-width: 4; indent-tabs-mode: nil -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# validate_env_version.sh — Verify the .env file is compatible with the
# current database scripts.  Sourced (not executed) by recreate_database.sh
# so that it can read variables already loaded into the caller's environment.
#
# Fails with exit 1 and a clear explanation when ORES_ENV_VERSION is missing
# or older than REQUIRED_ENV_VERSION.  Warns (no failure) when newer.
#
# To bump the version:
#   1. Increment REQUIRED_ENV_VERSION below.
#   2. Add a comment line to the Changelog block below.
#   3. Increment ENV_VERSION in build/scripts/init-environment.sh.
#   4. Add the corresponding comment in init-environment.sh changelog too.

set -euo pipefail

REQUIRED_ENV_VERSION=6

# Changelog:
#   1: Initial env versioning; renamed ORES_COMPUTE_WRAPPER_USER -> ORES_DB_COMPUTE_WRAPPER_USER
#   2: Removed ORES_WT_PORT; WT port is now owned by the controller via ORES_CONTROLLER_SERVICE_WT_PORT
#   3: Added ORES_WORKSPACE_SERVICE_DB_* for new workspace service
#   4: Added ORES_SITE_PORT for the local documentation site server
#   5: Added org-roam knowledge graph integration to site build
#   6: Added ORES_SHELL_NATS_* for ores.shell mTLS auto-connect support

CURRENT_VERSION="${ORES_ENV_VERSION:-0}"

if [[ "${CURRENT_VERSION}" -gt "${REQUIRED_ENV_VERSION}" ]]; then
    echo "Warning: .env version ${CURRENT_VERSION} is newer than required ${REQUIRED_ENV_VERSION} — proceeding." >&2
    return 0 2>/dev/null || true
fi

if [[ "${CURRENT_VERSION}" -lt "${REQUIRED_ENV_VERSION}" ]]; then
    echo "" >&2
    echo "=== ERROR: .env is out of date ===" >&2
    echo "" >&2
    echo "  Your .env:  version ${CURRENT_VERSION}" >&2
    echo "  Required:   version ${REQUIRED_ENV_VERSION}" >&2
    echo "" >&2
    echo "See the Changelog in validate_env_version.sh for details." >&2
    echo "" >&2
    echo "Fix: re-run init-environment.sh to regenerate .env:" >&2
    echo "  ./build/scripts/init-environment.sh --preset ${ORES_PRESET:-<preset>} -y" >&2
    echo "" >&2
    exit 1
fi
