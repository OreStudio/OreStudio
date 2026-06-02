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
# or older than the required version.  Warns (no failure) when newer.
#
# The required version + changelog are canonical in the org-mode log
# doc/knowledge/architecture/env_format_version_log.org, read here via
# `compass env version`.  Record a new version with:
#   compass env new-version "what changed in .env"
# then re-run `compass env init` to write the new ORES_ENV_VERSION.
#
# NOTE: this shell guard is transitional.  When recreate_database.sh moves
# to `compass db recreate`, the check becomes an internal compass call
# (env_init.current_version) and this script is deleted.

set -euo pipefail

_SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
_COMPASS="${_SCRIPT_DIR}/../../ores.compass/compass.sh"
REQUIRED_ENV_VERSION="$("${_COMPASS}" env version)"

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
    echo "See doc/knowledge/architecture/env_format_version_log.org for details." >&2
    echo "" >&2
    echo "Fix: re-run 'compass env init' to regenerate .env:" >&2
    echo "  ./projects/ores.compass/compass.sh env init --preset ${ORES_PRESET:-<preset>} -y" >&2
    echo "" >&2
    exit 1
fi
