#!/usr/bin/env bash
# -*- mode: sh; tab-width: 4; indent-tabs-mode: nil; sh-basic-offset: 4 -*-
#
# Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
#
# diff-environment.sh - Show a unified diff between .env.old and .env.
#
# Produces a unified diff of the previous .env (.env.old) and the current
# .env. This is a developer tool — output contains secrets in plain text and
# should not be shared. Exits 0 if the files are identical, 1 if they differ
# (standard diff exit codes), 2 on error (missing files).
#
# Usage:
#   ./build/scripts/diff-environment.sh
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECKOUT_ROOT="$(cd "${SCRIPT_DIR}/../.." && pwd)"
ENV_FILE="${CHECKOUT_ROOT}/.env"
ENV_OLD="${CHECKOUT_ROOT}/.env.old"

if [[ ! -f "${ENV_OLD}" ]]; then
    echo "No .env.old found — run init-environment.sh at least twice to generate a diff." >&2
    exit 2
fi

if [[ ! -f "${ENV_FILE}" ]]; then
    echo "No .env found." >&2
    exit 2
fi

diff -u "${ENV_OLD}" "${ENV_FILE}" || true
