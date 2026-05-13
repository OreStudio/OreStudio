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

# Snapshot test: verify that generated service grant SQL files have not been
# modified outside of the codegen pipeline.
#
# If this check fails:
#   - You may have edited a generated file by hand (revert and use codegen instead)
#   - Or you regenerated the grants (update tests/service_db_grants.sha256 with
#     the new hash, then commit both the generated file and the updated hash)
#
# Usage:
#   ./utility/validate_grants_hash.sh

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SQL_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"
HASH_FILE="${SQL_DIR}/tests/service_db_grants.sha256"

if [[ ! -f "${HASH_FILE}" ]]; then
    echo "Error: hash file not found: ${HASH_FILE}" >&2
    exit 1
fi

echo "=== Validating generated service grant files ==="
cd "${SQL_DIR}"

# sha256sum -c expects paths relative to CWD
if sha256sum -c "${HASH_FILE}"; then
    echo ""
    echo "=== All grant file hashes match ==="
else
    echo "" >&2
    echo "FAILED: generated grant file(s) do not match recorded hashes." >&2
    echo "If you regenerated grants via codegen, update tests/service_db_grants.sha256:" >&2
    echo "  cd projects/ores.sql && sha256sum create/iam/iam_service_db_grants_create.sql > tests/service_db_grants.sha256" >&2
    exit 1
fi
