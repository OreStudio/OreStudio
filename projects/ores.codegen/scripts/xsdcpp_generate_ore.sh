#!/bin/bash
#
# Generate C++ domain types from ORE XSD schema
#
# This script is a wrapper around xsdcpp_generate.sh with ORE-specific paths.
#
# Usage: ./xsdcpp_generate_ore.sh [--reset-goldens]
#
# Options:
#   --reset-goldens  Delete the golden test dataset before regenerating so that
#                    golden tests re-bootstrap on the next test run. Use this
#                    whenever the XSD schema or xsdcpp serialization changes.
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIT_ROOT="$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)"
DOMAIN_HPP="${GIT_ROOT}/projects/ores.ore/core/include/ores.ore.core/domain/domain.hpp"

EXTRA_ARGS=()
for arg in "$@"; do
    if [ "$arg" = "--reset-goldens" ]; then
        EXTRA_ARGS+=(--reset-goldens assets/test_data/golden_dataset/Products)
    else
        EXTRA_ARGS+=("$arg")
    fi
done

# xsdcpp has no notion of this project's export-macro convention (see
# doc/knowledge/external/shared_library_symbol_visibility.org), so the
# ORES_ORE_CORE_EXPORT annotations on domain.hpp are a hand-patch that
# regeneration silently drops. Preserve the current domain.hpp as the
# reference for reapply_export_macros.py before overwriting it.
PRE_REGEN_DOMAIN_HPP="$(mktemp)"
trap 'rm -f "$PRE_REGEN_DOMAIN_HPP"' EXIT
cp "$DOMAIN_HPP" "$PRE_REGEN_DOMAIN_HPP"

"${SCRIPT_DIR}/xsdcpp_generate.sh" \
    --xsd external/ore/xsd/input.xsd \
    --project ores.ore \
    --namespace ores::ore \
    --name domain \
    --include-prefix ores.ore.core/domain \
    --header-output projects/ores.ore/core/include/ores.ore.core/domain \
    --cpp-output projects/ores.ore/core/src/domain \
    "${EXTRA_ARGS[@]}"

echo ""
echo "Reapplying ORES_ORE_CORE_EXPORT hand-patch onto regenerated domain.hpp..."
if ! grep -q '#include "ores.ore.core/export.hpp"' "$DOMAIN_HPP"; then
    sed -i '/#include "domain_xsd.hpp"/a #include "ores.ore.core/export.hpp"' "$DOMAIN_HPP"
fi
python3 "${GIT_ROOT}/scripts/reapply_export_macros.py" "$PRE_REGEN_DOMAIN_HPP" "$DOMAIN_HPP"
