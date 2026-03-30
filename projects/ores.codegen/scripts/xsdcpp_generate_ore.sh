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

EXTRA_ARGS=()
for arg in "$@"; do
    if [ "$arg" = "--reset-goldens" ]; then
        EXTRA_ARGS+=(--reset-goldens assets/test_data/golden_dataset/Products)
    else
        EXTRA_ARGS+=("$arg")
    fi
done

exec "${SCRIPT_DIR}/xsdcpp_generate.sh" \
    --xsd external/ore/xsd/input.xsd \
    --project ores.ore \
    --namespace ores::ore \
    --name domain \
    --include-prefix ores.ore/domain \
    "${EXTRA_ARGS[@]}"
