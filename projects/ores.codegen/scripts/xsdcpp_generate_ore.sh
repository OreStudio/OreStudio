#!/bin/bash
#
# Generate C++ domain types from ORE XSD schema
#
# This script is a wrapper around xsdcpp_generate.sh with ORE-specific paths.
#
# Usage: ./xsdcpp_generate_ore.sh
#

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

exec "${SCRIPT_DIR}/xsdcpp_generate.sh" \
    --xsd external/ore/xsd/input.xsd \
    --project ores.ore \
    --namespace ores::ore \
    --name domain \
    --include-prefix ores.ore/domain
