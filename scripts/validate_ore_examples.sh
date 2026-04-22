#!/usr/bin/env bash
#
# Validate all XML files in external/ore/examples against the ORE XSD schema.
#
# Usage: ./scripts/validate_ore_examples.sh [examples_dir] [xsd_dir]
#
# Defaults:
#   examples_dir  external/ore/examples
#   xsd_dir       external/ore/xsd
#
# Requires xmllint (libxml2-utils on Debian/Ubuntu).
#
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

EXAMPLES_DIR="${1:-${REPO_ROOT}/external/ore/examples}"
XSD_DIR="${2:-${REPO_ROOT}/external/ore/xsd}"
XSD="${XSD_DIR}/input.xsd"

if ! command -v xmllint &>/dev/null; then
    echo "ERROR: xmllint not found. Install with: sudo apt-get install libxml2-utils" >&2
    exit 1
fi

if [[ ! -f "${XSD}" ]]; then
    echo "ERROR: schema not found: ${XSD}" >&2
    exit 1
fi

if [[ ! -d "${EXAMPLES_DIR}" ]]; then
    echo "ERROR: examples directory not found: ${EXAMPLES_DIR}" >&2
    exit 1
fi

echo "Schema : ${XSD}"
echo "Examples: ${EXAMPLES_DIR}"
echo ""

total=0
passed=0
failed=0
failed_files=()

while IFS= read -r -d '' file; do
    total=$((total + 1))
    output=$(xmllint --schema "${XSD}" --path "${XSD_DIR}" --noout "${file}" 2>&1)
    if [[ $? -eq 0 ]]; then
        passed=$((passed + 1))
    else
        failed=$((failed + 1))
        failed_files+=("${file}")
        echo "FAIL: ${file#${REPO_ROOT}/}"
        echo "${output}" | sed 's/^/  /'
        echo ""
    fi
done < <(find "${EXAMPLES_DIR}" -name "*.xml" -print0 | sort -z)

echo "---"
echo "Total : ${total}"
echo "Passed: ${passed}"
echo "Failed: ${failed}"

if [[ ${failed} -gt 0 ]]; then
    echo ""
    echo "Failed files:"
    for f in "${failed_files[@]}"; do
        echo "  ${f#${REPO_ROOT}/}"
    done
    exit 1
fi
