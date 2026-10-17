#!/usr/bin/env bash
#
# Proves the ORE XML codec over the corpus: the [ore][xml][corpus] test reads
# every example, writes it back and compares the two, leaving the written
# documents in the build tree. This hands that output to xmllint, because
# neither half alone says the document survived. Parsing proves nothing about
# the written form, and validation proves nothing about the values.
set -euo pipefail

root="$(git rev-parse --show-toplevel)"
cd "$root"

tests=""
for candidate in build/output/*/publish/bin/ores.ore.core.tests; do
    [[ -f "$candidate" ]] && tests="$candidate"
done
if [[ -z "$tests" ]]; then
    echo "no built ores.ore.core.tests under build/output" >&2
    exit 1
fi

roundtrip="$(dirname "$(dirname "$tests")")/../xml-roundtrip"
if [[ ! -d "$roundtrip" ]]; then
    echo "the corpus test wrote nothing to $roundtrip" >&2
    exit 1
fi

exec ./scripts/validate_ore_examples.sh "$roundtrip" external/ore/xsd
