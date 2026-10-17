#!/usr/bin/env bash
#
# Regenerates the codec tables from ORE's schemas.
#
# One invocation per document family. The schemas are the authority for the
# shape; the money list is the authority for what a numeric element means to
# us, because ORE types the same money concept three different ways.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
cd "$root"

xsd=external/ore/xsd
header=projects/ores.ore/core/include/ores.ore.core/xml/generated
source=projects/ores.ore/core/src/xml/generated

python3 "$here/generate_xml_bindings.py" \
    --schema "$xsd/calendaradjustment.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element CalendarAdjustments \
    --root-type calendaradjustment \
    --accessor calendar_adjustments_document \
    --include "ores.ore.core/xml/generated/calendar_adjustments.hpp" \
    --header "$header/calendar_adjustments.hpp" \
    --source "$source/calendar_adjustments.cpp"
