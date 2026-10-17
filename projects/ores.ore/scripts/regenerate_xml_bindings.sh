#!/usr/bin/env bash
#
# Regenerates the codec tables from ORE's schemas.
#
# One invocation per document family. The schemas are the authority for the
# shape; the money list is the authority for what a numeric element means to
# us, because ORE types the same money concept three different ways.
#
# XML_BINDINGS_OUT writes the same files under another root, which is how the
# drift check compares what the schemas produce with what is committed.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
cd "$root"

out="${XML_BINDINGS_OUT:-$root}"
xsd=external/ore/xsd
header="$out/projects/ores.ore/core/include/ores.ore.core/xml/generated"
source="$out/projects/ores.ore/core/src/xml/generated"

mkdir -p "$header" "$source"

generate() {
    python3 "$here/generate_xml_bindings.py" "$@"
}

generate \
    --schema "$xsd/calendaradjustment.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element CalendarAdjustments \
    --root-type calendaradjustment \
    --accessor calendar_adjustments_document \
    --include "ores.ore.core/xml/generated/calendar_adjustments.hpp" \
    --header "$header/calendar_adjustments.hpp" \
    --source "$source/calendar_adjustments.cpp"

# Spread is xs:float in the schema and money in our model, which is the same
# shape as the bond review's defect and the reason the money list exists.
generate \
    --schema "$xsd/iborfallbackconfig.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element IborFallbackConfig \
    --root-type IborFallbackConfig \
    --accessor ibor_fallback_config_document \
    --include "ores.ore.core/xml/generated/ibor_fallback_config.hpp" \
    --header "$header/ibor_fallback_config.hpp" \
    --source "$source/ibor_fallback_config.cpp" \
    --money Spread

# The correlation value type lives in simulation.xsd, so that schema is read
# only to resolve it. Nothing else from it is emitted here.
generate \
    --schema "$xsd/counterparty.xsd" \
    --schema "$xsd/simulation.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element CounterpartyInformation \
    --root-type counterpartyInformation \
    --accessor counterparty_information_document \
    --include "ores.ore.core/xml/generated/counterparty_information.hpp" \
    --header "$header/counterparty_information.hpp" \
    --source "$source/counterparty_information.cpp"

generate \
    --schema "$xsd/currencyconfig.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element CurrencyConfig \
    --root-type currencyConfig \
    --accessor currency_config_document \
    --include "ores.ore.core/xml/generated/currency_config.hpp" \
    --header "$header/currency_config.hpp" \
    --source "$source/currency_config.cpp"
