#!/usr/bin/env bash
#
# Regenerates the codec tables from ORE's schemas.
#
# One invocation per document family. The schemas are the authority for the
# shape; the exact-decimal list is the authority for which numbers must keep the
# digits the document wrote, because ORE types the same quantity as xs:float,
# xs:double and xs:string in different places.
#
# XML_BINDINGS_OUT writes the same files under another root, which is how the
# drift check compares what the schemas produce with what is committed.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
cd "$root"

out="${XML_BINDINGS_OUT:-$root}"

# Generate into a staging tree first. A family that fails part way through
# then leaves the committed files untouched, rather than half regenerated.
staging="$(mktemp -d)"
trap 'rm -rf "$staging"' EXIT
xsd=external/ore/xsd
header="$staging/projects/ores.ore/core/include/ores.ore.core/xml/generated"
source="$staging/projects/ores.ore/core/src/xml/generated"

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
    --namespace ores::ore::xml::generated::calendar_adjustments \
    --include "ores.ore.core/xml/generated/calendar_adjustments.hpp" \
    --header "$header/calendar_adjustments.hpp" \
    --source "$source/calendar_adjustments.cpp"

# Spread is xs:float in the schema and money in our model, which is the same
# shape as the bond review's defect and the reason the exact-decimal list
# exists.
generate \
    --schema "$xsd/iborfallbackconfig.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element IborFallbackConfig \
    --root-type IborFallbackConfig \
    --accessor ibor_fallback_config_document \
    --namespace ores::ore::xml::generated::ibor_fallback_config \
    --include "ores.ore.core/xml/generated/ibor_fallback_config.hpp" \
    --header "$header/ibor_fallback_config.hpp" \
    --source "$source/ibor_fallback_config.cpp" \
    --exact-decimal Spread

# The correlation value type lives in simulation.xsd, so that schema is read
# only to resolve it. Nothing else from it is emitted here.
generate \
    --schema "$xsd/counterparty.xsd" \
    --schema "$xsd/simulation.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element CounterpartyInformation \
    --root-type counterpartyInformation \
    --accessor counterparty_information_document \
    --namespace ores::ore::xml::generated::counterparty_information \
    --include "ores.ore.core/xml/generated/counterparty_information.hpp" \
    --header "$header/counterparty_information.hpp" \
    --source "$source/counterparty_information.cpp"

generate \
    --schema "$xsd/todaysmarket.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element TodaysMarket \
    --root-type todaysmarket \
    --accessor todays_market_document \
    --namespace ores::ore::xml::generated::todays_market \
    --include "ores.ore.core/xml/generated/todays_market.hpp" \
    --header "$header/todays_market.hpp" \
    --source "$source/todays_market.cpp"

# The netting set group is an abstract element in instruments.xsd with two
# substitution members, so that schema is read only to resolve it.
generate \
    --schema "$xsd/nettingsetdefinitions.xsd" \
    --schema "$xsd/instruments.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element NettingSetDefinitions \
    --root-type nettingsetdefinitions \
    --accessor netting_set_definitions_document \
    --namespace ores::ore::xml::generated::netting_set_definitions \
    --include "ores.ore.core/xml/generated/netting_set_definitions.hpp" \
    --header "$header/netting_set_definitions.hpp" \
    --source "$source/netting_set_definitions.cpp"

generate \
    --schema "$xsd/conventions.xsd" \
    --schema "$xsd/intradaypowerloaddata.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element Conventions \
    --root-type conventions \
    --accessor conventions_document \
    --namespace ores::ore::xml::generated::conventions \
    --include "ores.ore.core/xml/generated/conventions.hpp" \
    --header "$header/conventions.hpp" \
    --source "$source/conventions.cpp"

generate \
    --schema "$xsd/currencyconfig.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element CurrencyConfig \
    --root-type currencyConfig \
    --accessor currency_config_document \
    --namespace ores::ore::xml::generated::currency_config \
    --include "ores.ore.core/xml/generated/currency_config.hpp" \
    --header "$header/currency_config.hpp" \
    --source "$source/currency_config.cpp"

generate \
    --schema "$xsd/pricingengines.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element PricingEngines \
    --root-type pricingengines \
    --accessor pricing_engines_document \
    --namespace ores::ore::xml::generated::pricing_engines \
    --include "ores.ore.core/xml/generated/pricing_engines.hpp" \
    --header "$header/pricing_engines.hpp" \
    --source "$source/pricing_engines.cpp"

# The curve configuration writes the IBOR fallback spread as xs:float as well,
# and it is the same amount the fallback configuration document states, so it is
# carried as an exact decimal here too.
generate \
    --schema "$xsd/curveconfig.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element CurveConfiguration \
    --root-type curveconfiguration \
    --accessor curve_configuration_document \
    --namespace ores::ore::xml::generated::curve_configuration \
    --include "ores.ore.core/xml/generated/curve_configuration.hpp" \
    --header "$header/curve_configuration.hpp" \
    --source "$source/curve_configuration.cpp" \
    --exact-decimal Spread

# The portfolio document reaches the instrument types, which live in
# instruments.xsd and reach the reference data, simulation, script library and
# power load schemas, so all of them are read with input.xsd. It also reaches a
# model group, substitution members that write their own types, and types that
# hold themselves, which are carried as index rows into an arena.
#
# Every real in this family is carried as an exact decimal. ORE writes a rate,
# a spread or a strike from a double, which spells 7342.8000000000002 and
# 0.050000000000000003, and the shortest spelling of the same double is 7342.8
# and 0.05. The value a document states is its text, so the table keeps the
# digits rather than the nearest double. A text element is not a real and keeps
# its text, which is how the same name can be a script variable in one type and
# a number in another.
generate \
    --schema "$xsd/input.xsd" \
    --schema "$xsd/instruments.xsd" \
    --schema "$xsd/referencedata.xsd" \
    --schema "$xsd/simulation.xsd" \
    --schema "$xsd/scriptlibrary.xsd" \
    --schema "$xsd/simmcalibration.xsd" \
    --schema "$xsd/historicalreturnconfig.xsd" \
    --schema "$xsd/counterparty.xsd" \
    --schema "$xsd/intradaypowerloaddata.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element Portfolio \
    --root-type portfolio \
    --accessor portfolio_document \
    --namespace ores::ore::xml::generated::portfolio \
    --include "ores.ore.core/xml/generated/portfolio.hpp" \
    --header "$header/portfolio.hpp" \
    --source "$source/portfolio.cpp" \
    --exact-decimal '*' 

# The corpus names the run document 235 ways, six of them without the "ore"
# prefix, so the harness finds the family by its root element. The family keeps
# the schema's name; the product calls it the run document.
generate \
    --schema "$xsd/ore.xsd" \
    --root-element ORE \
    --root-type ore \
    --accessor ore_document \
    --namespace ores::ore::xml::generated::ore \
    --include "ores.ore.core/xml/generated/ore.hpp" \
    --header "$header/ore.hpp" \
    --source "$source/ore.cpp"

# shiftTypeEntry lives in sensitivity.xsd, which stress.xsd references but does
# not include, so that schema is read with it.
generate \
    --schema "$xsd/stress.xsd" \
    --schema "$xsd/sensitivity.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element StressTesting \
    --root-type stresstesting \
    --accessor stress_testing_document \
    --namespace ores::ore::xml::generated::stress_testing \
    --include "ores.ore.core/xml/generated/stress_testing.hpp" \
    --header "$header/stress_testing.hpp" \
    --source "$source/stress_testing.cpp"

generate \
    --schema "$xsd/creditsimulation.xsd" \
    --schema "$xsd/ore_types.xsd" \
    --root-element CreditSimulation \
    --root-type creditsimulation \
    --accessor credit_simulation_document \
    --namespace ores::ore::xml::generated::credit_simulation \
    --include "ores.ore.core/xml/generated/credit_simulation.hpp" \
    --header "$header/credit_simulation.hpp" \
    --source "$source/credit_simulation.cpp"

# The repository formats its C++ with clang-format and a check enforces it, so
# the generator's output has to be formatted too, not only correct.
# The style comes from the checkout, not from the staging tree: clang-format
# discovers a .clang-format by walking up from each file, and a staging tree
# outside the repository has none, so the files would be left in its default
# style and the repository's format gate would reject them.
if command -v clang-format >/dev/null 2>&1; then
    find "$staging" \( -name "*.hpp" -o -name "*.cpp" \) -print0 |
        xargs -0 clang-format -i -style="file:$root/.clang-format"
fi

mkdir -p "$out/projects/ores.ore/core/include/ores.ore.core/xml/generated"
mkdir -p "$out/projects/ores.ore/core/src/xml/generated"
cp "$header"/*.hpp "$out/projects/ores.ore/core/include/ores.ore.core/xml/generated/"
cp "$source"/*.cpp "$out/projects/ores.ore/core/src/xml/generated/"
