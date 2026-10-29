/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.testing/project_root.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <vector>

namespace {

namespace fs = std::filesystem;

using ores::ore::xml::document_families;
using ores::ore::xml::document_family;
using ores::ore::xml::document_kind;
using ores::ore::xml::family_of;

/** The root element each generated family writes, read from its own source. */
std::map<std::string, std::string> generated_roots() {
    std::map<std::string, std::string> roots;
    const auto directory =
        ores::testing::project_root::resolve("projects/ores.ore/core/src/xml/generated");
    for (const auto& entry : fs::directory_iterator(directory)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".cpp")
            continue;
        std::ifstream in(entry.path());
        std::stringstream buffer;
        buffer << in.rdbuf();
        const auto text = buffer.str();
        const auto at = text.find("doc{\"");
        if (at == std::string::npos)
            continue;
        const auto from = at + 5;
        const auto to = text.find('"', from);
        roots[entry.path().stem().string()] = text.substr(from, to - from);
    }
    return roots;
}

/** Every enumerator, so a value cannot be added to the enum and forgotten. */
const std::vector<document_kind>& every_kind() {
    static const std::vector<document_kind> kinds{
        document_kind::portfolio,
        document_kind::currency_config,
        document_kind::calendar_adjustments,
        document_kind::conventions,
        document_kind::curve_configuration,
        document_kind::pricing_engines,
        document_kind::todays_market,
        document_kind::credit_simulation,
        document_kind::counterparty_information,
        document_kind::ibor_fallback_config,
        document_kind::netting_set_definitions,
        document_kind::stress_testing,
        document_kind::run_document,
    };
    return kinds;
}

}

TEST_CASE("every_document_kind_the_generator_emits_has_a_family", "[ore][xml][family]") {
    const auto roots = generated_roots();

    REQUIRE(roots.size() == 13);
    CAPTURE(roots.size());
    for (const auto& [family, root] : roots) {
        INFO("generated family: " << family << " with root " << root);
        const auto found = std::ranges::find_if(
            document_families(), [&root](const auto& one) { return one.root_element == root; });
        CHECK(found != document_families().end());
    }
}

TEST_CASE("every_document_kind_has_one_row_in_the_table", "[ore][xml][family]") {
    CHECK(document_families().size() == every_kind().size());
    for (const auto kind : every_kind()) {
        INFO("kind: " << static_cast<int>(kind));
        // family_of refuses a kind with no row, so the call is the assertion.
        CHECK(family_of(kind).kind == kind);
    }
}

TEST_CASE("a_family_nothing_converts_says_so", "[ore][xml][family]") {
    const auto& counterparty = family_of(document_kind::counterparty_information);

    CHECK_FALSE(counterparty.read);
    CHECK_FALSE(counterparty.write);
    CHECK(counterparty.surface.empty());
    CHECK(family_of(document_kind::portfolio).surface == "portfolio import and export");
}

TEST_CASE("every_schema_in_the_ore_xsd_tree_is_recorded", "[ore][xml][family]") {
    // Which schema a family comes from, or that a schema declares no document of
    // ours. A new schema fails this case until it is said which it is.
    const std::map<std::string, document_kind> document_schemas{
        {"calendaradjustment", document_kind::calendar_adjustments},
        {"conventions", document_kind::conventions},
        {"counterparty", document_kind::counterparty_information},
        {"creditsimulation", document_kind::credit_simulation},
        {"currencyconfig", document_kind::currency_config},
        {"curveconfig", document_kind::curve_configuration},
        {"iborfallbackconfig", document_kind::ibor_fallback_config},
        {"nettingsetdefinitions", document_kind::netting_set_definitions},
        {"ore", document_kind::run_document},
        {"pricingengines", document_kind::pricing_engines},
        {"stress", document_kind::stress_testing},
        {"todaysmarket", document_kind::todays_market},
    };
    const std::set<std::string> other_schemas{
        "baselTrafficLightconfig", "collateralbalance", "historicalreturnconfig", "input",
        "instruments",            "intradaypowerloaddata", "ore_types",  "referencedata",
        "scriptlibrary",          "sensitivity",          "simmcalibration",
        "simulation",
    };

    const auto directory = ores::testing::project_root::resolve("external/ore/xsd");
    std::size_t seen = 0;
    for (const auto& entry : fs::directory_iterator(directory)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xsd")
            continue;
        const auto schema = entry.path().stem().string();
        ++seen;
        INFO("schema: " << schema);
        const auto documented = document_schemas.contains(schema) || other_schemas.contains(schema);
        CHECK(documented);
    }
    CHECK(seen == document_schemas.size() + other_schemas.size());
}
