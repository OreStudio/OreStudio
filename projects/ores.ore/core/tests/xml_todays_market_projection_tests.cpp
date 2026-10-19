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
#include "ores.ore.core/domain/todays_market_projection.hpp"
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.ore.core/xml/generated/todays_market.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <iterator>
#include <optional>
#include <regex>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

/**
 * @file xml_todays_market_projection_tests.cpp
 * @brief The today's market document, projected and projected back.
 *
 * The corpus walk is the proof over every document. The cases below it cover
 * what the corpus cannot: a document that uses all twenty-four collections and
 * all twenty-four bindings, including the one collection no corpus file uses,
 * the optional attributes written empty and left out, and a row the projection
 * does not know.
 */

namespace {

const std::string tags("[ore][xml][roundtrip][todaysmarket]");

using ores::ore::domain::todays_market_projection;
namespace xg = ores::ore::xml::generated::todays_market;

// The collection kinds the schema declares, read from the children of its
// todaysmarket type apart from Configuration. The seeded collection kind
// lookup is pinned to the same list, so matching it here ties the projection's
// branches to the lookup.
std::set<std::string> schema_collection_kinds() {
    std::ifstream in(ores::testing::project_root::resolve("external/ore/xsd/todaysmarket.xsd"));
    const std::string schema((std::istreambuf_iterator<char>(in)),
                             std::istreambuf_iterator<char>());
    const auto start = schema.find("<xs:complexType name=\"todaysmarket\"");
    const auto root = schema.substr(start, schema.find("</xs:complexType>", start) - start);
    const std::regex child(R"re(<xs:element type="[A-Za-z]+"\s+name=\s*"([A-Za-z]+)")re");
    std::set<std::string> out;
    for (std::sregex_iterator it(root.begin(), root.end(), child), end; it != end; ++it)
        if ((*it)[1].str() != "Configuration")
            out.insert((*it)[1].str());
    return out;
}

// One collection with a single entry. An empty id leaves the attribute out.
std::string collection(const std::string& name,
                       const std::string& entry,
                       const std::string& id,
                       const std::string& attributes,
                       const std::string& text) {
    const auto id_attribute = id.empty() ? std::string() : " id=\"" + id + "\"";
    return "<" + name + id_attribute + "><" + entry + " " + attributes + ">" + text + "</" + entry +
           "></" + name + ">";
}

std::string named(const std::string& name,
                  const std::string& entry,
                  const std::string& key,
                  const std::string& text,
                  const std::string& id = "default") {
    return collection(name, entry, id, "name=\"" + key + "\"", text);
}

// Every collection the schema allows, each with an entry, and one configuration
// that binds all of them. Securities is left without an id, which the schema
// allows, so the absent id is proven to stay absent. The swaption collection
// holds an entry with both optional attributes, one with the key written
// empty, and one with neither.
std::string every_collection() {
    std::string d = "<TodaysMarket>";
    d += "<Configuration id=\"default\">";
    for (const auto* binding : {"YieldCurves",
                                "DiscountingCurves",
                                "IndexForwardingCurves",
                                "SwapIndexCurves",
                                "ZeroInflationIndexCurves",
                                "ZeroInflationCapFloorVolatilities",
                                "YYInflationIndexCurves",
                                "FxSpots",
                                "BaseCorrelations",
                                "FxVolatilities",
                                "SwaptionVolatilities",
                                "YieldVolatilities",
                                "CapFloorVolatilities",
                                "CDSVolatilities",
                                "DefaultCurves",
                                "YYInflationCapFloorVolatilities",
                                "EquityCurves",
                                "EquityVolatilities",
                                "Securities",
                                "CommodityCurves",
                                "CommodityVolatilities",
                                "Correlations",
                                "BondFutureVolatilities",
                                "IntradayPowerPriceCurves"})
        d += std::string("<") + binding + "Id>default</" + binding + "Id>";
    d += "</Configuration>";

    d += named("YieldCurves", "YieldCurve", "EUR1D", "Yield/EUR/EUR1D");
    d += named("IndexForwardingCurves", "Index", "EUR-EURIBOR-6M", "Yield/EUR/EUR6M");
    d += named(
        "ZeroInflationIndexCurves", "ZeroInflationIndexCurve", "EUHICPXT", "Inflation/EUHICPXT/ZC");
    d += named(
        "YYInflationIndexCurves", "YYInflationIndexCurve", "EUHICPXT", "Inflation/EUHICPXT/YY");
    d += named("YieldVolatilities", "YieldVolatility", "BOND", "YieldVolatility/BOND");
    d += named("CDSVolatilities", "CDSVolatility", "CPTY_A", "CDSVolatility/CPTY_A");
    d += named("DefaultCurves", "DefaultCurve", "CPTY_A", "Default/USD/CPTY_A");
    d += named("EquityCurves", "EquityCurve", "SP5", "Equity/USD/SP5");
    d += named("EquityVolatilities", "EquityVolatility", "SP5", "EquityVolatility/USD/SP5");
    d += named("Securities", "Security", "BOND1", "Security/BOND1", "");
    d += named("BaseCorrelations", "BaseCorrelation", "CDXIG", "BaseCorrelation/CDXIG");
    d += named("CommodityCurves", "CommodityCurve", "GOLD", "Commodity/USD/GOLD");
    d += named(
        "CommodityVolatilities", "CommodityVolatility", "GOLD", "CommodityVolatility/USD/GOLD");
    d += named("Correlations", "Correlation", "A&amp;B", "Correlation/A&amp;B");
    d += named("BondFutureVolatilities", "BondFutureVolatility", "TY", "BondFutureVolatility/TY");
    d += named("IntradayPowerPriceCurves", "IntradayPowerPriceCurve", "PJM", "Power/PJM");
    d += named("ZeroInflationCapFloorVolatilities",
               "ZeroInflationCapFloorVolatility",
               "EUHICPXT",
               "InflationCapFloorVolatility/EUHICPXT");
    d += named("YYInflationCapFloorVolatilities",
               "YYInflationCapFloorVolatility",
               "EUHICPXT",
               "YYInflationCapFloorVolatility/EUHICPXT");

    d += collection(
        "DiscountingCurves", "DiscountingCurve", "default", "currency=\"EUR\"", "Yield/EUR/EUR1D");
    d += collection(
        "DiscountingCurves", "DiscountingCurve", "inccy", "currency=\"EUR\"", "Yield/EUR/EUR1D");
    d += collection("FxSpots", "FxSpot", "default", "pair=\"EURUSD\"", "FX/EUR/USD");
    d += collection(
        "FxVolatilities", "FxVolatility", "default", "pair=\"EURUSD\"", "FXVolatility/EUR/USD");

    d += "<SwaptionVolatilities id=\"default\">"
         "<SwaptionVolatility key=\"EUR-EURIBOR-6M\" currency=\"EUR\">SwaptionVolatility/EUR/A"
         "</SwaptionVolatility>"
         "<SwaptionVolatility key=\"\">SwaptionVolatility/EUR/B</SwaptionVolatility>"
         "<SwaptionVolatility>SwaptionVolatility/EUR/C</SwaptionVolatility>"
         "</SwaptionVolatilities>";
    d += "<CapFloorVolatilities id=\"default\">"
         "<CapFloorVolatility currency=\"USD\">CapFloorVolatility/USD</CapFloorVolatility>"
         "</CapFloorVolatilities>";
    d += "<SwapIndexCurves id=\"default\"><SwapIndex name=\"EUR-CMS-30Y\">"
         "<Discounting>EUR-EONIA</Discounting></SwapIndex></SwapIndexCurves>";
    d += "</TodaysMarket>";
    return d;
}

std::optional<ores::ore::xml::record_difference> difference(const xg::todaysmarket& lhs,
                                                            const xg::todaysmarket& rhs) {
    return ores::ore::xml::compare_records(*xg::todays_market_document().root, &lhs, &rhs);
}

xg::todaysmarket round_trip(const xg::todaysmarket& original) {
    const auto mapped = todays_market_projection::map(original);
    return todays_market_projection::read(
        todays_market_projection::write(todays_market_projection::reverse(mapped)));
}

}

TEST_CASE("every today's market document in the corpus round trips", tags) {
    const auto root = ores::testing::project_root::resolve("external/ore/examples");
    REQUIRE(std::filesystem::is_directory(root));

    std::size_t files = 0;
    for (const auto& entry : std::filesystem::recursive_directory_iterator(root)) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        if (ores::ore::xml::read_root_element(entry.path()) != "TodaysMarket")
            continue;
        ++files;
        INFO(entry.path().string());

        std::ifstream file(entry.path(), std::ios::binary);
        const std::string content((std::istreambuf_iterator<char>(file)),
                                  std::istreambuf_iterator<char>());

        const auto original = todays_market_projection::read(content);
        const auto difference_found = difference(original, round_trip(original));
        if (difference_found)
            INFO(difference_found->path << ": " << difference_found->detail);
        CHECK_FALSE(difference_found);
    }
    INFO("files: " << files);
    CHECK(files == 99);
}

TEST_CASE("a document with every collection and every binding round trips", tags) {
    const auto original = todays_market_projection::read(every_collection());
    const auto mapped = todays_market_projection::map(original);

    std::set<std::string> collections;
    for (const auto& c : mapped.collections)
        collections.insert(c.collection);
    std::set<std::string> bindings;
    for (const auto& b : mapped.bindings)
        bindings.insert(b.collection);
    CHECK(collections.size() == 24);
    CHECK(bindings == collections);
    CHECK(collections == schema_collection_kinds());

    const auto found = difference(original, round_trip(original));
    if (found)
        INFO(found->path << ": " << found->detail);
    CHECK_FALSE(found);
}

TEST_CASE("an optional attribute left out is stored as null, and one written empty is not", tags) {
    const auto mapped =
        todays_market_projection::map(todays_market_projection::read(every_collection()));

    std::vector<const ores::analytics::domain::todays_market_entry*> swaptions;
    for (const auto& e : mapped.entries) {
        if (e.target.starts_with("SwaptionVolatility/"))
            swaptions.push_back(&e);
    }
    REQUIRE(swaptions.size() == 3);
    CHECK(swaptions[0]->key_value_2 == std::optional<std::string>("EUR"));
    CHECK(swaptions[1]->key_value == std::optional<std::string>(""));
    CHECK(!swaptions[1]->key_value_2);
    CHECK(!swaptions[2]->key_value);

    for (const auto& c : mapped.collections) {
        if (c.collection == "Securities")
            CHECK(!c.collection_id);
        else
            CHECK(c.collection_id);
    }
}

TEST_CASE("a collection or a binding the projection does not know is an error", tags) {
    const auto mapped =
        todays_market_projection::map(todays_market_projection::read(every_collection()));

    auto bad_collection = mapped;
    bad_collection.collections.front().collection = "NoSuchCurves";
    CHECK_THROWS_AS(todays_market_projection::reverse(bad_collection), std::runtime_error);

    auto bad_binding = mapped;
    bad_binding.bindings.front().collection = "NoSuchCurves";
    CHECK_THROWS_AS(todays_market_projection::reverse(bad_binding), std::runtime_error);
}

TEST_CASE("an entry without the key its collection requires is refused", tags) {
    auto mapped = todays_market_projection::map(todays_market_projection::read(every_collection()));
    for (auto& e : mapped.entries) {
        if (e.target == "Yield/EUR/EUR1D" && e.key_value == std::optional<std::string>("EUR1D")) {
            e.key_value.reset();
            break;
        }
    }
    CHECK_THROWS_AS(todays_market_projection::reverse(mapped), std::runtime_error);
}
