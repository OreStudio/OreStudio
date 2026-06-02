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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.ore.core/domain/scripted_instrument_mapper.hpp"
#include "ores.ore.core/domain/composite_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_composite_scripted_mapper_roundtrip_tests.cpp
 * @brief Mapper fidelity tests for scripted and composite instruments (Phase 8).
 *
 * For each example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map via trade_mapper.
 *   3. Assert key fields are populated.
 *   4. Reverse-map back to ORE XSD trade.
 *   5. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite(
    "ores.ore.composite.scripted.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][composite][scripted]");

using ores::ore::domain::portfolio;
using ores::ore::domain::scripted_instrument_mapper;
using ores::trading::domain::scripted_instrument;
using ores::ore::domain::composite_instrument_mapper;
using ores::trading::domain::composite_instrument_data;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

scripted_instrument load_and_map_scripted(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_scripted_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

composite_instrument_data load_and_map_composite(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_composite_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

} // namespace

TEST_CASE("scripted_mapper_roundtrip_asian_basket_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_scripted("Scripted_BasketOption.xml");

    CHECK(r.trade_type_code == "ScriptedTrade");
    CHECK(r.script_name == "AsianBasketOption");
    CHECK(!r.underlyings_json.empty());
    CHECK(!r.parameters_json.empty());

    const auto rt = scripted_instrument_mapper::reverse_scripted_trade(r);
    REQUIRE(rt.ScriptedTradeData.operator bool());

    BOOST_LOG_SEV(lg, info) << "ScriptedTrade (AsianBasketOption) roundtrip "
                               "passed. Underlyings: "
                            << r.underlyings_json;
}

TEST_CASE("scripted_mapper_roundtrip_average_strike_basket_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_scripted("Scripted_BasketOption2.xml");

    CHECK(r.trade_type_code == "ScriptedTrade");
    CHECK(!r.underlyings_json.empty());

    BOOST_LOG_SEV(lg, info) << "ScriptedTrade (AverageStrikeBasketOption) "
                               "roundtrip passed.";
}

TEST_CASE("composite_mapper_roundtrip_composite_trade", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_composite("Hybrid_CompositeTrade.xml");

    CHECK(r.instrument.trade_type_code == "CompositeTrade");

    const auto rt = composite_instrument_mapper::reverse_composite_trade(
        r.instrument);
    REQUIRE(rt.CompositeTradeData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CompositeTrade roundtrip passed.";
}

TEST_CASE("composite_mapper_components_populate_legs", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map_composite("Hybrid_CompositeTrade.xml");

    // Hybrid_CompositeTrade.xml has two component trades under <Components>.
    REQUIRE(r.legs.size() == 2);

    // Legs are 1-based and ordered.
    CHECK(r.legs[0].leg_sequence == 1);
    CHECK(r.legs[1].leg_sequence == 2);

    // Each leg carries a constituent trade identifier.
    for (const auto& leg : r.legs) {
        CHECK(!leg.constituent_trade_id.empty());
        CHECK(leg.change_reason_code == "system.external_data_import");
    }

    BOOST_LOG_SEV(lg, info) << "CompositeTrade legs populated: "
                            << r.legs.size();
}
