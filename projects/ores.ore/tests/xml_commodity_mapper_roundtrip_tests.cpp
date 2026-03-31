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
#include "ores.ore/domain/domain.hpp"
#include "ores.ore/domain/trade_mapper.hpp"
#include "ores.ore/domain/commodity_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_commodity_mapper_roundtrip_tests.cpp
 * @brief Mapper fidelity tests for commodity instrument types (Phase 7).
 *
 * For each example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map to commodity_instrument via trade_mapper.
 *   3. Assert key economic fields are populated.
 *   4. Reverse-map back to ORE XSD trade.
 *   5. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite(
    "ores.ore.commodity.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][commodity]");

using ores::ore::domain::portfolio;
using ores::ore::domain::commodity_instrument_mapper;
using ores::ore::domain::commodity_mapping_result;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

commodity_mapping_result load_and_map(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_commodity_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

} // namespace

TEST_CASE("commodity_mapper_roundtrip_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Forward.xml");

    CHECK(r.instrument.trade_type_code == "CommodityForward");
    CHECK(!r.instrument.commodity_code.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.quantity > 0.0);
    CHECK(r.instrument.fixed_price.has_value());
    CHECK(*r.instrument.fixed_price > 0.0);
    CHECK(!r.instrument.maturity_date.empty());

    const auto rt = commodity_instrument_mapper::reverse_commodity_forward(
        r.instrument);
    REQUIRE(rt.CommodityForwardData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityForward roundtrip passed. "
                            << r.instrument.commodity_code;
}

TEST_CASE("commodity_mapper_roundtrip_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Option.xml");

    CHECK(r.instrument.trade_type_code == "CommodityOption");
    CHECK(!r.instrument.commodity_code.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.quantity > 0.0);
    CHECK(r.instrument.strike_price.has_value());
    CHECK(!r.instrument.option_type.empty());
    CHECK(!r.instrument.maturity_date.empty());

    const auto rt = commodity_instrument_mapper::reverse_commodity_option(
        r.instrument);
    REQUIRE(rt.CommodityOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityOption roundtrip passed. Strike: "
                            << *r.instrument.strike_price;
}

TEST_CASE("commodity_mapper_roundtrip_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Swap_NYMEX_A7Q.xml");

    CHECK(r.instrument.trade_type_code == "CommoditySwap");
    CHECK(!r.instrument.commodity_code.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(!r.instrument.start_date.empty());
    CHECK(!r.instrument.maturity_date.empty());

    const auto rt = commodity_instrument_mapper::reverse_commodity_swap(
        r.instrument);
    REQUIRE(rt.SwapData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommoditySwap roundtrip passed. "
                            << r.instrument.commodity_code;
}

TEST_CASE("commodity_mapper_roundtrip_swaption", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Swaption_NYMEX_NG.xml");

    CHECK(r.instrument.trade_type_code == "CommoditySwaption");
    CHECK(!r.instrument.commodity_code.empty());
    CHECK(!r.instrument.swaption_expiry_date.empty());

    const auto rt = commodity_instrument_mapper::reverse_commodity_swaption(
        r.instrument);
    REQUIRE(rt.CommoditySwaptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommoditySwaption roundtrip passed. Expiry: "
                            << r.instrument.swaption_expiry_date;
}

TEST_CASE("commodity_mapper_roundtrip_variance_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Variance_Swap.xml");

    CHECK(r.instrument.trade_type_code == "CommodityVarianceSwap");
    CHECK(!r.instrument.commodity_code.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(!r.instrument.start_date.empty());
    CHECK(!r.instrument.maturity_date.empty());
    CHECK(r.instrument.variance_strike.has_value());

    const auto rt = commodity_instrument_mapper::reverse_commodity_variance_swap(
        r.instrument);
    REQUIRE(rt.CommodityVarianceSwapData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityVarianceSwap roundtrip passed. Strike: "
                            << *r.instrument.variance_strike;
}

TEST_CASE("commodity_mapper_roundtrip_apo", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_APO_NYMEX_CL.xml");

    CHECK(r.instrument.trade_type_code == "CommodityAveragePriceOption");
    CHECK(!r.instrument.commodity_code.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.quantity > 0.0);
    CHECK(r.instrument.strike_price.has_value());
    CHECK(!r.instrument.averaging_start_date.empty());
    CHECK(!r.instrument.averaging_end_date.empty());

    const auto rt = commodity_instrument_mapper::reverse_commodity_apo(
        r.instrument);
    REQUIRE(rt.CommodityAveragePriceOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityAveragePriceOption roundtrip passed. "
                            << r.instrument.commodity_code;
}

TEST_CASE("commodity_mapper_roundtrip_option_strip", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Option_Strip_NYMEX_NG.xml");

    CHECK(r.instrument.trade_type_code == "CommodityOptionStrip");
    CHECK(!r.instrument.commodity_code.empty());
    CHECK(!r.instrument.strip_frequency_code.empty());

    const auto rt = commodity_instrument_mapper::reverse_commodity_option_strip(
        r.instrument);
    REQUIRE(rt.CommodityOptionStripData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityOptionStrip roundtrip passed. "
                            << r.instrument.commodity_code;
}
