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
#include "ores.ore/domain/bond_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_bond_option_mapper_roundtrip_tests.cpp
 * @brief Thing 3: mapper fidelity tests for BondOption and BondTRS types.
 *
 * For each example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map to bond_instrument via trade_mapper.
 *   3. Assert key economic fields are populated.
 *   4. Reverse-map back to ORE XSD trade.
 *   5. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite("ores.ore.bond.option.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][bond][option]");

using ores::ore::domain::portfolio;
using ores::ore::domain::bond_instrument_mapper;
using ores::ore::domain::bond_mapping_result;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

bond_mapping_result load_and_map(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_bond_instrument(p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

} // namespace

TEST_CASE("bond_option_mapper_roundtrip_bond_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_BondOption.xml");

    CHECK(r.instrument.trade_type_code == "BondOption");
    CHECK(!r.instrument.security_id.empty());

    // Reverse roundtrip
    const auto rt = bond_instrument_mapper::reverse_bond_option(r.instrument);
    REQUIRE(rt.BondOptionData);

    BOOST_LOG_SEV(lg, info) << "BondOption roundtrip passed. SecurityId: "
                            << r.instrument.security_id;
}

TEST_CASE("bond_option_mapper_roundtrip_bond_option_strike", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("BondOption_StrikePrice_StrikeYield.xml");

    CHECK(r.instrument.trade_type_code == "BondOption");

    // Reverse roundtrip
    const auto rt = bond_instrument_mapper::reverse_bond_option(r.instrument);
    REQUIRE(rt.BondOptionData);

    BOOST_LOG_SEV(lg, info) << "BondOption (strike) roundtrip passed.";
}

TEST_CASE("bond_option_mapper_roundtrip_bond_trs", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Bond_TRS.xml");

    CHECK(r.instrument.trade_type_code == "BondTRS");
    CHECK(!r.instrument.trs_return_type.empty());

    // Reverse roundtrip
    const auto rt = bond_instrument_mapper::reverse_bond_trs(r.instrument);
    REQUIRE(rt.BondTRSData);
    const bool has_price_type =
        !std::string(rt.BondTRSData->TotalReturnData.PriceType).empty();
    CHECK(has_price_type);

    BOOST_LOG_SEV(lg, info) << "BondTRS roundtrip passed. Return type: "
                            << r.instrument.trs_return_type;
}
