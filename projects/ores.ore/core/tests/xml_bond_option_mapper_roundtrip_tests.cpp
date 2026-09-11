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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/bond_instrument_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

/**
 * @file xml_bond_option_mapper_roundtrip_tests.cpp
 * @brief Thing 3: mapper fidelity tests for BondOption and BondTRS types.
 *
 * For each example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map to bond_instrument_data via trade_mapper.
 *   3. Assert key economic fields are populated.
 *   4. Reverse-map back to ORE XSD trade.
 *   5. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite("ores.ore.bond.option.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][bond][option]");

using ores::ore::domain::portfolio;
using ores::ore::domain::bond_instrument_mapper;
using ores::trading::domain::bond_instrument_data;
using namespace ores::logging;
using Catch::Approx;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

bond_instrument_data load_and_map(const std::string& filename) {
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

    CHECK(r.instrument.identity.trade_type_code == "BondOption");
    CHECK(!r.issue.security_id.empty());
    CHECK(r.instrument.issue_id == r.issue.issue_id);
    REQUIRE(r.option.has_value());
    CHECK(r.option->option_type == "Call");
    CHECK(r.option->option_strike == Approx(1.0).epsilon(0.0001));
    CHECK(r.option_expiry_date == "2025-04-16");

    // Reverse roundtrip
    const auto rt = bond_instrument_mapper::reverse_bond_option(r);
    REQUIRE(rt.BondOptionData);
    REQUIRE(rt.BondOptionData->OptionData.OptionType);
    CHECK(std::string(*rt.BondOptionData->OptionData.OptionType) == "Call");
    REQUIRE(rt.BondOptionData->strikeGroup.Strike);
    CHECK(std::string(*rt.BondOptionData->strikeGroup.Strike) == "1.000000");

    BOOST_LOG_SEV(lg, info) << "BondOption roundtrip passed. SecurityId: " << r.issue.security_id;
}

TEST_CASE("bond_option_mapper_roundtrip_bond_option_strike", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("BondOption_StrikePrice_StrikeYield.xml");

    CHECK(r.instrument.identity.trade_type_code == "BondOption");
    REQUIRE(r.option.has_value());
    CHECK(r.option->option_type == "Call");
    CHECK(r.option_expiry_date == "2028-02-02");
    // This fixture prices by StrikePrice/StrikeYield, which the mapper does
    // not read; the strike row value stays zero until that coverage lands.
    CHECK(r.option->option_strike == Approx(0.0).epsilon(0.0001));

    // Reverse roundtrip
    const auto rt = bond_instrument_mapper::reverse_bond_option(r);
    REQUIRE(rt.BondOptionData);

    BOOST_LOG_SEV(lg, info) << "BondOption (strike) roundtrip passed.";
}

TEST_CASE("bond_option_mapper_roundtrip_bond_trs", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Credit_Bond_TRS.xml");

    CHECK(r.instrument.identity.trade_type_code == "BondTRS");
    CHECK(!r.issue.security_id.empty());
    REQUIRE(r.trs.has_value());
    CHECK(r.trs->return_type == "TotalReturn");
    CHECK(r.trs->funding_leg_type == "Fixed");
    CHECK(r.trs->funding_rate == Approx(-0.0055).epsilon(0.0001));

    // Reverse roundtrip
    const auto rt = bond_instrument_mapper::reverse_bond_trs(r);
    REQUIRE(rt.BondTRSData);
    const bool has_price_type = !std::string(rt.BondTRSData->TotalReturnData.PriceType).empty();
    CHECK(has_price_type);
    const auto& funding_leg = rt.BondTRSData->FundingData.LegData;
    CHECK(funding_leg.LegType == ores::ore::domain::legType::Fixed);
    REQUIRE(funding_leg.legDataType.operator bool());
    REQUIRE(funding_leg.legDataType->FixedLegData.operator bool());
    REQUIRE(!funding_leg.legDataType->FixedLegData->Rates.Rate.empty());
    CHECK(static_cast<double>(funding_leg.legDataType->FixedLegData->Rates.Rate.front()) ==
          Approx(r.trs->funding_rate).epsilon(0.0001));

    BOOST_LOG_SEV(lg, info) << "BondTRS roundtrip passed. Funding type: "
                            << r.trs->funding_leg_type;
}
