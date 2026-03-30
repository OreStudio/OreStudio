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
#include "ores.ore/domain/bond_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

using Catch::Approx;

/**
 * @file xml_bond_mapper_roundtrip_tests.cpp
 * @brief Thing 3: Mapper fidelity tests for Bond/ForwardBond/ConvertibleBond.
 */

namespace {

const std::string_view test_suite("ores.ore.bond.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][bond]");

using ores::ore::domain::portfolio;
using ores::ore::domain::bond_instrument_mapper;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

ores::ore::domain::trade load_trade(
        const std::string& filename, std::size_t index = 0) {
    using ores::platform::filesystem::file;
    const auto path = example_path(filename);
    const std::string content = file::read_content(path);
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(p.Trade.size() > index);
    return p.Trade[index];
}

} // namespace

// =============================================================================
// Bond mapper tests (first trade in Cash_Bonds.xml)
// =============================================================================

TEST_CASE("mapper_roundtrip_bond_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("Cash_Bonds.xml", 0);

    const auto result = bond_instrument_mapper::forward_bond(t);
    const auto& instr = result.instrument;

    CHECK(instr.trade_type_code == "Bond");
    CHECK(instr.issuer == "CPTY_C");
    CHECK(instr.issue_date == "2025-02-03");
    CHECK(instr.currency == "EUR");
    CHECK(instr.face_value == Approx(10000000.0).epsilon(0.001));
    CHECK(instr.coupon_rate == Approx(0.05).epsilon(0.0001));
    CHECK(instr.maturity_date == "2035-02-03");
    CHECK(instr.coupon_frequency_code == "1Y");
    BOOST_LOG_SEV(lg, info) << "Bond forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_bond_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("Cash_Bonds.xml", 0);
    const auto result = bond_instrument_mapper::forward_bond(t);

    const auto reconstructed =
        bond_instrument_mapper::reverse_bond(result.instrument);

    REQUIRE(reconstructed.BondData.operator bool());
    const auto& bd = *reconstructed.BondData;

    // Issuer and issue date
    REQUIRE(bd.IssuerId);
    CHECK(std::string(*bd.IssuerId) == "CPTY_C");
    REQUIRE(bd.IssueDate);
    CHECK(std::string(*bd.IssueDate) == "2025-02-03");

    // Leg: currency and notional
    REQUIRE(!bd.LegData.empty());
    CHECK(std::string(*bd.LegData[0].Currency) == "EUR");
    REQUIRE(bd.LegData[0].Notionals);
    CHECK(static_cast<float>(bd.LegData[0].Notionals->Notional[0])
          == Approx(10000000.0f).epsilon(0.001f));

    // Coupon rate
    REQUIRE(bd.LegData[0].legDataType);
    REQUIRE(bd.LegData[0].legDataType->FixedLegData);
    REQUIRE(!bd.LegData[0].legDataType->FixedLegData->Rates.Rate.empty());
    CHECK(static_cast<float>(bd.LegData[0].legDataType->FixedLegData->Rates.Rate[0])
          == Approx(0.05f).epsilon(0.0001f));

    // Maturity date
    REQUIRE(bd.LegData[0].ScheduleData);
    REQUIRE(!bd.LegData[0].ScheduleData->Rules.empty());
    REQUIRE(bd.LegData[0].ScheduleData->Rules[0].EndDate);
    CHECK(std::string(*bd.LegData[0].ScheduleData->Rules[0].EndDate) == "2035-02-03");

    BOOST_LOG_SEV(lg, info) << "Bond reverse-mapper test passed";
}

// =============================================================================
// ForwardBond mapper tests (second trade in Cash_Bonds.xml)
// =============================================================================

TEST_CASE("mapper_roundtrip_forward_bond_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("Cash_Bonds.xml", 1);

    const auto result = bond_instrument_mapper::forward_forward_bond(t);
    const auto& instr = result.instrument;

    CHECK(instr.trade_type_code == "ForwardBond");
    CHECK(instr.issuer == "CPTY_C");
    CHECK(instr.currency == "EUR");
    CHECK(instr.face_value == Approx(10000000.0).epsilon(0.001));
    BOOST_LOG_SEV(lg, info) << "ForwardBond forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_forward_bond_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("Cash_Bonds.xml", 1);
    const auto result = bond_instrument_mapper::forward_forward_bond(t);

    const auto reconstructed =
        bond_instrument_mapper::reverse_forward_bond(result.instrument);

    REQUIRE(reconstructed.ForwardBondData.operator bool());
    const auto& fbd = *reconstructed.ForwardBondData;
    REQUIRE(!fbd.BondData.LegData.empty());
    CHECK(std::string(*fbd.BondData.LegData[0].Currency) == "EUR");
    BOOST_LOG_SEV(lg, info) << "ForwardBond reverse-mapper test passed";
}

// =============================================================================
// ConvertibleBond mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_convertible_bond_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("Cash_ConvertibleBond.xml", 0);

    const auto result = bond_instrument_mapper::forward_convertible_bond(t);
    const auto& instr = result.instrument;

    CHECK(instr.trade_type_code == "ConvertibleBond");
    BOOST_LOG_SEV(lg, info) << "ConvertibleBond forward-mapper test passed, "
                            << "issuer=" << instr.issuer
                            << " currency=" << instr.currency;
}

TEST_CASE("mapper_roundtrip_convertible_bond_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("Cash_ConvertibleBond.xml", 0);
    const auto result = bond_instrument_mapper::forward_convertible_bond(t);
    const auto& instr = result.instrument;

    const auto reconstructed =
        bond_instrument_mapper::reverse_convertible_bond(result.instrument);

    REQUIRE(reconstructed.ConvertibleBondData.operator bool());
    const auto& cbd = *reconstructed.ConvertibleBondData;

    // Cash_ConvertibleBond.xml has no LegData — verify clean empty round-trip
    CHECK(cbd.BondData.LegData.empty());
    CHECK(!cbd.BondData.IssuerId);
    CHECK(!cbd.BondData.IssueDate);

    // Instrument fields should round-trip consistently with the forward pass
    CHECK(instr.currency.empty());
    CHECK(instr.issuer.empty());
    CHECK(instr.face_value == Approx(0.0).epsilon(0.001));

    BOOST_LOG_SEV(lg, info) << "ConvertibleBond reverse-mapper test passed";
}
