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
#include "ores.ore/domain/swap_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>

using Catch::Approx;
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_ir_mapper_roundtrip_tests.cpp
 * @brief Thing 3: Mapper fidelity tests for IR/CCS/FRA/CapFloor.
 *
 * For each example trade:
 *   1. Parse ORE XML using ores.ore XSD types.
 *   2. Forward-map to ORES domain types (instrument + swap_legs).
 *   3. Verify key fields were captured correctly.
 *   4. Reverse-map back to ORE XSD types.
 *   5. Verify the reconstructed ORE type has the fields we mapped.
 *
 * These tests check field-level fidelity for the fields the ORES domain model
 * currently captures. They do NOT do full-text XML comparison — that is the
 * job of Thing 1 (golden roundtrip tests). The gaps between what these tests
 * cover and the full ORE XML schema are reported by the Python coverage check.
 */

namespace {

const std::string_view test_suite("ores.ore.ir.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][ir]");

using ores::ore::domain::portfolio;
using ores::ore::domain::swap_instrument_mapper;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

ores::ore::domain::trade load_first_trade(const std::string& filename) {
    using ores::platform::filesystem::file;
    const auto path = example_path(filename);
    const std::string content = file::read_content(path);
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    return p.Trade.front();
}

} // namespace

// =============================================================================
// Vanilla Swap mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_swap_vanilla_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Swap_Vanilla.xml");

    const auto result = swap_instrument_mapper::forward_swap(t);
    const auto& instr =
        std::get<ores::trading::domain::vanilla_swap_instrument>(
            result.instrument);
    const auto& legs = result.legs;

    CHECK(instr.start_date == "2023-02-21");
    CHECK(instr.maturity_date == "2043-02-21");

    REQUIRE(legs.size() == 2);
    CHECK(legs[0].leg_type_code == "Fixed");
    CHECK(legs[0].currency == "EUR");
    CHECK(legs[0].notional == Approx(10000000.0).epsilon(0.001));
    CHECK(legs[0].fixed_rate == Approx(0.021).epsilon(0.0001));

    CHECK(legs[1].leg_type_code == "Floating");
    CHECK(legs[1].currency == "EUR");
    CHECK(legs[1].floating_index_code == "EUR-EURIBOR-6M");
    CHECK(legs[1].notional == Approx(10000000.0).epsilon(0.001));
    BOOST_LOG_SEV(lg, info) << "Swap forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_swap_vanilla_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Swap_Vanilla.xml");
    const auto result = swap_instrument_mapper::forward_swap(t);

    const auto reconstructed = swap_instrument_mapper::reverse_swap(
        std::get<ores::trading::domain::vanilla_swap_instrument>(
            result.instrument),
        result.legs);

    REQUIRE(reconstructed.SwapData.operator bool());
    CHECK(reconstructed.SwapData->LegData.size() == 2);

    const auto& leg0 = reconstructed.SwapData->LegData[0];
    CHECK(leg0.LegType == ores::ore::domain::legType::Fixed);
    REQUIRE(leg0.ScheduleData);
    CHECK(!leg0.ScheduleData->Rules.empty());
    CHECK(std::string(leg0.ScheduleData->Rules[0].StartDate) == "2023-02-21");

    BOOST_LOG_SEV(lg, info) << "Swap reverse-mapper test passed";
}

// =============================================================================
// FRA mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_fra_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_FRA.xml");

    const auto result = swap_instrument_mapper::forward_fra(t);
    const auto& instr =
        std::get<ores::trading::domain::fra_instrument>(result.instrument);
    const auto& legs = result.legs;

    CHECK(instr.start_date == "2026-10-19");
    CHECK(instr.end_date == "2027-04-20");
    CHECK(instr.currency == "EUR");
    CHECK(instr.notional == Approx(100000000.0).epsilon(0.001));

    REQUIRE(legs.size() == 1);
    CHECK(legs[0].floating_index_code == "EUR-EURIBOR-6M");
    CHECK(legs[0].fixed_rate == Approx(0.005).epsilon(0.00001));
    CHECK(legs[0].currency == "EUR");
    BOOST_LOG_SEV(lg, info) << "FRA forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_fra_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_FRA.xml");
    const auto result = swap_instrument_mapper::forward_fra(t);

    const auto reconstructed = swap_instrument_mapper::reverse_fra(
        std::get<ores::trading::domain::fra_instrument>(result.instrument),
        result.legs);

    REQUIRE(reconstructed.ForwardRateAgreementData.operator bool());
    const auto& fra = *reconstructed.ForwardRateAgreementData;
    CHECK(std::string(fra.StartDate) == "2026-10-19");
    CHECK(std::string(fra.EndDate) == "2027-04-20");
    CHECK(ores::ore::domain::to_string(fra.Currency) == "EUR");
    CHECK(std::string(fra.Index) == "EUR-EURIBOR-6M");
    CHECK(static_cast<float>(fra.Strike) == Approx(0.005f).epsilon(0.00001f));
    BOOST_LOG_SEV(lg, info) << "FRA reverse-mapper test passed";
}

// =============================================================================
// CapFloor mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_capfloor_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Cap_on_IBOR.xml");

    const auto result = swap_instrument_mapper::forward_capfloor(t);
    const auto& instr =
        std::get<ores::trading::domain::cap_floor_instrument>(result.instrument);
    const auto& legs = result.legs;

    CHECK(instr.start_date == "2023-10-11");
    CHECK(instr.maturity_date == "2038-10-10");

    REQUIRE(legs.size() == 1);
    CHECK(legs[0].leg_type_code == "Floating");
    CHECK(legs[0].currency == "EUR");
    CHECK(legs[0].notional == Approx(3000000.0).epsilon(0.001));
    CHECK(legs[0].floating_index_code == "EUR-EURIBOR-6M");
    BOOST_LOG_SEV(lg, info) << "CapFloor forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_capfloor_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IR_Cap_on_IBOR.xml");
    const auto result = swap_instrument_mapper::forward_capfloor(t);

    const auto reconstructed = swap_instrument_mapper::reverse_capfloor(
        std::get<ores::trading::domain::cap_floor_instrument>(result.instrument),
        result.legs);

    REQUIRE(reconstructed.CapFloorData.operator bool());
    const auto& cf = *reconstructed.CapFloorData;
    CHECK(cf.LegData.LegType == ores::ore::domain::legType::Floating);
    CHECK(ores::ore::domain::to_string(cf.LegData.Currency) == "EUR");
    CHECK(cf.LegData.ScheduleData.Rules.size() == 1);
    CHECK(std::string(cf.LegData.ScheduleData.Rules[0].StartDate) == "2023-10-11");
    CHECK(std::string(cf.LegData.legDataType.FloatingLegData->Index) ==
          "EUR-EURIBOR-6M");
    BOOST_LOG_SEV(lg, info) << "CapFloor reverse-mapper test passed";
}

// =============================================================================
// CCS mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_ccs_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("IRFX_Cross_Currency_Swap_rebalancing.xml");

    const auto result = swap_instrument_mapper::forward_swap(t);
    REQUIRE(std::holds_alternative<ores::trading::domain::vanilla_swap_instrument>(
        result.instrument));
    const auto& legs = result.legs;

    REQUIRE(legs.size() >= 2);
    BOOST_LOG_SEV(lg, info) << "CCS forward-mapper test passed, legs: "
                            << legs.size();
}
