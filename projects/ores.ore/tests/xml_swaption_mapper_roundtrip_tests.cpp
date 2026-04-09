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
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

using Catch::Approx;

/**
 * @file xml_swaption_mapper_roundtrip_tests.cpp
 * @brief Thing 3: Mapper fidelity tests for Swaption and CallableSwap.
 */

namespace {

const std::string_view test_suite("ores.ore.swaption.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][swaption]");

using ores::ore::domain::portfolio;
using ores::ore::domain::swap_instrument_mapper;
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
// Swaption (European) mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_swaption_european_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Swaption_European.xml", 0);

    const auto result = swap_instrument_mapper::forward_swaption(t);
    const auto& instr =
        std::get<ores::trading::domain::swaption_instrument>(result.instrument);

    CHECK(instr.exercise_type == "European");
    CHECK(instr.expiry_date == "2033-02-20");   // first exercise date
    CHECK(instr.maturity_date == "2043-02-21");
    REQUIRE(result.legs.size() == 2u);
    // leg 0: floating (EUR-EURIBOR-3M)
    CHECK(result.legs[0].leg_type_code == "Floating");
    CHECK(result.legs[0].currency == "EUR");
    CHECK(result.legs[0].floating_index_code == "EUR-EURIBOR-3M");
    // leg 1: fixed (2%)
    CHECK(result.legs[1].leg_type_code == "Fixed");
    CHECK(result.legs[1].fixed_rate == Approx(0.02).epsilon(0.0001));
    BOOST_LOG_SEV(lg, info) << "Swaption European forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_swaption_european_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Swaption_European.xml", 0);
    const auto result = swap_instrument_mapper::forward_swaption(t);

    const auto reconstructed =
        swap_instrument_mapper::reverse_swaption(
            std::get<ores::trading::domain::swaption_instrument>(
                result.instrument),
            result.legs);

    REQUIRE(reconstructed.SwaptionData.operator bool());
    const auto& sd = *reconstructed.SwaptionData;

    // OptionData: style and exercise date round-trip
    REQUIRE(sd.OptionData.operator bool());
    CHECK(std::string(*sd.OptionData->Style) == "European");
    REQUIRE(sd.OptionData->exerciseDatesGroup.operator bool());
    REQUIRE(sd.OptionData->exerciseDatesGroup->ExerciseDates.operator bool());
    REQUIRE(!sd.OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate.empty());
    CHECK(std::string(
        sd.OptionData->exerciseDatesGroup->ExerciseDates->ExerciseDate[0])
        == "2033-02-20");

    // Two legs reconstructed
    REQUIRE(sd.LegData.size() == 2u);
    CHECK(std::string(*sd.LegData[0].Currency) == "EUR");
    CHECK(std::string(*sd.LegData[1].Currency) == "EUR");
    BOOST_LOG_SEV(lg, info) << "Swaption European reverse-mapper test passed";
}

// =============================================================================
// Swaption (Bermudan) mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_swaption_bermudan_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Swaption_Bermudan.xml", 0);

    const auto result = swap_instrument_mapper::forward_swaption(t);
    const auto& instr =
        std::get<ores::trading::domain::swaption_instrument>(result.instrument);

    CHECK(instr.exercise_type == "Bermudan");
    // First exercise date from the 6 listed
    CHECK(instr.expiry_date == "2035-09-23");
    REQUIRE(result.legs.size() == 2u);
    BOOST_LOG_SEV(lg, info) << "Swaption Bermudan forward-mapper test passed";
}

// =============================================================================
// CallableSwap mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_callable_swap_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Callable_Swap_Bermudan.xml", 0);

    const auto result = swap_instrument_mapper::forward_callable_swap(t);
    const auto& instr =
        std::get<ores::trading::domain::callable_swap_instrument>(
            result.instrument);

    // Exercise dates captured as JSON array
    CHECK(!instr.call_dates_json.empty());
    CHECK(instr.call_dates_json.front() == '[');
    CHECK(instr.call_dates_json.back() == ']');
    CHECK(!result.legs.empty());
    BOOST_LOG_SEV(lg, info) << "CallableSwap forward-mapper test passed, "
                            << " dates=" << instr.call_dates_json;
}

TEST_CASE("mapper_roundtrip_callable_swap_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_trade("IR_Callable_Swap_Bermudan.xml", 0);
    const auto result = swap_instrument_mapper::forward_callable_swap(t);

    const auto reconstructed =
        swap_instrument_mapper::reverse_callable_swap(
            std::get<ores::trading::domain::callable_swap_instrument>(
                result.instrument),
            result.legs);

    REQUIRE(reconstructed.CallableSwapData.operator bool());
    const auto& cd = *reconstructed.CallableSwapData;

    // OptionData present (callable dates were non-empty)
    REQUIRE(cd.OptionData.operator bool());

    // Legs round-trip
    CHECK(!cd.LegData.empty());
    CHECK(cd.LegData.size() == result.legs.size());
    BOOST_LOG_SEV(lg, info) << "CallableSwap reverse-mapper test passed";
}
