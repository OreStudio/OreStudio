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
#include "ores.ore/domain/fx_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include <catch2/catch_approx.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

using Catch::Approx;

/**
 * @file xml_fx_mapper_roundtrip_tests.cpp
 * @brief Thing 3: Mapper fidelity tests for FxForward/FxSwap/FxOption.
 *
 * For each example trade:
 *   1. Parse ORE XML using ores.ore XSD types.
 *   2. Forward-map to ORES typed domain object (via fx_instrument_variant).
 *   3. Verify key fields were captured correctly.
 *   4. Reverse-map back to ORE XSD types.
 *   5. Verify the reconstructed ORE type has the fields we mapped.
 */

namespace {

const std::string_view test_suite("ores.ore.fx.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][fx]");

using ores::ore::domain::portfolio;
using ores::ore::domain::fx_instrument_mapper;
using ores::trading::domain::fx_forward_instrument;
using ores::trading::domain::fx_vanilla_option_instrument;
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
// FxForward mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_fx_forward_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("FX_Forward.xml");

    const auto result = fx_instrument_mapper::forward_fx_forward(t);
    const auto& instr = std::get<fx_forward_instrument>(result.instrument);

    CHECK(instr.trade_type_code == "FxForward");
    CHECK(!instr.value_date.empty());
    CHECK(instr.value_date == "2033-02-20");
    CHECK(instr.bought_currency == "EUR");
    CHECK(instr.bought_amount == Approx(1000000.0).epsilon(0.001));
    CHECK(instr.sold_currency == "USD");
    CHECK(instr.sold_amount == Approx(1100000.0).epsilon(0.001));
    CHECK(instr.settlement == "Cash");
    BOOST_LOG_SEV(lg, info) << "FxForward forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_fx_forward_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("FX_Forward.xml");
    const auto result = fx_instrument_mapper::forward_fx_forward(t);
    const auto& instr = std::get<fx_forward_instrument>(result.instrument);

    const auto reconstructed = fx_instrument_mapper::reverse_fx_forward(instr);

    REQUIRE(reconstructed.FxForwardData.operator bool());
    const auto& fwd = *reconstructed.FxForwardData;
    CHECK(std::string(fwd.ValueDate) == "2033-02-20");
    CHECK(ores::ore::domain::to_string(fwd.BoughtCurrency) == "EUR");
    CHECK(static_cast<float>(fwd.BoughtAmount) == Approx(1000000.0f).epsilon(0.001f));
    CHECK(ores::ore::domain::to_string(fwd.SoldCurrency) == "USD");
    CHECK(static_cast<float>(fwd.SoldAmount) == Approx(1100000.0f).epsilon(0.001f));
    BOOST_LOG_SEV(lg, info) << "FxForward reverse-mapper test passed";
}

// =============================================================================
// FxSwap mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_fx_swap_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("FX_Swap.xml");

    const auto result = fx_instrument_mapper::forward_fx_swap(t);
    const auto& instr = std::get<fx_forward_instrument>(result.instrument);

    CHECK(instr.trade_type_code == "FxSwap");
    CHECK(!instr.value_date.empty());
    CHECK(instr.value_date == "2025-08-23");
    CHECK(instr.bought_currency == "EUR");
    CHECK(instr.bought_amount == Approx(1000000.0).epsilon(0.001));
    CHECK(instr.sold_currency == "USD");
    CHECK(instr.sold_amount == Approx(1100000.0).epsilon(0.001));
    CHECK(instr.settlement == "Cash");
    BOOST_LOG_SEV(lg, info) << "FxSwap forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_fx_swap_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("FX_Swap.xml");
    const auto result = fx_instrument_mapper::forward_fx_swap(t);
    const auto& instr = std::get<fx_forward_instrument>(result.instrument);

    const auto reconstructed = fx_instrument_mapper::reverse_fx_swap(instr);

    REQUIRE(reconstructed.FxSwapData.operator bool());
    const auto& sw = *reconstructed.FxSwapData;
    CHECK(std::string(sw.NearDate) == "2025-08-23");
    CHECK(ores::ore::domain::to_string(sw.NearBoughtCurrency) == "EUR");
    CHECK(ores::ore::domain::to_string(sw.NearSoldCurrency) == "USD");
    BOOST_LOG_SEV(lg, info) << "FxSwap reverse-mapper test passed";
}

// =============================================================================
// FxOption mapper tests
// =============================================================================

TEST_CASE("mapper_roundtrip_fx_option_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("FX_Option_European.xml");

    const auto result = fx_instrument_mapper::forward_fx_option(t);
    const auto& instr = std::get<fx_vanilla_option_instrument>(result.instrument);

    CHECK(instr.trade_type_code == "FxOption");
    CHECK(instr.bought_currency == "EUR");
    CHECK(instr.bought_amount == Approx(1000000.0).epsilon(0.001));
    CHECK(instr.sold_currency == "USD");
    CHECK(instr.sold_amount == Approx(1100000.0).epsilon(0.001));
    CHECK(instr.option_type == "Call");
    CHECK(instr.expiry_date == "2033-02-20");
    BOOST_LOG_SEV(lg, info) << "FxOption forward-mapper test passed";
}

TEST_CASE("mapper_roundtrip_fx_option_reverse", tags) {
    auto lg(make_logger(test_suite));
    const auto t = load_first_trade("FX_Option_European.xml");
    const auto result = fx_instrument_mapper::forward_fx_option(t);
    const auto& instr = std::get<fx_vanilla_option_instrument>(result.instrument);

    const auto reconstructed = fx_instrument_mapper::reverse_fx_option(instr);

    REQUIRE(reconstructed.FxOptionData.operator bool());
    const auto& opt = *reconstructed.FxOptionData;
    CHECK(ores::ore::domain::to_string(opt.BoughtCurrency) == "EUR");
    CHECK(ores::ore::domain::to_string(opt.SoldCurrency) == "USD");
    REQUIRE(opt.OptionData.OptionType.operator bool());
    CHECK(std::string(*opt.OptionData.OptionType) == "Call");
    REQUIRE(opt.OptionData.exerciseDatesGroup.operator bool());
    REQUIRE(opt.OptionData.exerciseDatesGroup->ExerciseDates.operator bool());
    REQUIRE(!opt.OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate.empty());
    CHECK(std::string(
        opt.OptionData.exerciseDatesGroup->ExerciseDates->ExerciseDate.front())
        == "2033-02-20");
    BOOST_LOG_SEV(lg, info) << "FxOption reverse-mapper test passed";
}
