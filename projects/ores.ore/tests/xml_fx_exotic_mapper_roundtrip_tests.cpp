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
#include "ores.ore/domain/fx_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_fx_exotic_mapper_roundtrip_tests.cpp
 * @brief Thing 3: mapper fidelity tests for FX exotic instrument types (Phase 6).
 *
 * For each example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map to the typed fx_instrument_variant via trade_mapper.
 *   3. Extract the concrete typed struct from the variant.
 *   4. Assert key economic fields are populated.
 *   5. Reverse-map back to ORE XSD trade.
 *   6. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite("ores.ore.fx.exotic.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][fx][exotic]");

using ores::ore::domain::portfolio;
using ores::ore::domain::fx_instrument_mapper;
using ores::ore::domain::fx_mapping_result;
using ores::trading::domain::fx_barrier_option_instrument;
using ores::trading::domain::fx_digital_option_instrument;
using ores::trading::domain::fx_variance_swap_instrument;
using ores::trading::domain::fx_asian_forward_instrument;
using ores::trading::domain::fx_accumulator_instrument;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

fx_mapping_result load_and_map(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_fx_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

} // namespace

TEST_CASE("fx_exotic_mapper_roundtrip_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("FX_Barrier_Option.xml");
    const auto& instr = std::get<fx_barrier_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxBarrierOption");
    CHECK(!instr.bought_currency.empty());
    CHECK(!instr.sold_currency.empty());
    CHECK(instr.bought_amount > 0.0);
    CHECK(!instr.barrier_type.empty());
    CHECK(instr.lower_barrier > 0.0);

    const auto rt = fx_instrument_mapper::reverse_fx_barrier_option(instr);
    const bool has_data = rt.FxBarrierOptionData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxBarrierOption roundtrip passed. Barrier: "
                            << instr.barrier_type;
}

TEST_CASE("fx_exotic_mapper_roundtrip_digital_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("FX_Digital_Option.xml");
    const auto& instr = std::get<fx_digital_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxDigitalOption");
    CHECK(!instr.foreign_currency.empty());
    CHECK(!instr.domestic_currency.empty());
    REQUIRE(instr.strike.has_value());
    CHECK(*instr.strike > 0.0);
    CHECK(instr.payoff_amount > 0.0);
    CHECK(!instr.option_type.empty());

    const auto rt = fx_instrument_mapper::reverse_fx_digital_option(instr);
    const bool has_data = rt.FxDigitalOptionData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxDigitalOption roundtrip passed. Strike: "
                            << *instr.strike;
}

TEST_CASE("fx_exotic_mapper_roundtrip_digital_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("FX_Digital_Barrier_Option.xml");
    const auto& instr = std::get<fx_digital_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxDigitalBarrierOption");
    CHECK(!instr.foreign_currency.empty());
    CHECK(!instr.domestic_currency.empty());
    REQUIRE(instr.strike.has_value());
    CHECK(*instr.strike > 0.0);
    CHECK(!instr.barrier_type.empty());
    REQUIRE(instr.lower_barrier.has_value());
    CHECK(*instr.lower_barrier > 0.0);

    const auto rt = fx_instrument_mapper::reverse_fx_digital_barrier_option(instr);
    const bool has_data = rt.FxDigitalBarrierOptionData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxDigitalBarrierOption roundtrip passed. Barrier: "
                            << instr.barrier_type;
}

TEST_CASE("fx_exotic_mapper_roundtrip_touch_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("FX_OneTouch_option.xml");
    const auto& instr = std::get<fx_digital_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxTouchOption");
    CHECK(!instr.foreign_currency.empty());
    CHECK(instr.payoff_amount > 0.0);
    CHECK(!instr.barrier_type.empty());
    REQUIRE(instr.lower_barrier.has_value());
    CHECK(*instr.lower_barrier > 0.0);

    const auto rt = fx_instrument_mapper::reverse_fx_touch_option(instr);
    const bool has_data = rt.FxTouchOptionData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxTouchOption roundtrip passed. Barrier: "
                            << *instr.lower_barrier;
}

TEST_CASE("fx_exotic_mapper_roundtrip_double_touch_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("FX_DoubleTouch_Option.xml");
    const auto& instr = std::get<fx_digital_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxDoubleTouchOption");
    CHECK(!instr.foreign_currency.empty());
    CHECK(instr.payoff_amount > 0.0);
    CHECK(!instr.barrier_type.empty());

    const auto rt = fx_instrument_mapper::reverse_fx_touch_option(instr);
    const bool has_data = rt.FxDoubleTouchOptionData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxDoubleTouchOption roundtrip passed.";
}

TEST_CASE("fx_exotic_mapper_roundtrip_variance_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("FX_Variance_Swap.xml");
    const auto& instr = std::get<fx_variance_swap_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxVarianceSwap");
    CHECK(!instr.underlying_code.empty());
    CHECK(instr.strike > 0.0);
    CHECK(instr.notional > 0.0);
    CHECK(!instr.start_date.empty());
    CHECK(!instr.end_date.empty());

    const auto rt = fx_instrument_mapper::reverse_fx_variance_swap(instr);
    const bool has_data = rt.FxVarianceSwapData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxVarianceSwap roundtrip passed. Strike: "
                            << instr.strike;
}

TEST_CASE("fx_exotic_mapper_roundtrip_average_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("FX_Average_Forward.xml");
    const auto& instr = std::get<fx_asian_forward_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxAverageForward");
    CHECK(!instr.payment_date.empty());
    CHECK(!instr.reference_currency.empty());
    CHECK(!instr.settlement_currency.empty());
    CHECK(!instr.fx_index.empty());

    const auto rt = fx_instrument_mapper::reverse_fx_average_forward(instr);
    const bool has_data = rt.FxAverageForwardData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxAverageForward roundtrip passed. FX index: "
                            << instr.fx_index;
}

TEST_CASE("fx_exotic_mapper_roundtrip_accumulator", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_FxAccumulator.xml");
    const auto& instr = std::get<fx_accumulator_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxAccumulator");
    CHECK(!instr.underlying_code.empty());
    CHECK(!instr.currency.empty());
    CHECK(instr.fixing_amount > 0.0);

    const auto rt = fx_instrument_mapper::reverse_fx_accumulator(instr);
    const bool has_data = rt.FxAccumulatorData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxAccumulator roundtrip passed. Amount: "
                            << instr.fixing_amount;
}

TEST_CASE("fx_exotic_mapper_roundtrip_tarf", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_FxTaRF.xml");
    const auto& instr = std::get<fx_asian_forward_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxTaRF");
    CHECK(!instr.fx_index.empty());
    CHECK(!instr.currency.empty());
    REQUIRE(instr.fixing_amount.has_value());
    CHECK(*instr.fixing_amount > 0.0);

    const auto rt = fx_instrument_mapper::reverse_fx_tarf(instr);
    const bool has_data = rt.FxTaRFData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxTaRF roundtrip passed. Amount: "
                            << *instr.fixing_amount;
}

TEST_CASE("fx_exotic_mapper_roundtrip_generic_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_FxGenericBarrierOption.xml");
    const auto& instr = std::get<fx_barrier_option_instrument>(r.instrument);

    CHECK(instr.trade_type_code == "FxGenericBarrierOption");
    CHECK(!instr.underlying_code.empty());
    CHECK(!instr.bought_currency.empty());

    const auto rt = fx_instrument_mapper::reverse_fx_generic_barrier_option(instr);
    const bool has_data = rt.FxGenericBarrierOptionData.operator bool();
    REQUIRE(has_data);

    BOOST_LOG_SEV(lg, info) << "FxGenericBarrierOption roundtrip passed. Underlying: "
                            << instr.underlying_code;
}
