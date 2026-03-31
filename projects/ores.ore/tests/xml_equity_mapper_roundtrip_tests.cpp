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
#include "ores.ore/domain/equity_instrument_mapper.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.testing/project_root.hpp"

/**
 * @file xml_equity_mapper_roundtrip_tests.cpp
 * @brief Thing 3: mapper fidelity tests for equity instrument types.
 *
 * Phases 4 and 5: vanilla and exotic equity types.
 * For each example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map to equity_instrument via trade_mapper.
 *   3. Assert key economic fields are populated.
 *   4. Reverse-map back to ORE XSD trade.
 *   5. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite("ores.ore.equity.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][equity]");

using ores::ore::domain::portfolio;
using ores::ore::domain::equity_instrument_mapper;
using ores::ore::domain::equity_mapping_result;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve(
        "external/ore/examples/Products/Example_Trades/" + filename);
}

equity_mapping_result load_and_map(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_equity_instrument(
        p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

} // namespace

// ---------------------------------------------------------------------------
// Phase 4 — vanilla equity
// ---------------------------------------------------------------------------

TEST_CASE("equity_mapper_roundtrip_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Option_European.xml");

    CHECK(r.instrument.trade_type_code == "EquityOption");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.quantity > 0.0);
    CHECK(!r.instrument.option_type.empty());
    CHECK(!r.instrument.maturity_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_option(
        r.instrument);
    REQUIRE(rt.EquityOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityOption roundtrip passed. Underlying: "
                            << r.instrument.underlying_code;
}

TEST_CASE("equity_mapper_roundtrip_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Forward.xml");

    CHECK(r.instrument.trade_type_code == "EquityForward");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.quantity > 0.0);
    CHECK(!r.instrument.maturity_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_forward(
        r.instrument);
    REQUIRE(rt.EquityForwardData);

    BOOST_LOG_SEV(lg, info) << "EquityForward roundtrip passed. Maturity: "
                            << r.instrument.maturity_date;
}

TEST_CASE("equity_mapper_roundtrip_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Swap.xml");

    CHECK(r.instrument.trade_type_code == "EquitySwap");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(!r.instrument.currency.empty());
    CHECK(!r.instrument.return_type.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_swap(r.instrument);
    REQUIRE(rt.EquitySwapData);
    const bool has_legs = !rt.EquitySwapData->LegData.empty();
    CHECK(has_legs);

    BOOST_LOG_SEV(lg, info) << "EquitySwap roundtrip passed. Return type: "
                            << r.instrument.return_type;
}

TEST_CASE("equity_mapper_roundtrip_variance_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Variance_Swap.xml");

    CHECK(r.instrument.trade_type_code == "EquityVarianceSwap");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(r.instrument.variance_strike > 0.0);
    CHECK(!r.instrument.start_date.empty());
    CHECK(!r.instrument.maturity_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_variance_swap(
        r.instrument);
    REQUIRE(rt.EquityVarianceSwapData);

    BOOST_LOG_SEV(lg, info) << "EquityVarianceSwap roundtrip passed. Strike: "
                            << r.instrument.variance_strike;
}

TEST_CASE("equity_mapper_roundtrip_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Barrier_Option.xml");

    CHECK(r.instrument.trade_type_code == "EquityBarrierOption");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(!r.instrument.barrier_type.empty());
    CHECK(r.instrument.lower_barrier > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_barrier_option(
        r.instrument);
    REQUIRE(rt.EquityBarrierOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityBarrierOption roundtrip passed. Barrier: "
                            << r.instrument.barrier_type;
}

TEST_CASE("equity_mapper_roundtrip_asian_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Asian_Option.xml");

    CHECK(r.instrument.trade_type_code == "EquityAsianOption");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(r.instrument.strike_price > 0.0);
    CHECK(!r.instrument.averaging_start_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_asian_option(
        r.instrument);
    REQUIRE(rt.EquityAsianOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityAsianOption roundtrip passed. Averaging from: "
                            << r.instrument.averaging_start_date;
}

TEST_CASE("equity_mapper_roundtrip_digital_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Digital_Option.xml");

    CHECK(r.instrument.trade_type_code == "EquityDigitalOption");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(r.instrument.strike_price > 0.0);
    CHECK(!r.instrument.option_type.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_digital_option(
        r.instrument);
    REQUIRE(rt.EquityDigitalOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityDigitalOption roundtrip passed. Strike: "
                            << r.instrument.strike_price;
}

TEST_CASE("equity_mapper_roundtrip_touch_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_OneTouch_Option.xml");

    CHECK(r.instrument.trade_type_code == "EquityTouchOption");
    CHECK(!r.instrument.barrier_type.empty());
    CHECK(r.instrument.lower_barrier > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_touch_option(
        r.instrument);
    REQUIRE(rt.EquityTouchOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityTouchOption roundtrip passed. Barrier: "
                            << r.instrument.lower_barrier;
}

TEST_CASE("equity_mapper_roundtrip_outperformance_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_OutperformanceOption.xml");

    CHECK(r.instrument.trade_type_code == "EquityOutperformanceOption");
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.notional > 0.0);
    CHECK(!r.instrument.basket_json.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_outperformance_option(
        r.instrument);
    REQUIRE(rt.EquityOutperformanceOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityOutperformanceOption roundtrip passed. Basket: "
                            << r.instrument.basket_json;
}

// ---------------------------------------------------------------------------
// Phase 5 — exotic equity
// ---------------------------------------------------------------------------

TEST_CASE("equity_mapper_roundtrip_accumulator", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityAccumulator_single_name.xml");

    CHECK(r.instrument.trade_type_code == "EquityAccumulator");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(r.instrument.accumulation_amount > 0.0);
    CHECK(r.instrument.knock_out_barrier > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_accumulator(
        r.instrument);
    REQUIRE(rt.EquityAccumulatorData);

    BOOST_LOG_SEV(lg, info) << "EquityAccumulator roundtrip passed. Amount: "
                            << r.instrument.accumulation_amount;
}

TEST_CASE("equity_mapper_roundtrip_tarf", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityTaRF.xml");

    CHECK(r.instrument.trade_type_code == "EquityTaRF");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(r.instrument.accumulation_amount > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_tarf(r.instrument);
    REQUIRE(rt.EquityTaRFData);

    BOOST_LOG_SEV(lg, info) << "EquityTaRF roundtrip passed. Amount: "
                            << r.instrument.accumulation_amount;
}

TEST_CASE("equity_mapper_roundtrip_cliquet_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_Equity_Cliquet_Option.xml");

    CHECK(r.instrument.trade_type_code == "EquityCliquetOption");
    CHECK(!r.instrument.underlying_code.empty());
    CHECK(r.instrument.notional > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_cliquet_option(
        r.instrument);
    REQUIRE(rt.EquityCliquetOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityCliquetOption roundtrip passed.";
}

TEST_CASE("equity_mapper_roundtrip_worst_of_basket_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityWorstOfBasketSwap.xml");

    CHECK(r.instrument.trade_type_code == "EquityWorstOfBasketSwap");
    CHECK(!r.instrument.currency.empty());
    CHECK(r.instrument.quantity > 0.0);
    CHECK(!r.instrument.basket_json.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_worst_of_basket_swap(
        r.instrument);
    REQUIRE(rt.EquityWorstOfBasketSwapData);

    BOOST_LOG_SEV(lg, info) << "EquityWorstOfBasketSwap roundtrip passed. Basket: "
                            << r.instrument.basket_json;
}
