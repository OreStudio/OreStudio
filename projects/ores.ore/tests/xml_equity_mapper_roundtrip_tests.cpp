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

// Transitional helper: extracts the legacy flat equity_instrument from the
// mapping-result variant. Deleted in the final Phase 2 commit once every
// test has migrated to per-type assertions via std::get<per_type>.
const ores::trading::domain::equity_instrument& flat(
    const equity_mapping_result& r) {
    return std::get<ores::trading::domain::equity_instrument>(r.instrument);
}

} // namespace

// ---------------------------------------------------------------------------
// Phase 4 — vanilla equity
// ---------------------------------------------------------------------------

TEST_CASE("equity_mapper_roundtrip_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Option_European.xml");

    CHECK(flat(r).trade_type_code == "EquityOption");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(!flat(r).currency.empty());
    CHECK(flat(r).quantity > 0.0);
    CHECK(!flat(r).option_type.empty());
    CHECK(!flat(r).maturity_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_option(flat(r));
    REQUIRE(rt.EquityOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityOption roundtrip passed. Underlying: "
                            << flat(r).underlying_code;
}

TEST_CASE("equity_mapper_roundtrip_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Forward.xml");

    CHECK(flat(r).trade_type_code == "EquityForward");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(!flat(r).currency.empty());
    CHECK(flat(r).quantity > 0.0);
    CHECK(!flat(r).maturity_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_forward(flat(r));
    REQUIRE(rt.EquityForwardData);

    BOOST_LOG_SEV(lg, info) << "EquityForward roundtrip passed. Maturity: "
                            << flat(r).maturity_date;
}

TEST_CASE("equity_mapper_roundtrip_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Swap.xml");

    CHECK(flat(r).trade_type_code == "EquitySwap");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(!flat(r).currency.empty());
    CHECK(!flat(r).return_type.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_swap(flat(r));
    REQUIRE(rt.EquitySwapData);
    const bool has_legs = !rt.EquitySwapData->LegData.empty();
    CHECK(has_legs);

    BOOST_LOG_SEV(lg, info) << "EquitySwap roundtrip passed. Return type: "
                            << flat(r).return_type;
}

TEST_CASE("equity_mapper_roundtrip_variance_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Variance_Swap.xml");

    CHECK(flat(r).trade_type_code == "EquityVarianceSwap");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(flat(r).variance_strike > 0.0);
    CHECK(!flat(r).start_date.empty());
    CHECK(!flat(r).maturity_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_variance_swap(flat(r));
    REQUIRE(rt.EquityVarianceSwapData);

    BOOST_LOG_SEV(lg, info) << "EquityVarianceSwap roundtrip passed. Strike: "
                            << flat(r).variance_strike;
}

TEST_CASE("equity_mapper_roundtrip_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Barrier_Option.xml");

    CHECK(flat(r).trade_type_code == "EquityBarrierOption");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(!flat(r).barrier_type.empty());
    CHECK(flat(r).lower_barrier > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_barrier_option(flat(r));
    REQUIRE(rt.EquityBarrierOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityBarrierOption roundtrip passed. Barrier: "
                            << flat(r).barrier_type;
}

TEST_CASE("equity_mapper_roundtrip_asian_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Asian_Option.xml");

    CHECK(flat(r).trade_type_code == "EquityAsianOption");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(flat(r).strike_price > 0.0);
    CHECK(!flat(r).averaging_start_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_asian_option(flat(r));
    REQUIRE(rt.EquityAsianOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityAsianOption roundtrip passed. Averaging from: "
                            << flat(r).averaging_start_date;
}

TEST_CASE("equity_mapper_roundtrip_digital_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Digital_Option.xml");

    CHECK(flat(r).trade_type_code == "EquityDigitalOption");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(flat(r).strike_price > 0.0);
    CHECK(!flat(r).option_type.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_digital_option(flat(r));
    REQUIRE(rt.EquityDigitalOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityDigitalOption roundtrip passed. Strike: "
                            << flat(r).strike_price;
}

TEST_CASE("equity_mapper_roundtrip_touch_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_OneTouch_Option.xml");

    CHECK(flat(r).trade_type_code == "EquityTouchOption");
    CHECK(!flat(r).barrier_type.empty());
    CHECK(flat(r).lower_barrier > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_touch_option(flat(r));
    REQUIRE(rt.EquityTouchOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityTouchOption roundtrip passed. Barrier: "
                            << flat(r).lower_barrier;
}

TEST_CASE("equity_mapper_roundtrip_outperformance_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_OutperformanceOption.xml");

    CHECK(flat(r).trade_type_code == "EquityOutperformanceOption");
    CHECK(!flat(r).currency.empty());
    CHECK(flat(r).notional > 0.0);
    CHECK(!flat(r).basket_json.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_outperformance_option(flat(r));
    REQUIRE(rt.EquityOutperformanceOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityOutperformanceOption roundtrip passed. Basket: "
                            << flat(r).basket_json;
}

// ---------------------------------------------------------------------------
// Phase 5 — exotic equity
// ---------------------------------------------------------------------------

TEST_CASE("equity_mapper_roundtrip_accumulator", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityAccumulator_single_name.xml");

    CHECK(flat(r).trade_type_code == "EquityAccumulator");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(flat(r).accumulation_amount > 0.0);
    CHECK(flat(r).knock_out_barrier > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_accumulator(flat(r));
    REQUIRE(rt.EquityAccumulatorData);

    BOOST_LOG_SEV(lg, info) << "EquityAccumulator roundtrip passed. Amount: "
                            << flat(r).accumulation_amount;
}

TEST_CASE("equity_mapper_roundtrip_tarf", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityTaRF.xml");

    CHECK(flat(r).trade_type_code == "EquityTaRF");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(flat(r).accumulation_amount > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_tarf(flat(r));
    REQUIRE(rt.EquityTaRFData);

    BOOST_LOG_SEV(lg, info) << "EquityTaRF roundtrip passed. Amount: "
                            << flat(r).accumulation_amount;
}

TEST_CASE("equity_mapper_roundtrip_cliquet_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_Equity_Cliquet_Option.xml");

    CHECK(flat(r).trade_type_code == "EquityCliquetOption");
    CHECK(!flat(r).underlying_code.empty());
    CHECK(flat(r).notional > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_cliquet_option(flat(r));
    REQUIRE(rt.EquityCliquetOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityCliquetOption roundtrip passed.";
}

TEST_CASE("equity_mapper_roundtrip_worst_of_basket_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityWorstOfBasketSwap.xml");

    CHECK(flat(r).trade_type_code == "EquityWorstOfBasketSwap");
    CHECK(!flat(r).currency.empty());
    CHECK(flat(r).quantity > 0.0);
    CHECK(!flat(r).basket_json.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_worst_of_basket_swap(flat(r));
    REQUIRE(rt.EquityWorstOfBasketSwapData);

    BOOST_LOG_SEV(lg, info) << "EquityWorstOfBasketSwap roundtrip passed. Basket: "
                            << flat(r).basket_json;
}
