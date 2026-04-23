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

// Transitional helper: returns the flat equity_instrument shape regardless
// of which variant alternative the mapping result carries, so the legacy
// flat-signature reverse_equity_* functions still work while forward_*
// functions migrate one per-type at a time. Deleted in the final Phase 2
// commit.
ores::trading::domain::equity_instrument flat(
    const equity_mapping_result& r) {
    return std::visit([](const auto& inst)
            -> ores::trading::domain::equity_instrument {
        using T = std::decay_t<decltype(inst)>;
        using namespace ores::trading::domain;
        if constexpr (std::is_same_v<T, equity_instrument>) {
            return inst;
        } else if constexpr (std::is_same_v<T, equity_option_instrument>) {
            equity_instrument f;
            f.trade_type_code = inst.trade_type_code;
            f.underlying_code = inst.underlying_name;
            f.currency        = inst.currency;
            f.quantity        = inst.notional;
            f.option_type     = inst.option_type;
            f.exercise_type   = inst.exercise_type;
            f.maturity_date   = inst.expiry_date;
            f.strike_price    = inst.strike;
            return f;
        } else if constexpr (std::is_same_v<T, equity_forward_instrument>) {
            equity_instrument f;
            f.trade_type_code = inst.trade_type_code;
            f.underlying_code = inst.underlying_name;
            f.currency        = inst.currency;
            f.quantity        = inst.quantity;
            f.maturity_date   = inst.expiry_date;
            f.strike_price    = inst.forward_price.value_or(0.0);
            return f;
        } else if constexpr (std::is_same_v<T, equity_swap_instrument>) {
            equity_instrument f;
            f.trade_type_code = inst.trade_type_code;
            f.underlying_code = inst.underlying_name;
            f.currency        = inst.currency;
            f.notional        = inst.notional;
            f.return_type     = inst.return_type;
            f.start_date      = inst.start_date;
            f.maturity_date   = inst.maturity_date;
            return f;
        } else if constexpr (std::is_same_v<T, equity_variance_swap_instrument>) {
            equity_instrument f;
            f.trade_type_code = inst.trade_type_code;
            f.underlying_code = inst.underlying_name;
            f.currency        = inst.currency;
            f.notional        = inst.notional;
            f.variance_strike = inst.variance_strike;
            f.start_date      = inst.start_date;
            f.maturity_date   = inst.maturity_date;
            return f;
        } else if constexpr (std::is_same_v<T,
                equity_barrier_option_instrument>) {
            equity_instrument f;
            f.trade_type_code = inst.trade_type_code;
            f.underlying_code = inst.underlying_name;
            f.currency        = inst.currency;
            f.quantity        = inst.notional;
            f.option_type     = inst.option_type;
            f.exercise_type   = inst.exercise_type;
            f.maturity_date   = inst.expiry_date;
            f.strike_price    = inst.strike;
            f.barrier_type    = inst.lower_barrier_type;
            f.lower_barrier   = inst.lower_barrier;
            f.upper_barrier   = inst.upper_barrier.value_or(0.0);
            return f;
        } else if constexpr (std::is_same_v<T,
                equity_asian_option_instrument>) {
            equity_instrument f;
            f.trade_type_code       = inst.trade_type_code;
            f.underlying_code       = inst.underlying_name;
            f.currency              = inst.currency;
            f.quantity              = inst.notional;
            f.option_type           = inst.option_type;
            f.exercise_type         = inst.exercise_type;
            f.maturity_date         = inst.expiry_date;
            f.strike_price          = inst.strike;
            f.averaging_start_date  = inst.averaging_start_date;
            return f;
        } else if constexpr (std::is_same_v<T,
                equity_digital_option_instrument>) {
            equity_instrument f;
            f.trade_type_code = inst.trade_type_code;
            f.underlying_code = inst.underlying_name;
            f.currency        = inst.currency;
            f.notional        = inst.notional;
            f.option_type     = inst.option_type;
            f.maturity_date   = inst.expiry_date;
            f.strike_price    = inst.strike.value_or(0.0);
            f.barrier_type    = inst.barrier_type;
            f.lower_barrier   = inst.barrier_level.value_or(0.0);
            return f;
        }
        return {};
    }, r.instrument);
}

} // namespace

// ---------------------------------------------------------------------------
// Phase 4 — vanilla equity
// ---------------------------------------------------------------------------

TEST_CASE("equity_mapper_roundtrip_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Option_European.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_option_instrument>(r.instrument);

    CHECK(inst.trade_type_code == "EquityOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.currency.empty());
    CHECK(inst.notional > 0.0);
    CHECK(!inst.option_type.empty());
    CHECK(!inst.expiry_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_option(flat(r));
    REQUIRE(rt.EquityOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityOption roundtrip passed. Underlying: "
                            << inst.underlying_name;
}

TEST_CASE("equity_mapper_roundtrip_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Forward.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_forward_instrument>(r.instrument);

    CHECK(inst.trade_type_code == "EquityForward");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.currency.empty());
    CHECK(inst.quantity > 0.0);
    CHECK(!inst.expiry_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_forward(flat(r));
    REQUIRE(rt.EquityForwardData);

    BOOST_LOG_SEV(lg, info) << "EquityForward roundtrip passed. Maturity: "
                            << inst.expiry_date;
}

TEST_CASE("equity_mapper_roundtrip_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Swap.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_swap_instrument>(r.instrument);

    CHECK(inst.trade_type_code == "EquitySwap");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.currency.empty());
    CHECK(!inst.return_type.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_swap(flat(r));
    REQUIRE(rt.EquitySwapData);
    const bool has_legs = !rt.EquitySwapData->LegData.empty();
    CHECK(has_legs);

    BOOST_LOG_SEV(lg, info) << "EquitySwap roundtrip passed. Return type: "
                            << inst.return_type;
}

TEST_CASE("equity_mapper_roundtrip_variance_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Variance_Swap.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_variance_swap_instrument>(
            r.instrument);

    CHECK(inst.trade_type_code == "EquityVarianceSwap");
    CHECK(!inst.underlying_name.empty());
    CHECK(inst.variance_strike > 0.0);
    CHECK(!inst.start_date.empty());
    CHECK(!inst.maturity_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_variance_swap(flat(r));
    REQUIRE(rt.EquityVarianceSwapData);

    BOOST_LOG_SEV(lg, info) << "EquityVarianceSwap roundtrip passed. Strike: "
                            << inst.variance_strike;
}

TEST_CASE("equity_mapper_roundtrip_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Barrier_Option.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_barrier_option_instrument>(
            r.instrument);

    CHECK(inst.trade_type_code == "EquityBarrierOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.lower_barrier_type.empty());
    CHECK(inst.lower_barrier > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_barrier_option(
        flat(r));
    REQUIRE(rt.EquityBarrierOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityBarrierOption roundtrip passed. Barrier: "
                            << inst.lower_barrier_type;
}

TEST_CASE("equity_mapper_roundtrip_asian_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Asian_Option.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_asian_option_instrument>(
            r.instrument);

    CHECK(inst.trade_type_code == "EquityAsianOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(inst.strike > 0.0);
    CHECK(!inst.averaging_start_date.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_asian_option(
        flat(r));
    REQUIRE(rt.EquityAsianOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityAsianOption roundtrip passed. Averaging from: "
                            << inst.averaging_start_date;
}

TEST_CASE("equity_mapper_roundtrip_digital_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Digital_Option.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_digital_option_instrument>(
            r.instrument);

    CHECK(inst.trade_type_code == "EquityDigitalOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(inst.strike.value_or(0.0) > 0.0);
    CHECK(!inst.option_type.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_digital_option(
        flat(r));
    REQUIRE(rt.EquityDigitalOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityDigitalOption roundtrip passed. Strike: "
                            << inst.strike.value_or(0.0);
}

TEST_CASE("equity_mapper_roundtrip_touch_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_OneTouch_Option.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_digital_option_instrument>(
            r.instrument);

    CHECK(inst.trade_type_code == "EquityTouchOption");
    CHECK(!inst.barrier_type.empty());
    CHECK(inst.barrier_level.value_or(0.0) > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_touch_option(
        flat(r));
    REQUIRE(rt.EquityTouchOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityTouchOption roundtrip passed. Barrier: "
                            << inst.barrier_level.value_or(0.0);
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
