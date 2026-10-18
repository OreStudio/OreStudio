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
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/equity_instrument_mapper.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>

/**
 * @file xml_equity_mapper_roundtrip_tests.cpp
 * @brief Thing 3: mapper fidelity tests for equity instrument types.
 *
 * Phases 4 and 5: vanilla and exotic equity types.
 * For each example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map to the per-type equity domain via trade_mapper.
 *   3. Assert key economic fields are populated.
 *   4. Reverse-map back to ORE XSD trade.
 *   5. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite("ores.ore.equity.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][equity]");

using ores::ore::domain::portfolio;
using ores::ore::domain::equity_instrument_mapper;
using ores::trading::domain::equity_instrument_data;
using namespace ores::logging;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

equity_instrument_data load_and_map(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_equity_instrument(p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

/*
 * A position is a top-level trade, and the corpus documents none at the top
 * level: the only documented positions sit inside a total return swap. Take the
 * sub-trade out of the swap and map it as the top-level document it is, so the
 * test exercises the model rather than the nesting the importer no longer
 * produces.
 */
equity_instrument_data load_position_as_top_level(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    REQUIRE(p.Trade.front().TotalReturnSwapData);
    const auto& groups = p.Trade.front().TotalReturnSwapData->UnderlyingData.subTradeGroup;
    REQUIRE(!groups.empty());
    REQUIRE(groups.front().SubTrade);

    ores::ore::domain::trade position;
    position.TradeType = groups.front().SubTrade->SubTradeType;
    position.EquityPositionData = groups.front().SubTrade->EquityPositionData;
    position.EquityOptionPositionData = groups.front().SubTrade->EquityOptionPositionData;

    auto r = ores::ore::domain::trade_mapper::map_equity_instrument(position);
    REQUIRE(r.has_value());
    return *r;
}

} // namespace

// ---------------------------------------------------------------------------
// Phase 4 — vanilla equity
// ---------------------------------------------------------------------------


namespace {

// The domain holds a calendar date; the ORE XML holds its ISO-8601 spelling.
[[maybe_unused]] std::string ore_iso(const std::chrono::year_month_day& d) {
    return ores::platform::time::datetime::to_iso8601_date(d);
}

[[maybe_unused]] std::string ore_iso(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

} // namespace

TEST_CASE("equity_mapper_roundtrip_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Option_European.xml");
    const auto& inst = std::get<ores::trading::domain::equity_option_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.currency.empty());
    CHECK(inst.notional.to_double() > 0.0);
    CHECK(!inst.option_type.empty());
    CHECK(inst.expiry_date.ok());

    const auto rt = equity_instrument_mapper::reverse_equity_option(inst);
    REQUIRE(rt.EquityOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityOption roundtrip passed. Underlying: "
                            << inst.underlying_name;
}

TEST_CASE("equity_mapper_roundtrip_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Forward.xml");
    const auto& inst = std::get<ores::trading::domain::equity_forward_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityForward");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.currency.empty());
    CHECK(inst.quantity > 0.0);
    CHECK(inst.expiry_date.ok());

    const auto rt = equity_instrument_mapper::reverse_equity_forward(inst);
    REQUIRE(rt.EquityForwardData);

    BOOST_LOG_SEV(lg, info) << "EquityForward roundtrip passed. Maturity: " << inst.expiry_date;
}

TEST_CASE("equity_mapper_roundtrip_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Swap.xml");
    const auto& inst = std::get<ores::trading::domain::equity_swap_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquitySwap");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.currency.empty());
    CHECK(!inst.return_type.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_swap(inst);
    REQUIRE(rt.EquitySwapData);
    const bool has_legs = !rt.EquitySwapData->LegData.empty();
    CHECK(has_legs);

    BOOST_LOG_SEV(lg, info) << "EquitySwap roundtrip passed. Return type: " << inst.return_type;
}

TEST_CASE("equity_mapper_roundtrip_variance_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Variance_Swap.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_variance_swap_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityVarianceSwap");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.variance_strike.is_zero());
    CHECK(inst.start_date.ok());
    CHECK(inst.maturity_date.ok());

    const auto rt = equity_instrument_mapper::reverse_equity_variance_swap(inst);
    REQUIRE(rt.EquityVarianceSwapData);

    BOOST_LOG_SEV(lg, info) << "EquityVarianceSwap roundtrip passed. Strike: "
                            << inst.variance_strike.to_string();
}

TEST_CASE("equity_mapper_roundtrip_barrier_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Barrier_Option.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_barrier_option_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityBarrierOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(!inst.lower_barrier_type.empty());
    CHECK(inst.lower_barrier.to_double() > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_barrier_option(inst);
    REQUIRE(rt.EquityBarrierOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityBarrierOption roundtrip passed. Barrier: "
                            << inst.lower_barrier_type;
}

TEST_CASE("equity_mapper_roundtrip_asian_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Asian_Option.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_asian_option_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityAsianOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(inst.strike.to_double() > 0.0);
    CHECK(inst.averaging_start_date.ok());

    const auto rt = equity_instrument_mapper::reverse_equity_asian_option(inst);
    REQUIRE(rt.EquityAsianOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityAsianOption roundtrip passed. Averaging from: "
                            << inst.averaging_start_date;
}

TEST_CASE("equity_mapper_roundtrip_digital_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_Digital_Option.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_digital_option_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityDigitalOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(inst.strike.value_or(ores::utility::decimal::decimal{}).to_double() > 0.0);
    CHECK(!inst.option_type.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_digital_option(inst);
    REQUIRE(rt.EquityDigitalOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityDigitalOption roundtrip passed. Strike: "
                            << inst.strike.value_or(ores::utility::decimal::decimal{}).to_double();
}

TEST_CASE("equity_mapper_roundtrip_touch_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_OneTouch_Option.xml");
    const auto& inst =
        std::get<ores::trading::domain::equity_digital_option_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityTouchOption");
    CHECK(!inst.barrier_type.empty());
    CHECK(inst.barrier_level.value_or(ores::utility::decimal::decimal{}).to_double() > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_touch_option(inst);
    REQUIRE(rt.EquityTouchOptionData);

    BOOST_LOG_SEV(lg, info)
        << "EquityTouchOption roundtrip passed. Barrier: "
        << inst.barrier_level.value_or(ores::utility::decimal::decimal{}).to_double();
}

TEST_CASE("equity_mapper_roundtrip_outperformance_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Equity_OutperformanceOption.xml");
    const auto& inst = std::get<ores::trading::domain::equity_option_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityOutperformanceOption");
    CHECK(!inst.currency.empty());
    CHECK(inst.notional.to_double() > 0.0);
    // Outperformance joins two underlyings as "n1/n2" in underlying_name
    // because the per-type option struct has no basket field.
    CHECK(inst.underlying_name.find('/') != std::string::npos);

    const auto rt = equity_instrument_mapper::reverse_equity_outperformance_option(inst);
    REQUIRE(rt.EquityOutperformanceOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityOutperformanceOption roundtrip passed. Pair: "
                            << inst.underlying_name;
}

// ---------------------------------------------------------------------------
// Phase 5 — exotic equity
// ---------------------------------------------------------------------------

TEST_CASE("equity_mapper_roundtrip_accumulator", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityAccumulator_single_name.xml");
    const auto& inst = std::get<ores::trading::domain::equity_accumulator_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityAccumulator");
    CHECK(!inst.underlying_name.empty());
    CHECK(inst.fixing_amount.to_double() > 0.0);
    CHECK(inst.knock_out_level.value_or(ores::utility::decimal::decimal{}).to_double() > 0.0);
    CHECK(!inst.range_bounds_json.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_accumulator(inst);
    REQUIRE(rt.EquityAccumulatorData);
    REQUIRE(rt.EquityAccumulatorData->RangeBounds.RangeBound.size() == 2);
    CHECK(rt.EquityAccumulatorData->RangeBounds.RangeBound[0].RangeTo == 4000.0f);
    CHECK(rt.EquityAccumulatorData->RangeBounds.RangeBound[0].Leverage == 1.0f);
    CHECK(rt.EquityAccumulatorData->RangeBounds.RangeBound[1].RangeFrom == 4000.0f);
    CHECK(rt.EquityAccumulatorData->RangeBounds.RangeBound[1].Leverage == 2.0f);

    BOOST_LOG_SEV(lg, info) << "EquityAccumulator roundtrip passed. Amount: " << inst.fixing_amount;
}

TEST_CASE("equity_mapper_roundtrip_tarf", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityTaRF.xml");
    const auto& inst = std::get<ores::trading::domain::equity_accumulator_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityTaRF");
    CHECK(!inst.underlying_name.empty());
    CHECK(inst.fixing_amount.to_double() > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_tarf(inst);
    REQUIRE(rt.EquityTaRFData);

    BOOST_LOG_SEV(lg, info) << "EquityTaRF roundtrip passed. Amount: " << inst.fixing_amount;
}

TEST_CASE("equity_mapper_roundtrip_cliquet_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_Equity_Cliquet_Option.xml");
    const auto& inst = std::get<ores::trading::domain::equity_option_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityCliquetOption");
    CHECK(!inst.underlying_name.empty());
    CHECK(inst.notional.to_double() > 0.0);

    const auto rt = equity_instrument_mapper::reverse_equity_cliquet_option(inst);
    REQUIRE(rt.EquityCliquetOptionData);

    BOOST_LOG_SEV(lg, info) << "EquityCliquetOption roundtrip passed.";
}

TEST_CASE("equity_mapper_roundtrip_worst_of_basket_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Exotic_EquityWorstOfBasketSwap.xml");
    const auto& inst = std::get<ores::trading::domain::equity_swap_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityWorstOfBasketSwap");
    CHECK(!inst.currency.empty());
    CHECK(inst.notional.to_double() > 0.0);
    CHECK(!inst.basket_json.empty());
    CHECK(!inst.initial_prices_json.empty());
    CHECK(!inst.knock_out_levels_json.empty());
    CHECK(!inst.fixed_trigger_levels_json.empty());

    const auto rt = equity_instrument_mapper::reverse_equity_worst_of_basket_swap(inst);
    REQUIRE(rt.EquityWorstOfBasketSwapData);
    REQUIRE(rt.EquityWorstOfBasketSwapData->InitialPrices.InitialPrice.size() == 2);
    CHECK(rt.EquityWorstOfBasketSwapData->InitialPrices.InitialPrice[0] == 3481.44f);
    CHECK(rt.EquityWorstOfBasketSwapData->InitialPrices.InitialPrice[1] == 3714.24f);
    REQUIRE(rt.EquityWorstOfBasketSwapData->KnockOutLevels.KnockOutLevel.size() == 4);
    CHECK(rt.EquityWorstOfBasketSwapData->KnockOutLevels.KnockOutLevel[0] == 1.0f);
    REQUIRE(rt.EquityWorstOfBasketSwapData->FixedTriggerLevels.FixedTriggerLevel.size() == 4);
    CHECK(rt.EquityWorstOfBasketSwapData->FixedTriggerLevels.FixedTriggerLevel[0] == 0.75f);

    BOOST_LOG_SEV(lg, info) << "EquityWorstOfBasketSwap roundtrip passed. Basket: "
                            << inst.basket_json;
}

// ---------------------------------------------------------------------------
// Equity option position — the entries the mapper used to drop
// ---------------------------------------------------------------------------

TEST_CASE("equity_option_position_forward_carries_every_entry", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_position_as_top_level("Hybrid_GenericTRS_with_EquityOptionPosition.xml");
    const auto& inst = std::get<ores::trading::domain::equity_position_instrument>(r.instrument);

    CHECK(inst.identity.trade_type_code == "EquityOptionPosition");
    CHECK(inst.quantity == 1000.0);

    REQUIRE(r.underlyings.size() == 2);

    CHECK(r.underlyings[0].sequence_number == 1);
    CHECK(r.underlyings[0].underlying_name == ".SPX");
    CHECK(r.underlyings[0].strike.to_string() == "3300");
    CHECK(r.underlyings[0].long_short == "Long");
    CHECK(r.underlyings[0].option_type == "Call");
    CHECK(r.underlyings[0].exercise_type == "European");
    CHECK(r.underlyings[0].settlement_type == "Cash");

    CHECK(r.underlyings[1].sequence_number == 2);
    CHECK(r.underlyings[1].underlying_name == ".SPX");
    CHECK(r.underlyings[1].strike.to_string() == "3400");

    REQUIRE(r.underlyings[0].weight.has_value());
    CHECK(*r.underlyings[0].weight == 0.5);

    BOOST_LOG_SEV(lg, info) << "EquityOptionPosition forward passed. Entries: "
                            << r.underlyings.size();
}

TEST_CASE("equity_option_position_roundtrip_rebuilds_the_entries", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_position_as_top_level("Hybrid_GenericTRS_with_EquityOptionPosition.xml");
    const auto& inst = std::get<ores::trading::domain::equity_position_instrument>(r.instrument);

    const auto rt = equity_instrument_mapper::reverse_equity_option_position(
        inst, r.underlyings);

    CHECK(rt.TradeType == ores::ore::domain::oreTradeType::EquityOptionPosition);
    REQUIRE(rt.EquityOptionPositionData);
    const auto& d = *rt.EquityOptionPositionData;

    CHECK(d.Quantity == 1000.0F);
    REQUIRE(d.Underlying.size() == 2);
    CHECK(std::string(d.Underlying[0].Underlying.Name) == ".SPX");
    CHECK(d.Underlying[0].Strike == 3300.0F);
    CHECK(std::string(d.Underlying[1].Underlying.Name) == ".SPX");
    CHECK(d.Underlying[1].Strike == 3400.0F);
    REQUIRE(d.Underlying[0].Underlying.Weight);
    CHECK(*d.Underlying[0].Underlying.Weight == 0.5F);
    CHECK(std::string(d.Underlying[0].OptionData.LongShort) == "Long");
    REQUIRE(d.Underlying[0].OptionData.OptionType);
    CHECK(std::string(*d.Underlying[0].OptionData.OptionType) == "Call");
    REQUIRE(d.Underlying[0].OptionData.Style);
    CHECK(std::string(*d.Underlying[0].OptionData.Style) == "European");
    REQUIRE(d.Underlying[0].OptionData.Settlement);
    CHECK(*d.Underlying[0].OptionData.Settlement == ores::ore::domain::settlementType::Cash);

    BOOST_LOG_SEV(lg, info) << "EquityOptionPosition roundtrip passed. Entries: "
                            << d.Underlying.size();
}

/*
 * The plain position reverse had no coverage: nothing but the exporter called
 * it, and the corpus carries no top-level position document to reach it with.
 * It writes the whole document, so build the domain row and assert what comes
 * back rather than reaching it through a fixture.
 */
TEST_CASE("equity_position_reverse_writes_its_own_document", tags) {
    auto lg(make_logger(test_suite));

    ores::trading::domain::equity_position_instrument inst;
    inst.quantity = 7006.0;
    inst.underlying_name = ".SPX";

    const auto rt = equity_instrument_mapper::reverse_equity_position(inst);

    CHECK(rt.TradeType == ores::ore::domain::oreTradeType::EquityPosition);
    REQUIRE(rt.EquityPositionData);
    CHECK(!rt.TotalReturnSwapData);
    CHECK(rt.EquityPositionData->Quantity == 7006.0F);
    REQUIRE(rt.EquityPositionData->Underlying.size() == 1);
    CHECK(std::string(rt.EquityPositionData->Underlying[0].Name) == ".SPX");
}
