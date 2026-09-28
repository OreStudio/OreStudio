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
#include "ores.ore.core/domain/commodity_instrument_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/trade_mapper.hpp"
#include "ores.platform/filesystem/file.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>

/**
 * @file xml_commodity_mapper_roundtrip_tests.cpp
 * @brief Mapper fidelity tests for commodity instrument types (Phase 7).
 *
 * For each example file:
 *   1. Parse ORE XML into ores.ore domain types.
 *   2. Forward-map to commodity_instrument via trade_mapper.
 *   3. Assert key economic fields are populated.
 *   4. Reverse-map back to ORE XSD trade.
 *   5. Assert the round-tripped XSD type is structurally populated.
 */

namespace {

const std::string_view test_suite("ores.ore.commodity.mapper.roundtrip.tests");
const std::string tags("[ore][xml][mapper][roundtrip][commodity]");

using ores::ore::domain::portfolio;
using ores::ore::domain::commodity_instrument_mapper;
using ores::trading::domain::commodity_instrument;
using ores::trading::domain::commodity_instrument_data;
using namespace ores::logging;
using Catch::Approx;

std::filesystem::path example_path(const std::string& filename) {
    return ores::testing::project_root::resolve("external/ore/examples/Products/Example_Trades/" +
                                                filename);
}

commodity_instrument_data load_and_map_data(const std::string& filename) {
    using ores::platform::filesystem::file;
    const std::string content = file::read_content(example_path(filename));
    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(!p.Trade.empty());
    auto r = ores::ore::domain::trade_mapper::map_commodity_instrument(p.Trade.front());
    REQUIRE(r.has_value());
    return *r;
}

commodity_instrument load_and_map(const std::string& filename) {
    return load_and_map_data(filename).instrument;
}

} // namespace


namespace {

// The domain holds a calendar date; the ORE XML holds its ISO-8601 spelling.
[[maybe_unused]] std::string ore_iso(const std::chrono::year_month_day& d) {
    return ores::platform::time::datetime::to_iso8601_date(d);
}

[[maybe_unused]] std::string ore_iso(const std::optional<std::chrono::year_month_day>& d) {
    return d ? ores::platform::time::datetime::to_iso8601_date(*d) : std::string{};
}

} // namespace

TEST_CASE("commodity_mapper_roundtrip_forward", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Forward.xml");

    CHECK(r.identity.trade_type_code == "CommodityForward");
    CHECK(!r.commodity_code.empty());
    CHECK(!r.currency.empty());
    CHECK(r.quantity > 0.0);
    CHECK(r.fixed_price.has_value());
    CHECK(r.fixed_price->to_double() > 0.0);
    CHECK(r.maturity_date.has_value());

    const auto rt = commodity_instrument_mapper::reverse_commodity_forward(r);
    REQUIRE(rt.CommodityForwardData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityForward roundtrip passed. " << r.commodity_code;
}

TEST_CASE("commodity_mapper_roundtrip_option", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Option.xml");

    CHECK(r.identity.trade_type_code == "CommodityOption");
    CHECK(!r.commodity_code.empty());
    CHECK(!r.currency.empty());
    CHECK(r.quantity > 0.0);
    CHECK(r.strike_price.has_value());
    CHECK(!r.option_type.empty());
    CHECK(r.maturity_date.has_value());

    const auto rt = commodity_instrument_mapper::reverse_commodity_option(r);
    REQUIRE(rt.CommodityOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityOption roundtrip passed. Strike: " << *r.strike_price;
}

TEST_CASE("commodity_mapper_roundtrip_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Swap_NYMEX_A7Q.xml");

    CHECK(r.identity.trade_type_code == "CommoditySwap");
    CHECK(!r.commodity_code.empty());
    CHECK(!r.currency.empty());
    CHECK(r.start_date.has_value());
    CHECK(r.maturity_date.has_value());

    const auto rt = commodity_instrument_mapper::reverse_commodity_swap(r);
    REQUIRE(rt.SwapData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommoditySwap roundtrip passed. " << r.commodity_code;
}

TEST_CASE("commodity_mapper_roundtrip_swaption", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Swaption_NYMEX_NG.xml");

    CHECK(r.identity.trade_type_code == "CommoditySwaption");
    CHECK(!r.commodity_code.empty());
    CHECK(r.swaption_expiry_date.has_value());

    const auto rt = commodity_instrument_mapper::reverse_commodity_swaption(r);
    REQUIRE(rt.CommoditySwaptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommoditySwaption roundtrip passed. Expiry: "
                            << ore_iso(r.swaption_expiry_date);
}

TEST_CASE("commodity_mapper_roundtrip_variance_swap", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Variance_Swap.xml");

    CHECK(r.identity.trade_type_code == "CommodityVarianceSwap");
    CHECK(!r.commodity_code.empty());
    CHECK(!r.currency.empty());
    CHECK(r.start_date.has_value());
    CHECK(r.maturity_date.has_value());
    CHECK(r.variance_strike.has_value());

    const auto rt = commodity_instrument_mapper::reverse_commodity_variance_swap(r);
    REQUIRE(rt.CommodityVarianceSwapData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityVarianceSwap roundtrip passed. Strike: "
                            << *r.variance_strike;
}

TEST_CASE("commodity_mapper_roundtrip_apo", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_APO_NYMEX_CL.xml");

    CHECK(r.identity.trade_type_code == "CommodityAveragePriceOption");
    CHECK(!r.commodity_code.empty());
    CHECK(!r.currency.empty());
    CHECK(r.quantity > 0.0);
    CHECK(r.strike_price.has_value());
    CHECK(r.averaging_start_date.has_value());
    CHECK(r.averaging_end_date.has_value());

    const auto rt = commodity_instrument_mapper::reverse_commodity_apo(r);
    REQUIRE(rt.CommodityAveragePriceOptionData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityAveragePriceOption roundtrip passed. " << r.commodity_code;
}

TEST_CASE("commodity_mapper_roundtrip_option_strip", tags) {
    auto lg(make_logger(test_suite));
    const auto r = load_and_map("Commodity_Option_Strip_NYMEX_NG.xml");

    CHECK(r.identity.trade_type_code == "CommodityOptionStrip");
    CHECK(!r.commodity_code.empty());
    CHECK(!r.strip_frequency_code.empty());

    const auto rt = commodity_instrument_mapper::reverse_commodity_option_strip(r);
    REQUIRE(rt.CommodityOptionStripData.operator bool());

    BOOST_LOG_SEV(lg, info) << "CommodityOptionStrip roundtrip passed. " << r.commodity_code;
}

// The corpus states no commodity basket document, so the producer is proven
// against a document built from the ORE schema's basketOptionData shape. The
// constituent list and its weights are the values the text column used to
// drop, and they must survive the round trip.
TEST_CASE("commodity_mapper_basket_option_carries_constituents", tags) {
    auto lg(make_logger(test_suite));
    const std::string content = R"(<Portfolio>
  <Trade id="Commodity_Basket_Option">
    <TradeType>CommodityBasketOption</TradeType>
    <Envelope>
      <CounterParty>CPTY</CounterParty>
      <NettingSetId>NS</NettingSetId>
      <AdditionalFields>
        <party_id>party</party_id>
        <valuation_date>2025-02-10</valuation_date>
      </AdditionalFields>
    </Envelope>
    <CommodityBasketOptionData>
      <Currency>USD</Currency>
      <Notional>1000000</Notional>
      <Strike>55</Strike>
      <Underlyings>
        <Underlying>
          <Type>Commodity</Type>
          <Name>NYMEX:CL</Name>
          <Weight>0.6</Weight>
        </Underlying>
        <Underlying>
          <Type>Commodity</Type>
          <Name>NYMEX:NG</Name>
          <Weight>0.4</Weight>
        </Underlying>
      </Underlyings>
      <OptionData>
        <LongShort>Long</LongShort>
        <OptionType>Call</OptionType>
        <Style>European</Style>
        <Settlement>Cash</Settlement>
        <ExerciseDates>
          <ExerciseDate>2025-10-03</ExerciseDate>
        </ExerciseDates>
      </OptionData>
    </CommodityBasketOptionData>
  </Trade>
</Portfolio>)";

    portfolio p;
    ores::ore::domain::load_data(content, p);
    REQUIRE(p.Trade.size() == 1);

    auto data = ores::ore::domain::trade_mapper::map_commodity_instrument(p.Trade.front());
    REQUIRE(data.has_value());
    CHECK(data->instrument.identity.trade_type_code == "CommodityBasketOption");
    CHECK(data->instrument.commodity_code == "NYMEX:CL");
    CHECK(data->instrument.currency == "USD");
    REQUIRE(data->constituents.size() == 2);
    CHECK(data->constituents[0].sequence_number == 1);
    CHECK(data->constituents[0].underlying_code == "NYMEX:CL");
    REQUIRE(data->constituents[0].weight.has_value());
    CHECK(data->constituents[0].weight->to_double() == Approx(0.6));
    CHECK(data->constituents[1].sequence_number == 2);
    CHECK(data->constituents[1].underlying_code == "NYMEX:NG");
    REQUIRE(data->constituents[1].weight.has_value());
    CHECK(data->constituents[1].weight->to_double() == Approx(0.4));

    const auto rt = commodity_instrument_mapper::reverse_commodity_basket_option(
        data->instrument, data->constituents);
    REQUIRE(rt.CommodityBasketOptionData.operator bool());
    const auto& underlyings = rt.CommodityBasketOptionData->Underlyings.Underlying;
    REQUIRE(underlyings.size() == 2);
    CHECK(std::string(underlyings[0].Name) == "NYMEX:CL");
    REQUIRE(static_cast<bool>(underlyings[0].Weight));
    CHECK(*underlyings[0].Weight == Approx(0.6f));
    CHECK(std::string(underlyings[1].Name) == "NYMEX:NG");
    REQUIRE(static_cast<bool>(underlyings[1].Weight));
    CHECK(*underlyings[1].Weight == Approx(0.4f));

    BOOST_LOG_SEV(lg, info) << "CommodityBasketOption roundtrip passed. Constituents: "
                            << data->constituents.size();
}
