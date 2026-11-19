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
#include "ores.marketdata.api/datum/market_index.hpp"
#include "ores.trading.api/builders/multi_asset_builders.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto multi_asset_tags("[builders][multi_asset]");

decimal multi_asset_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference multi_asset_equity(const char* code) {
    auto index = md::market_index::make(md::index_family::equity, code, {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

european_option_barrier_terms spx_on_ndx_barrier() {
    return {.family = scripted_family::equity,
            .option_underlying = multi_asset_equity(".SPX"),
            .barrier_underlying = multi_asset_equity(".NDX"),
            .quantity = multi_asset_amount("1000"),
            .kind = option_kind::call,
            .position = option_position::long_option,
            .strike = multi_asset_amount("4000"),
            .premium_amount = multi_asset_amount("0"),
            .premium_currency = std::string("USD"),
            .premium_date = 2026y / 1 / 7,
            .expiry = 2027y / 1 / 5,
            .barrier = scripted_barrier{.type = "DownAndOut", .level = multi_asset_amount("15000")},
            .settlement = 2027y / 1 / 7,
            .pay_currency = std::string("USD")};
}

}

TEST_CASE("a_double_digital_option_states_its_kinds_and_its_pair", multi_asset_tags) {
    const auto option =
        double_digital_option_builder({.expiry = 2027y / 1 / 5,
                                       .settlement = 2027y / 1 / 7,
                                       .binary_payout = multi_asset_amount("1000"),
                                       .binary_level_1 = multi_asset_amount("100"),
                                       .binary_level_2 = multi_asset_amount("120"),
                                       .kind_1 = digital_kind::call,
                                       .kind_2 = digital_kind::put,
                                       .position = option_position::short_option,
                                       .pay_currency = std::string("EUR"),
                                       .underlying_1 = multi_asset_equity(".SPX"),
                                       .underlying_2 = multi_asset_equity(".STOXX50E")})
            .build();
    REQUIRE(option.has_value());
    REQUIRE(option->double_digital_options.size() == 1u);
    const auto& row = option->double_digital_options[0];
    CHECK(row.type_1 == "Call");
    CHECK(row.type_2 == "Put");
    CHECK(row.long_short == "Short");
    CHECK(row.binary_level_2 == multi_asset_amount("120"));
    CHECK_FALSE(row.binary_level_upper_1.has_value());
    REQUIRE(option->instrument_underlyings.size() == 2u);
    CHECK(option->instrument_underlyings[1].underlying_role == "underlying2");
}

TEST_CASE("an_autocallable_states_its_fixing_and_settlement_schedules", multi_asset_tags) {
    const auto note =
        autocallable_builder(
            {.notional = multi_asset_amount("1000000"),
             .determination_level = multi_asset_amount("70"),
             .trigger_level = multi_asset_amount("100"),
             .underlying = multi_asset_equity(".SPX"),
             .position = option_position::long_option,
             .pay_currency = std::string("EUR"),
             .fixing_dates = scripted_schedule{2026y / 1 / 5, 2029y / 1 / 5, "1Y"},
             .settlement_dates = scripted_schedule{2026y / 1 / 5, 2029y / 2 / 5, "1Y"},
             .accumulation_factors =
                 std::vector{multi_asset_amount("0.05"), multi_asset_amount("0.1")},
             .cap = multi_asset_amount("0.15")})
            .build();
    REQUIRE(note.has_value());
    REQUIRE(note->autocallables.size() == 1u);
    CHECK(note->autocallables[0].identity.trade_type_code == "Autocallable_01");
    CHECK(note->autocallables[0].cap == multi_asset_amount("0.15"));
    REQUIRE(note->schedules.size() == 2u);
    CHECK(note->schedules[0].schedule_role == "fixing_schedule");
    CHECK(note->schedules[1].schedule_role == "settlement_schedule");
    REQUIRE(note->values.size() == 2u);
    CHECK(note->values[1].value_role == "accumulation_factor");
    CHECK(note->values[1].value == multi_asset_amount("0.1"));
    REQUIRE(note->instrument_underlyings.size() == 1u);
}

TEST_CASE("an_autocallable_whose_fixings_end_before_they_start_fails", multi_asset_tags) {
    const auto note =
        autocallable_builder(
            {.notional = multi_asset_amount("1000000"),
             .determination_level = multi_asset_amount("70"),
             .trigger_level = multi_asset_amount("100"),
             .underlying = multi_asset_equity(".SPX"),
             .position = option_position::long_option,
             .pay_currency = std::string("EUR"),
             .fixing_dates = scripted_schedule{2029y / 1 / 5, 2026y / 1 / 5, "1Y"},
             .settlement_dates = scripted_schedule{2026y / 1 / 5, 2029y / 2 / 5, "1Y"},
             .accumulation_factors = std::vector{multi_asset_amount("0.05")},
             .cap = multi_asset_amount("0.15")})
            .build();
    REQUIRE_FALSE(note.has_value());
}

TEST_CASE("a_performance_option_states_its_strike_ladder_and_its_basket", multi_asset_tags) {
    const auto option =
        performance_option_builder(
            {.notional = multi_asset_amount("1000000"),
             .participation_rate = multi_asset_amount("1"),
             .valuation_date = 2027y / 1 / 5,
             .settlement_date = 2027y / 1 / 7,
             .strike_prices = std::vector{multi_asset_amount("100"),
                                          multi_asset_amount("110"),
                                          multi_asset_amount("120")},
             .strike = multi_asset_amount("1"),
             .position = option_position::long_option,
             .pay_currency = std::string("USD"),
             .underlyings = std::vector{multi_asset_equity(".SPX"), multi_asset_equity(".NDX")}})
            .build();
    REQUIRE(option.has_value());
    REQUIRE(option->performance_options.size() == 1u);
    CHECK(option->performance_options[0].participation_rate == 1.0);
    REQUIRE(option->values.size() == 3u);
    CHECK(option->values[2].value_role == "strike_price");
    CHECK(option->values[2].sequence_number == 3);
    REQUIRE(option->instrument_underlyings.size() == 2u);
    CHECK(option->instrument_underlyings[1].sequence_number == 2);
}

TEST_CASE("a_european_option_barrier_names_its_option_and_its_barrier_underlying",
          multi_asset_tags) {
    const auto built = european_option_barrier_builder(spx_on_ndx_barrier()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->european_option_barriers.size() == 1u);
    const auto& row = built->european_option_barriers[0];
    CHECK(row.barrier_style == "European");
    CHECK(row.barrier_type == "DownAndOut");
    CHECK(row.quantity == 1000.0);
    CHECK(row.option_type == "Call");
    REQUIRE(built->instrument_underlyings.size() == 2);
    CHECK(built->instrument_underlyings[0].underlying_role == "optionunderlying");
    CHECK(built->instrument_underlyings[1].underlying_role == "barrierunderlying");
    CHECK(built->instrument_underlyings[0].underlying_type == "Equity");
}

TEST_CASE("a_european_option_barrier_refuses_a_settlement_before_its_expiry", multi_asset_tags) {
    auto terms = spx_on_ndx_barrier();
    terms.settlement = 2027y / 1 / 1;
    const auto built = european_option_barrier_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "settlement");
}
