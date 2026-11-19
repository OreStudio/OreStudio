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
#include "ores.trading.api/builders/worst_of_basket_swap_builders.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <vector>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace md = ores::marketdata::datum;
using ores::utility::decimal::decimal;

namespace {

const auto worst_of_tags("[builders][worst_of]");

decimal worst_of_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference worst_of_index(const char* code) {
    auto index = md::market_index::make(md::index_family::equity, code, {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

}

namespace {

fx_worst_of_basket_swap_terms spx_and_ndx_worst_of() {
    return {
        .members =
            std::vector<worst_of_member>{
                {.underlying = worst_of_index(".SPX"), .initial_price = worst_of_amount("1.1")},
                {.underlying = worst_of_index(".NDX"), .initial_price = worst_of_amount("0.8")}},
        .currency = std::string("USD"),
        .quantity = worst_of_amount("1000000"),
        .initial_fixed_rate = worst_of_amount("0.02"),
        .period_dates = std::vector<year_month_day>{2026y / 1 / 5, 2026y / 7 / 6, 2027y / 1 / 5},
        .pay_dates = std::vector<year_month_day>{2026y / 7 / 8, 2027y / 1 / 7},
        .floating_index = std::string("USD-SOFR"),
        .floating_spread = worst_of_amount("0"),
        .floating_day_count = std::string("A360"),
        .knock_out_levels = std::vector<decimal>{worst_of_amount("1"), worst_of_amount("1")},
        .fixed_trigger_levels =
            std::vector<decimal>{worst_of_amount("0.75"), worst_of_amount("0.75")},
        .fixed_rate = worst_of_amount("0.05"),
        .knock_in_level = worst_of_amount("0.75"),
        .strike = worst_of_amount("1"),
        .position = option_position::long_option};
}

}

TEST_CASE("an_fx_worst_of_basket_swap_states_its_schedules_and_its_level_lists", worst_of_tags) {
    const auto built = fx_worst_of_basket_swap_builder(spx_and_ndx_worst_of()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->worst_of_basket_swaps.size() == 1);
    const auto& row = built->worst_of_basket_swaps[0];
    CHECK(row.identity.trade_type_code == "FxWorstOfBasketSwap");
    CHECK(row.quantity == 1000000.0);
    CHECK(row.knock_in_level == std::optional(worst_of_amount("0.75")));
    CHECK(row.bermudan_knock_in == std::optional(false));
    REQUIRE(built->instrument_underlyings.size() == 2);
    CHECK(built->instrument_underlyings[1].underlying_type == "FX");
    REQUIRE(built->schedules.size() == 3);
    const auto fixing = std::ranges::find_if(built->schedules, [](const auto& s) {
        return s.schedule_role == "floating_fixing_schedule";
    });
    REQUIRE(fixing != built->schedules.end());
    CHECK(fixing->schedule_kind == "derived");
    CHECK(fixing->base_schedule == "FloatingPeriodSchedule");
    CHECK(built->schedule_dates.size() == 5u);
    REQUIRE(built->values.size() == 6u);
    CHECK(built->values[0].value_role == "initial_price");
    CHECK(built->values[1].value == worst_of_amount("0.8"));
    CHECK(built->values[2].value_role == "knock_out_level");
    CHECK(built->values[5].value_role == "fixed_trigger_level");
    CHECK(built->values[5].sequence_number == 2);
}

TEST_CASE("an_fx_worst_of_basket_swap_refuses_level_lists_that_miss_a_period", worst_of_tags) {
    auto terms = spx_and_ndx_worst_of();
    terms.knock_out_levels = std::vector<decimal>{worst_of_amount("1")};
    auto built = fx_worst_of_basket_swap_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "knock_out_levels");

    terms = spx_and_ndx_worst_of();
    terms.pay_dates = std::vector<year_month_day>{2026y / 7 / 1, 2027y / 1 / 7};
    built = fx_worst_of_basket_swap_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "pay_dates");
}
