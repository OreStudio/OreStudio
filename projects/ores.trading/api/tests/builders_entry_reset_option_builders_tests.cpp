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
#include "ores.trading.api/builders/entry_reset_option_builders.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <variant>
#include <vector>

using namespace ores::trading::builders;
using namespace std::chrono;
namespace md = ores::marketdata::datum;
namespace domain = ores::trading::domain;
using ores::utility::decimal::decimal;

namespace {

const auto entry_reset_tags("[builders][entry_reset]");

decimal entry_reset_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference entry_reset_equity(const char* code) {
    auto index = md::market_index::make(md::index_family::equity, code, {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

best_entry_option_terms spx_best_entry() {
    return {.underlying = entry_reset_equity(".SPX"),
            .currency = std::string("USD"),
            .notional = entry_reset_amount("1000000"),
            .multiplier = entry_reset_amount("1"),
            .strike = entry_reset_amount("0.85"),
            .cap = entry_reset_amount("0.06"),
            .trigger_level = entry_reset_amount("0.95"),
            .reset_minimum = entry_reset_amount("0.85"),
            .premium = entry_reset_amount("100"),
            .premium_date = 2026y / 1 / 7,
            .strike_date = 2026y / 1 / 5,
            .strike_observation_dates =
                std::vector<year_month_day>{2026y / 3 / 2, 2026y / 6 / 1, 2026y / 9 / 1},
            .expiry = 2027y / 1 / 5,
            .settlement = 2027y / 1 / 7,
            .position = option_position::long_option};
}

strike_resettable_option_terms spx_strike_resettable() {
    return {.underlying = entry_reset_equity(".SPX"),
            .currency = std::string("USD"),
            .kind = option_kind::put,
            .position = option_position::long_option,
            .quantity = entry_reset_amount("10000"),
            .strike = entry_reset_amount("3500"),
            .reset_strike = entry_reset_amount("3650"),
            .trigger = trigger_direction::up,
            .trigger_price = entry_reset_amount("4000"),
            .observation_dates =
                std::vector<year_month_day>{2026y / 4 / 27, 2026y / 5 / 25, 2026y / 6 / 22},
            .expiry = 2026y / 6 / 22,
            .settlement = 2026y / 6 / 26,
            .premium = entry_reset_amount("100"),
            .premium_date = 2026y / 6 / 26};
}

}

TEST_CASE("a_best_entry_option_states_its_terms_its_underlying_and_its_observation_dates",
          entry_reset_tags) {
    const auto built = fx_best_entry_option_builder(spx_best_entry()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->best_entry_options.size() == 1);
    const auto& row = built->best_entry_options[0];
    CHECK(row.identity.trade_type_code == "FxBestEntryOption");
    CHECK(row.long_short == "Long");
    CHECK(row.trigger_level == entry_reset_amount("0.95"));
    CHECK(row.reset_minimum == entry_reset_amount("0.85"));
    CHECK(row.strike_date == 2026y / 1 / 5);
    REQUIRE(built->instrument_underlyings.size() == 1);
    CHECK(built->instrument_underlyings[0].underlying_role == "underlying");
    CHECK(built->instrument_underlyings[0].underlying_type == "FX");
    REQUIRE(built->schedules.size() == 1);
    CHECK(built->schedules[0].schedule_kind == "dates");
    REQUIRE(built->schedule_dates.size() == 3);
    CHECK(built->schedule_dates[0].schedule_role == "strike_observation_schedule");
    CHECK(built->schedule_dates[2].sequence_number == 3);
    CHECK(built->schedule_dates[2].schedule_date == 2026y / 9 / 1);
}

TEST_CASE("a_best_entry_option_refuses_an_observation_on_or_before_the_strike_date",
          entry_reset_tags) {
    auto terms = spx_best_entry();
    terms.strike_observation_dates = std::vector<year_month_day>{2026y / 1 / 5};
    const auto built = equity_best_entry_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "strike_observation_dates");
}

TEST_CASE("a_strike_resettable_option_states_its_trigger_and_its_observation_dates",
          entry_reset_tags) {
    const auto built = equity_strike_resettable_option_builder(spx_strike_resettable()).build();
    REQUIRE(built.has_value());
    const auto* row =
        std::get_if<domain::equity_strike_resettable_option_instrument>(&built->instrument);
    REQUIRE(row != nullptr);
    CHECK(row->identity.trade_type_code == "EquityStrikeResettableOption");
    CHECK(row->option_type == "Put");
    CHECK(row->trigger_type == "Up");
    CHECK(row->quantity == 10000.0);
    CHECK(row->reset_strike == entry_reset_amount("3650"));
    CHECK(built->instrument_underlyings[0].underlying_type == "Equity");
    REQUIRE(built->schedule_dates.size() == 3);
    CHECK(built->schedule_dates[0].schedule_role == "observation_schedule");
}

TEST_CASE("a_strike_resettable_option_refuses_dates_out_of_order_or_after_the_expiry",
          entry_reset_tags) {
    auto terms = spx_strike_resettable();
    terms.observation_dates = std::vector<year_month_day>{2026y / 5 / 25, 2026y / 4 / 27};
    auto built = fx_strike_resettable_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "observation_dates");

    terms.observation_dates = std::vector<year_month_day>{2026y / 7 / 1};
    built = fx_strike_resettable_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "observation_dates");
}
