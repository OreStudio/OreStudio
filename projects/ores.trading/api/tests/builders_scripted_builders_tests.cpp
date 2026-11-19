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
#include "ores.trading.api/builders/scripted_builders.hpp"
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

const auto scripted_tags("[builders][scripted]");

decimal scripted_amount(const char* text) {
    return *decimal::from_string(text);
}

md::market_reference scripted_equity(const char* code) {
    auto index = md::market_index::make(md::index_family::equity, code, {});
    REQUIRE(index);
    auto reference = md::market_reference::of(std::move(*index));
    REQUIRE(reference);
    return *reference;
}

ores::trading::domain::script_input
scripted_input(const char* name, const char* type, bool is_array) {
    ores::trading::domain::script_input row;
    row.script_name = "Example";
    row.input_name = name;
    row.data_type = type;
    row.is_array = is_array;
    return row;
}

scripted_trade_terms scripted_example() {
    return {.script_name = std::string("Example"),
            .inputs = {scripted_input("Strike", "Number", false),
                       scripted_input("Expiry", "Event", false),
                       scripted_input("Underlyings", "Index", true),
                       scripted_input("FixingDates", "Event", true)},
            .variables = {
                {"Strike", "Number", std::string("100")},
                {"Expiry", "Event", std::string("2027-01-05")},
                {"Underlyings", "Index", std::vector<std::string>{"A", "B"}},
                {"FixingDates", "Event", scripted_schedule{2026y / 1 / 5, 2027y / 1 / 5, "3M"}}}};
}

}

TEST_CASE("a_scripted_trade_states_each_variable_in_its_shape", scripted_tags) {
    const auto trade = scripted_trade_builder(scripted_example()).build();
    REQUIRE(trade.has_value());
    CHECK(trade->instrument.script_name == "Example");
    CHECK(trade->instrument.identity.trade_type_code == "ScriptedTrade");
    REQUIRE(trade->parameters.size() == 4u);
    CHECK(trade->parameters[0].shape == "scalar");
    CHECK(trade->parameters[2].shape == "vector");
    CHECK(trade->parameters[3].shape == "schedule");
    CHECK(trade->parameter_values.size() == 2u);
    REQUIRE(trade->schedules.size() == 1u);
    CHECK(trade->schedules[0].owner_number == 4);
}

TEST_CASE("a_scripted_trade_that_leaves_out_an_input_names_it", scripted_tags) {
    auto terms = scripted_example();
    std::erase_if(terms.variables, [](const auto& v) { return v.name == "Expiry"; });
    const auto trade = scripted_trade_builder(terms).build();
    REQUIRE_FALSE(trade.has_value());
    CHECK(trade.error().term == "Expiry");
}

TEST_CASE("a_scripted_trade_that_states_an_input_in_another_shape_names_it", scripted_tags) {
    auto terms = scripted_example();
    terms.variables[2].value = std::string("A");
    const auto trade = scripted_trade_builder(terms).build();
    REQUIRE_FALSE(trade.has_value());
    CHECK(trade.error().term == "Underlyings");
}

TEST_CASE("a_scripted_trade_that_states_a_variable_the_script_does_not_read_names_it",
          scripted_tags) {
    auto terms = scripted_example();
    terms.variables.push_back({"Notionl", "Number", std::string("1000")});
    const auto trade = scripted_trade_builder(terms).build();
    REQUIRE_FALSE(trade.has_value());
    CHECK(trade.error().term == "Notionl");
}

TEST_CASE("a_scripted_trade_that_states_a_variable_twice_names_it", scripted_tags) {
    auto terms = scripted_example();
    terms.variables.push_back({"Strike", "Number", std::string("110")});
    const auto trade = scripted_trade_builder(terms).build();
    REQUIRE_FALSE(trade.has_value());
    CHECK(trade.error().term == "Strike");
}

TEST_CASE("a_scripted_trade_with_no_library_inputs_is_refused", scripted_tags) {
    auto terms = scripted_example();
    terms.inputs.clear();
    const auto trade = scripted_trade_builder(terms).build();
    REQUIRE_FALSE(trade.has_value());
    CHECK(trade.error().term == "inputs");
}

TEST_CASE("a_double_digital_option_states_its_kinds_and_its_pair", scripted_tags) {
    const auto option =
        double_digital_option_builder({.expiry = 2027y / 1 / 5,
                                       .settlement = 2027y / 1 / 7,
                                       .binary_payout = scripted_amount("1000"),
                                       .binary_level_1 = scripted_amount("100"),
                                       .binary_level_2 = scripted_amount("120"),
                                       .kind_1 = digital_kind::call,
                                       .kind_2 = digital_kind::put,
                                       .position = option_position::short_option,
                                       .pay_currency = std::string("EUR"),
                                       .underlying_1 = scripted_equity(".SPX"),
                                       .underlying_2 = scripted_equity(".STOXX50E")})
            .build();
    REQUIRE(option.has_value());
    const auto& p = option->parameters;
    const auto value_of = [&](const char* name) {
        return std::ranges::find(
                   p, std::string(name), &ores::trading::domain::scripted_parameter::name)
            ->value.value_or("");
    };
    CHECK(value_of("Type1") == "Call");
    CHECK(value_of("Type2") == "Put");
    CHECK(value_of("Position") == "Short");
    REQUIRE(option->instrument_underlyings.size() == 2u);
    CHECK(option->instrument_underlyings[1].underlying_role == "underlying2");
}

TEST_CASE("an_autocallable_states_its_fixing_and_settlement_schedules", scripted_tags) {
    const auto note =
        autocallable_builder(
            {.notional = scripted_amount("1000000"),
             .determination_level = scripted_amount("70"),
             .trigger_level = scripted_amount("100"),
             .underlying = scripted_equity(".SPX"),
             .position = option_position::long_option,
             .pay_currency = std::string("EUR"),
             .fixing_dates = scripted_schedule{2026y / 1 / 5, 2029y / 1 / 5, "1Y"},
             .settlement_dates = scripted_schedule{2026y / 1 / 5, 2029y / 2 / 5, "1Y"},
             .accumulation_factors = std::vector{scripted_amount("0.05"), scripted_amount("0.1")},
             .cap = scripted_amount("0.15")})
            .build();
    REQUIRE(note.has_value());
    CHECK(note->instrument.identity.trade_type_code == "Autocallable_01");
    CHECK(note->schedules.size() == 2u);
    CHECK(note->parameter_values.size() == 2u);
    REQUIRE(note->instrument_underlyings.size() == 1u);
}

TEST_CASE("an_autocallable_whose_fixings_end_before_they_start_fails", scripted_tags) {
    const auto note =
        autocallable_builder(
            {.notional = scripted_amount("1000000"),
             .determination_level = scripted_amount("70"),
             .trigger_level = scripted_amount("100"),
             .underlying = scripted_equity(".SPX"),
             .position = option_position::long_option,
             .pay_currency = std::string("EUR"),
             .fixing_dates = scripted_schedule{2029y / 1 / 5, 2026y / 1 / 5, "1Y"},
             .settlement_dates = scripted_schedule{2026y / 1 / 5, 2029y / 2 / 5, "1Y"},
             .accumulation_factors = std::vector{scripted_amount("0.05")},
             .cap = scripted_amount("0.15")})
            .build();
    REQUIRE_FALSE(note.has_value());
}

namespace {

best_entry_option_terms spx_best_entry() {
    return {.underlying = scripted_equity(".SPX"),
            .currency = std::string("USD"),
            .notional = scripted_amount("1000000"),
            .multiplier = scripted_amount("1"),
            .strike = scripted_amount("0.85"),
            .cap = scripted_amount("0.06"),
            .trigger_level = scripted_amount("0.95"),
            .reset_minimum = scripted_amount("0.85"),
            .premium = scripted_amount("100"),
            .premium_date = 2026y / 1 / 7,
            .strike_date = 2026y / 1 / 5,
            .strike_observation_dates =
                std::vector<year_month_day>{2026y / 3 / 2, 2026y / 6 / 1, 2026y / 9 / 1},
            .expiry = 2027y / 1 / 5,
            .settlement = 2027y / 1 / 7,
            .position = option_position::long_option};
}

strike_resettable_option_terms spx_strike_resettable() {
    return {.underlying = scripted_equity(".SPX"),
            .currency = std::string("USD"),
            .kind = option_kind::put,
            .position = option_position::long_option,
            .quantity = scripted_amount("10000"),
            .strike = scripted_amount("3500"),
            .reset_strike = scripted_amount("3650"),
            .trigger = trigger_direction::up,
            .trigger_price = scripted_amount("4000"),
            .observation_dates =
                std::vector<year_month_day>{2026y / 4 / 27, 2026y / 5 / 25, 2026y / 6 / 22},
            .expiry = 2026y / 6 / 22,
            .settlement = 2026y / 6 / 26,
            .premium = scripted_amount("100"),
            .premium_date = 2026y / 6 / 26};
}

}

TEST_CASE("a_best_entry_option_states_its_terms_its_underlying_and_its_observation_dates",
          scripted_tags) {
    const auto built = best_entry_option_builder<scripted_family::fx>(spx_best_entry()).build();
    REQUIRE(built.has_value());
    CHECK(built->instrument.identity.trade_type_code == "FxBestEntryOption");
    REQUIRE(built->instrument_underlyings.size() == 1);
    CHECK(built->instrument_underlyings[0].underlying_role == "underlying");
    CHECK(built->instrument_underlyings[0].underlying_type == "FX");
    const auto trigger = std::ranges::find_if(
        built->parameters, [](const auto& p) { return p.name == "TriggerLevel"; });
    REQUIRE(trigger != built->parameters.end());
    CHECK(trigger->value == "0.95");
    REQUIRE(built->schedule_dates.size() == 3);
    CHECK(built->schedule_dates[0].schedule_role == "strikeobservationdates");
    CHECK(built->schedule_dates[2].schedule_date == 2026y / 9 / 1);
}

TEST_CASE("a_best_entry_option_refuses_an_observation_on_or_before_the_strike_date",
          scripted_tags) {
    auto terms = spx_best_entry();
    terms.strike_observation_dates = std::vector<year_month_day>{2026y / 1 / 5};
    const auto built = best_entry_option_builder<scripted_family::equity>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "strike_observation_dates");
}

TEST_CASE("a_strike_resettable_option_states_its_trigger_and_its_observation_dates",
          scripted_tags) {
    const auto built =
        strike_resettable_option_builder<scripted_family::equity>(spx_strike_resettable()).build();
    REQUIRE(built.has_value());
    CHECK(built->instrument.identity.trade_type_code == "EquityStrikeResettableOption");
    CHECK(built->instrument_underlyings[0].underlying_type == "Equity");
    const auto trigger = std::ranges::find_if(
        built->parameters, [](const auto& p) { return p.name == "TriggerType"; });
    REQUIRE(trigger != built->parameters.end());
    CHECK(trigger->value == "Up");
    REQUIRE(built->schedule_dates.size() == 3);
    CHECK(built->schedule_dates[0].schedule_role == "observationdates");
}

TEST_CASE("a_strike_resettable_option_refuses_dates_out_of_order_or_after_the_expiry",
          scripted_tags) {
    auto terms = spx_strike_resettable();
    terms.observation_dates = std::vector<year_month_day>{2026y / 5 / 25, 2026y / 4 / 27};
    auto built = strike_resettable_option_builder<scripted_family::fx>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "observation_dates");

    terms.observation_dates = std::vector<year_month_day>{2026y / 7 / 1};
    built = strike_resettable_option_builder<scripted_family::fx>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "observation_dates");
}

namespace {

window_barrier_option_terms spx_window_barrier() {
    return {.underlying = scripted_equity(".SPX"),
            .currency = std::string("USD"),
            .fixing_amount = scripted_amount("1000"),
            .strike = scripted_amount("4000"),
            .kind = option_kind::call,
            .position = option_position::long_option,
            .expiry = 2027y / 1 / 5,
            .window_start = 2026y / 1 / 5,
            .window_end = 2026y / 7 / 5,
            .barrier = scripted_barrier{.type = "DownAndOut", .level = scripted_amount("3600")}};
}

generic_barrier_option_terms spx_generic_barrier() {
    return {
        .underlying = scripted_equity(".SPX"),
        .pay_currency = std::string("USD"),
        .quantity = scripted_amount("1000"),
        .strike = scripted_amount("4000"),
        .kind = option_kind::call,
        .position = option_position::long_option,
        .expiry = 2027y / 1 / 5,
        .settlement = 2027y / 1 / 7,
        .monitoring_start = 2026y / 1 / 5,
        .barriers =
            std::vector<scripted_barrier>{{.type = "DownAndOut", .level = scripted_amount("3600")},
                                          {.type = "UpAndIn", .level = scripted_amount("4400")}},
        .kiko_type = std::string("KoAfterKi")};
}

}

TEST_CASE("a_window_barrier_option_states_its_window_its_option_and_its_barrier", scripted_tags) {
    const auto built =
        window_barrier_option_builder<scripted_family::fx>(spx_window_barrier()).build();
    REQUIRE(built.has_value());
    CHECK(built->instrument.identity.trade_type_code == "FxWindowBarrierOption");
    CHECK(built->instrument_underlyings[0].underlying_type == "FX");
    REQUIRE(built->options.size() == 1);
    CHECK(built->options[0].option_type == "Call");
    REQUIRE(built->barriers.size() == 1);
    CHECK(built->barriers[0].barrier_role == "barrier");
    CHECK(built->barriers[0].barrier_type == "DownAndOut");
    REQUIRE(built->barrier_levels.size() == 1);
    CHECK(built->barrier_levels[0].level == scripted_amount("3600"));
}

TEST_CASE("a_window_barrier_option_refuses_a_window_past_the_expiry_and_an_unknown_barrier",
          scripted_tags) {
    auto terms = spx_window_barrier();
    terms.window_end = 2027y / 2 / 1;
    auto built = window_barrier_option_builder<scripted_family::equity>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "window_end");

    terms = spx_window_barrier();
    terms.barrier = scripted_barrier{.type = "KnockOut", .level = scripted_amount("3600")};
    built = window_barrier_option_builder<scripted_family::equity>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "barrier");
}

TEST_CASE("a_generic_barrier_option_states_its_barriers_their_schedule_and_its_kiko_type",
          scripted_tags) {
    const auto built = equity_generic_barrier_option_builder(spx_generic_barrier()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->barriers.size() == 2);
    CHECK(built->barriers[1].barrier_role == "barrier");
    CHECK(built->barriers[1].sequence_number == 2);
    CHECK(built->barriers[1].barrier_type == "UpAndIn");
    REQUIRE(built->schedules.size() == 2);
    const auto monitoring = std::ranges::find_if(
        built->schedules, [](const auto& s) { return s.schedule_role == "barrier_schedule"; });
    REQUIRE(monitoring != built->schedules.end());
    CHECK(monitoring->tenor == "1D");
    const auto kiko = std::ranges::find_if(
        built->parameters, [](const auto& p) { return p.name == "Barriers.KikoType"; });
    REQUIRE(kiko != built->parameters.end());
    CHECK(kiko->value == "KoAfterKi");
}

TEST_CASE("a_generic_barrier_option_refuses_no_barrier_and_an_unknown_kiko_type", scripted_tags) {
    auto terms = spx_generic_barrier();
    terms.barriers = std::vector<scripted_barrier>{};
    auto built = equity_generic_barrier_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "barriers");

    terms = spx_generic_barrier();
    terms.kiko_type = std::string("Sometimes");
    built = equity_generic_barrier_option_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "kiko_type");
}

namespace {

european_option_barrier_terms spx_on_ndx_barrier() {
    return {.family = scripted_family::equity,
            .option_underlying = scripted_equity(".SPX"),
            .barrier_underlying = scripted_equity(".NDX"),
            .quantity = scripted_amount("1000"),
            .kind = option_kind::call,
            .position = option_position::long_option,
            .strike = scripted_amount("4000"),
            .premium_amount = scripted_amount("0"),
            .premium_currency = std::string("USD"),
            .premium_date = 2026y / 1 / 7,
            .expiry = 2027y / 1 / 5,
            .barrier = scripted_barrier{.type = "DownAndOut", .level = scripted_amount("15000")},
            .settlement = 2027y / 1 / 7,
            .pay_currency = std::string("USD")};
}

fx_worst_of_basket_swap_terms spx_and_ndx_worst_of() {
    return {
        .members =
            std::vector<worst_of_member>{
                {.underlying = scripted_equity(".SPX"), .initial_price = scripted_amount("1.1")},
                {.underlying = scripted_equity(".NDX"), .initial_price = scripted_amount("0.8")}},
        .currency = std::string("USD"),
        .quantity = scripted_amount("1000000"),
        .initial_fixed_rate = scripted_amount("0.02"),
        .period_dates = std::vector<year_month_day>{2026y / 1 / 5, 2026y / 7 / 6, 2027y / 1 / 5},
        .pay_dates = std::vector<year_month_day>{2026y / 7 / 8, 2027y / 1 / 7},
        .floating_index = std::string("USD-SOFR"),
        .floating_spread = scripted_amount("0"),
        .floating_day_count = std::string("A360"),
        .knock_out_levels = std::vector<decimal>{scripted_amount("1"), scripted_amount("1")},
        .fixed_trigger_levels =
            std::vector<decimal>{scripted_amount("0.75"), scripted_amount("0.75")},
        .fixed_rate = scripted_amount("0.05"),
        .knock_in_level = scripted_amount("0.75"),
        .strike = scripted_amount("1"),
        .position = option_position::long_option};
}

}

TEST_CASE("a_european_option_barrier_names_its_option_and_its_barrier_underlying", scripted_tags) {
    const auto built = european_option_barrier_builder(spx_on_ndx_barrier()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->instrument_underlyings.size() == 2);
    CHECK(built->instrument_underlyings[0].underlying_role == "optionunderlying");
    CHECK(built->instrument_underlyings[1].underlying_role == "barrierunderlying");
    CHECK(built->instrument_underlyings[0].underlying_type == "Equity");
    const auto style = std::ranges::find_if(built->parameters,
                                            [](const auto& p) { return p.name == "BarrierStyle"; });
    REQUIRE(style != built->parameters.end());
    CHECK(style->value == "European");
}

TEST_CASE("a_european_option_barrier_refuses_a_settlement_before_its_expiry", scripted_tags) {
    auto terms = spx_on_ndx_barrier();
    terms.settlement = 2027y / 1 / 1;
    const auto built = european_option_barrier_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "settlement");
}

TEST_CASE("an_fx_worst_of_basket_swap_states_its_schedules_and_its_level_lists", scripted_tags) {
    const auto built = fx_worst_of_basket_swap_builder(spx_and_ndx_worst_of()).build();
    REQUIRE(built.has_value());
    REQUIRE(built->schedules.size() == 3);
    const auto fixing = std::ranges::find_if(built->schedules, [](const auto& s) {
        return s.schedule_role == "floatingfixingschedule";
    });
    REQUIRE(fixing != built->schedules.end());
    CHECK(fixing->schedule_kind == "derived");
    CHECK(fixing->base_schedule == "FloatingPeriodSchedule");
    CHECK(built->schedule_dates.size() == 5u);
    const auto levels = std::ranges::find_if(
        built->parameters, [](const auto& p) { return p.name == "KnockOutLevels"; });
    REQUIRE(levels != built->parameters.end());
    CHECK(levels->shape == "vector");
}

TEST_CASE("an_fx_worst_of_basket_swap_refuses_level_lists_that_miss_a_period", scripted_tags) {
    auto terms = spx_and_ndx_worst_of();
    terms.knock_out_levels = std::vector<decimal>{scripted_amount("1")};
    auto built = fx_worst_of_basket_swap_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "knock_out_levels");

    terms = spx_and_ndx_worst_of();
    terms.pay_dates = std::vector<year_month_day>{2026y / 7 / 1, 2027y / 1 / 7};
    built = fx_worst_of_basket_swap_builder(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "pay_dates");
}
