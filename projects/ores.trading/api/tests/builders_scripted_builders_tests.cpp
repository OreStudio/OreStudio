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

basket_option_terms spx_and_ndx(std::string payoff_type) {
    return {.currency = std::string("USD"),
            .notional = scripted_amount("1"),
            .strike = scripted_amount("20000"),
            .members = std::vector<basket_option_member>{{.underlying = scripted_equity(".SPX"),
                                                          .currency = std::nullopt,
                                                          .weight = scripted_amount("1")},
                                                         {.underlying = scripted_equity(".NDX"),
                                                          .currency = std::nullopt,
                                                          .weight = scripted_amount("0.5")}},
            .payoff_type = std::move(payoff_type),
            .kind = option_kind::call,
            .position = option_position::long_option,
            .expiry = 2027y / 1 / 5,
            .settlement = 2027y / 1 / 7};
}

}

TEST_CASE("a_basket_option_states_its_members_its_option_block_and_its_exercise_date",
          scripted_tags) {
    const auto built =
        basket_option_builder<basket_product::equity_basket>(spx_and_ndx("Vanilla")).build();
    REQUIRE(built.has_value());
    CHECK(built->instrument.identity.trade_type_code == "EquityBasketOption");
    REQUIRE(built->instrument_underlyings.size() == 2);
    CHECK(built->instrument_underlyings[0].underlying_type == "Equity");
    CHECK(built->instrument_underlyings[0].sequence_number == 1);
    CHECK(built->instrument_underlyings[1].sequence_number == 2);
    CHECK(built->instrument_underlyings[1].weight == 0.5);
    REQUIRE(built->options.size() == 1);
    CHECK(built->options[0].payoff_type == "Vanilla");
    CHECK(built->options[0].option_type == "Call");
    REQUIRE(built->schedule_dates.size() == 1);
    CHECK(built->schedule_dates[0].schedule_role == "exercise_dates");
    CHECK(built->schedule_dates[0].schedule_date == 2027y / 1 / 5);
}

TEST_CASE("a_rainbow_on_a_best_of_payoff_states_no_option_type", scripted_tags) {
    const auto built =
        basket_option_builder<basket_product::fx_rainbow>(spx_and_ndx("BestOfAssetOrCash")).build();
    REQUIRE(built.has_value());
    CHECK(built->instrument.identity.trade_type_code == "FxRainbowOption");
    CHECK(built->instrument_underlyings[0].underlying_type == "FX");
    REQUIRE(built->options.size() == 1);
    CHECK_FALSE(built->options[0].option_type.has_value());
}

TEST_CASE("a_basket_option_refuses_a_payoff_of_the_other_product", scripted_tags) {
    const auto basket =
        basket_option_builder<basket_product::equity_basket>(spx_and_ndx("MaxRainbow")).build();
    REQUIRE_FALSE(basket.has_value());
    CHECK(basket.error().term == "payoff_type");
    const auto rainbow =
        basket_option_builder<basket_product::equity_rainbow>(spx_and_ndx("Vanilla")).build();
    REQUIRE_FALSE(rainbow.has_value());
    CHECK(rainbow.error().term == "payoff_type");
}

TEST_CASE("a_basket_option_refuses_one_member_and_a_member_named_twice", scripted_tags) {
    auto terms = spx_and_ndx("Vanilla");
    auto members = terms.members.get();
    members.pop_back();
    terms.members = members;
    auto built = basket_option_builder<basket_product::equity_basket>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "members");

    terms = spx_and_ndx("Vanilla");
    members = terms.members.get();
    members[1].underlying = scripted_equity(".SPX");
    terms.members = members;
    built = basket_option_builder<basket_product::equity_basket>(terms).build();
    REQUIRE_FALSE(built.has_value());
    CHECK(built.error().term == "members");
}
