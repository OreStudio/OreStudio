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
