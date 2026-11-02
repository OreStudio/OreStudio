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
#include "ores.marketdata.core/service/market_state_reader.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

namespace {

const std::string market_state_tags("[marketdata][market_state_reader]");

using ores::marketdata::service::market_state_file;
using ores::marketdata::service::market_state_run;
using ores::marketdata::service::read_market_state;

const auto priced_at = std::chrono::sys_days{std::chrono::year{2026} / 10 / 31};

market_state_run the_run() {
    return market_state_run{.party_id = boost::uuids::random_generator()(),
                            .report_instance_id = boost::uuids::random_generator()(),
                            .report_definition_id = boost::uuids::random_generator()(),
                            .as_of = priced_at};
}

std::string two_date_scenario_file() {
    return "#Date,Scenario,Numeraire,DiscountCurve/USD/0,DiscountCurve/USD/1\n"
           "2026-10-31,BASE,1.0,0.99,0.98\n"
           "2026-10-31,UP,1.0,1.01,1.00\n"
           "2026-11-30,BASE,1.0,0.98,0.97\n";
}

}

TEST_CASE("a scenario file is stored as one value for each factor of each line",
          market_state_tags) {
    const auto run = the_run();
    const auto result = boost::uuids::random_generator()();
    const auto outcome = read_market_state({market_state_file{.result_id = result,
                                                              .name = "stress_scenarios.csv",
                                                              .body = two_date_scenario_file()}},
                                           run);

    REQUIRE(outcome.refusals.empty());
    CHECK(outcome.left_alone.empty());
    CHECK(outcome.families == std::vector<std::string>{"stress_scenarios"});
    CHECK(outcome.calibration_values.empty());

    // Three lines, three factors each.
    REQUIRE(outcome.scenario_values.size() == 9);

    const auto found = std::ranges::find_if(outcome.scenario_values, [](const auto& one) {
        return one.line == 3 && one.factor == "DiscountCurve/USD/0";
    });
    REQUIRE(found != outcome.scenario_values.end());
    CHECK(found->scenario == "UP");
    CHECK(found->value == 1.01);
    CHECK(found->family == "stress_scenarios");
    CHECK(found->source == "stress_scenarios.csv");
    CHECK(found->result_id == result);
    CHECK(found->party_id == run.party_id);
    CHECK(found->report_instance_id == run.report_instance_id);
    CHECK(found->report_definition_id == run.report_definition_id);
    CHECK(found->as_of == std::chrono::sys_days{std::chrono::year{2026} / 10 / 31});

    // The line's own date is the observation, not the run's.
    const auto later = std::ranges::find_if(outcome.scenario_values,
                                            [](const auto& one) { return one.line == 4; });
    REQUIRE(later != outcome.scenario_values.end());
    CHECK(later->as_of == std::chrono::sys_days{std::chrono::year{2026} / 11 / 30});
}

TEST_CASE("a calibration is stored as one value for each line, text kept as text",
          market_state_tags) {
    const auto run = the_run();
    const auto outcome = read_market_state(
        {market_state_file{
            .name = "todaysmarketcalibration.csv",
            .body = "#MarketObjectType,MarketObjectId,ResultId,ResultKey1,ResultKey2,ResultKey3,"
                    "ResultType,ResultValue\n"
                    "yieldCurve,USD1D,currency,,,,string,USD\n"
                    "yieldCurve,USD1D,time,2016-02-08,MM/RATE/USD/0D/1D,,double,0.00821918\n"}},
        run);

    REQUIRE(outcome.refusals.empty());
    CHECK(outcome.scenario_values.empty());
    REQUIRE(outcome.calibration_values.size() == 2);

    const auto& text = outcome.calibration_values[0];
    CHECK(text.market_object_type == "yieldCurve");
    CHECK(text.market_object_id == "USD1D");
    CHECK(text.result_name == "currency");
    CHECK(text.value == "USD");
    CHECK_FALSE(text.figure.has_value());
    CHECK(text.line == 2);
    CHECK(text.as_of == priced_at);

    const auto& pillar = outcome.calibration_values[1];
    CHECK(pillar.result_key_1 == "2016-02-08");
    CHECK(pillar.result_key_2 == "MM/RATE/USD/0D/1D");
    REQUIRE(pillar.figure.has_value());
    CHECK(*pillar.figure == 0.00821918);
    CHECK(pillar.line == 3);
}

TEST_CASE("a run's results are left alone rather than stored as market state", market_state_tags) {
    const auto outcome = read_market_state(
        {market_state_file{.name = "npv.csv", .body = "#TradeId,NPV\nT1,1.0\n"},
         market_state_file{.name = "netcube.csv", .body = "#Id,NettingSet\n"},
         market_state_file{.name = "something-else.txt", .body = "not a table\n"}},
        the_run());

    CHECK(outcome.refusals.empty());
    CHECK(outcome.scenario_values.empty());
    CHECK(outcome.calibration_values.empty());
    CHECK(outcome.families.empty());
    CHECK(outcome.left_alone ==
          std::vector<std::string>{"npv.csv", "netcube.csv", "something-else.txt"});
}

TEST_CASE("a market-state file with no heading stops the run's state", market_state_tags) {
    const auto outcome = read_market_state(
        {market_state_file{.name = "stress_scenarios.csv", .body = ""}}, the_run());

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("stress_scenarios") != std::string::npos);
    CHECK(outcome.scenario_values.empty());
}

TEST_CASE("a scenario line whose date is not a date stops the run's state", market_state_tags) {
    const auto outcome = read_market_state(
        {market_state_file{.name = "stress_scenarios.csv",
                           .body = "#Date,Scenario,Numeraire\nsoon,BASE,1.0\n"}},
        the_run());

    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals.front().find("line 2") != std::string::npos);
}

TEST_CASE("a scenario file refused part way leaves no rows and names no family",
          market_state_tags) {
    const auto outcome = read_market_state(
        {market_state_file{.name = "stress_scenarios.csv",
                           .body = "#Date,Scenario,Numeraire\n"
                                   "2026-10-31,BASE,1.0\n"
                                   "soon,UP,1.0\n"}},
        the_run());

    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals.front().find("line 3") != std::string::npos);
    CHECK(outcome.scenario_values.empty());
    CHECK(outcome.families.empty());
}

TEST_CASE("each result of a run keeps its own file of the same name", market_state_tags) {
    const auto first = boost::uuids::random_generator()();
    const auto second = boost::uuids::random_generator()();
    const auto outcome = read_market_state({market_state_file{.result_id = first,
                                                              .name = "stress_scenarios.csv",
                                                              .body = two_date_scenario_file()},
                                            market_state_file{.result_id = second,
                                                              .name = "stress_scenarios.csv",
                                                              .body = two_date_scenario_file()}},
                                           the_run());

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.scenario_values.size() == 18);
    CHECK(std::ranges::count_if(outcome.scenario_values,
                                [&](const auto& one) { return one.result_id == first; }) == 9);
    CHECK(std::ranges::count_if(outcome.scenario_values,
                                [&](const auto& one) { return one.result_id == second; }) == 9);
}

TEST_CASE("a result that states the same file twice is refused", market_state_tags) {
    const auto result = boost::uuids::random_generator()();
    const auto outcome = read_market_state({market_state_file{.result_id = result,
                                                              .name = "stress_scenarios.csv",
                                                              .body = two_date_scenario_file()},
                                            market_state_file{.result_id = result,
                                                              .name = "stress_scenarios.csv",
                                                              .body = two_date_scenario_file()}},
                                           the_run());

    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals.front().find("twice") != std::string::npos);
}
