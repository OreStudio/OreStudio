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
#include "ores.marketdata.core/repository/market_calibration_value_repository.hpp"
#include "ores.marketdata.core/repository/market_input_value_repository.hpp"
#include "ores.marketdata.core/repository/market_scenario_value_repository.hpp"
#include "ores.marketdata.core/service/market_state_reader.hpp"
#include "ores.testing/database_helper.hpp"
#include <boost/uuid/uuid_generators.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string value_tags("[repository][market_state_value]");

using ores::marketdata::repository::market_calibration_value_repository;
using ores::marketdata::repository::market_input_value_repository;
using ores::marketdata::repository::market_scenario_value_repository;
using ores::marketdata::service::market_state_file;
using ores::marketdata::service::market_state_run;
using ores::marketdata::service::read_market_state;
using ores::testing::database_helper;

market_state_run a_stored_run() {
    return market_state_run{.party_id = boost::uuids::random_generator()(),
                            .report_instance_id = boost::uuids::random_generator()(),
                            .report_definition_id = boost::uuids::random_generator()(),
                            .as_of = std::chrono::sys_days{std::chrono::year{2026} / 10 / 31}};
}

}

TEST_CASE("every value a scenario file states for one date is stored", value_tags) {
    database_helper h;
    const auto run = a_stored_run();

    // Three scenarios on one date state the same factors, so only the line tells
    // one value of a factor from another.
    const auto outcome = read_market_state(
        {market_state_file{.result_id = boost::uuids::random_generator()(),
                           .name = "stress_scenarios.csv",
                           .body = "#Date,Scenario,Numeraire,DiscountCurve/USD/0\n"
                                   "2026-10-31,BASE,1.0,0.99\n"
                                   "2026-10-31,UP,1.0,1.01\n"
                                   "2026-10-31,DOWN,1.0,0.97\n"}},
        run);
    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.scenario_values.size() == 6);

    market_scenario_value_repository repo;
    repo.insert(h.context(), outcome.scenario_values);

    const auto rows = repo.read_latest(h.context());
    const auto stored = std::ranges::count_if(
        rows, [&](const auto& one) { return one.report_instance_id == run.report_instance_id; });
    CHECK(stored == 6);

    const auto up = std::ranges::find_if(rows, [&](const auto& one) {
        return one.report_instance_id == run.report_instance_id && one.scenario == "UP" &&
               one.factor == "DiscountCurve/USD/0";
    });
    REQUIRE(up != rows.end());
    CHECK(up->value == 1.01);
    CHECK(up->line == 3);
}

TEST_CASE("a calibration reads back with its text and its figure", value_tags) {
    database_helper h;
    const auto run = a_stored_run();

    const auto outcome = read_market_state(
        {market_state_file{
            .result_id = boost::uuids::random_generator()(),
            .name = "todaysmarketcalibration.csv",
            .body = "#MarketObjectType,MarketObjectId,ResultId,ResultKey1,ResultKey2,ResultKey3,"
                    "ResultType,ResultValue\n"
                    "yieldCurve,USD1D,dayCounter,,,,string,Actual/365 (Fixed)\n"
                    "yieldCurve,USD1D,time,2016-02-08,MM/RATE/USD/0D/1D,,double,0.00821918\n"}},
        run);
    REQUIRE(outcome.refusals.empty());

    market_calibration_value_repository repo;
    repo.insert(h.context(), outcome.calibration_values);

    const auto rows = repo.read_latest(h.context());
    const auto text = std::ranges::find_if(rows, [&](const auto& one) {
        return one.report_instance_id == run.report_instance_id && one.line == 2;
    });
    REQUIRE(text != rows.end());
    CHECK(text->value == "Actual/365 (Fixed)");
    CHECK_FALSE(text->figure.has_value());

    const auto pillar = std::ranges::find_if(rows, [&](const auto& one) {
        return one.report_instance_id == run.report_instance_id && one.line == 3;
    });
    REQUIRE(pillar != rows.end());
    CHECK(pillar->result_key_2 == "MM/RATE/USD/0D/1D");
    REQUIRE(pillar->figure.has_value());
    CHECK(*pillar->figure == 0.00821918);
}

TEST_CASE("two quotes of one datum on one date from two lines are both stored", value_tags) {
    database_helper h;
    const auto run = a_stored_run();

    const auto outcome = read_market_state(
        {market_state_file{.result_id = boost::uuids::random_generator()(),
                           .name = "fixings.csv",
                           .body = "#fixingDate,fixingId,fixingValue\n"
                                   "2026-10-30,EQ-SP5,2244.2\n"
                                   "2026-10-30,EQ-SP5,2244.3\n"}},
        run);
    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.input_values.size() == 2);

    market_input_value_repository repo;
    repo.insert(h.context(), outcome.input_values);

    const auto rows = repo.read_latest(h.context());
    const auto stored = std::ranges::count_if(
        rows, [&](const auto& one) { return one.report_instance_id == run.report_instance_id; });
    CHECK(stored == 2);
}
