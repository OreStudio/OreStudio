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
#include "ores.ore.core/results/child_row_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.testing/project_root.hpp"
#include "ores.testing/test_files.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>

namespace {

const std::string child_tags("[ore][results][child_row_reader]");

using ores::ore::results::family_named;
using ores::ore::results::read_calibration_flows;
using ores::ore::results::read_flows;
using ores::ore::results::read_stressed_flows;

std::string child_sample_text(const std::string& sample_file) {
    const auto relative = std::filesystem::path(sample_file).lexically_relative("external/ore");
    return ores::testing::read_file(ores::testing::ore_path(relative.string()));
}

// A fixture states a case the ORE corpus does not ship, so it lives beside the
// suite rather than in the corpus.
std::string child_fixture_text(const std::string& name) {
    return ores::testing::read_file(
        ores::testing::project_root::resolve("projects/ores.ore/core/tests/fixtures/" + name));
}

std::string child_family_sample(const std::string& code) {
    const auto* family = family_named(code);
    REQUIRE(family != nullptr);
    return child_sample_text(family->sample_file);
}

}

TEST_CASE("a flow row keeps its trade, leg, cashflow number and pay date", child_tags) {
    const auto outcome = read_flows(child_family_sample("flows"), "flows.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE_FALSE(outcome.rows.empty());

    const auto& first = outcome.rows.front();
    CHECK(first.trade_id == "Swap_20y");
    CHECK(first.leg_no == "0");
    CHECK(first.cashflow_no == "1");
    CHECK(first.pay_date == "2017-03-01");
    CHECK(first.currency == "EUR");
    REQUIRE(first.amount.has_value());
    CHECK(first.amount.value() == 212916.6667);
}

TEST_CASE("a flow the engine states no fixing for keeps an empty figure", child_tags) {
    const auto outcome = read_flows(child_family_sample("flows"), "flows.csv");

    REQUIRE_FALSE(outcome.rows.empty());
    // The first row's fixing is the engine's null string, so no figure is
    // carried and no refusal is made.
    CHECK(outcome.refusals.empty());
}

TEST_CASE("a flow row without its pay date is refused", child_tags) {
    const auto outcome =
        read_flows(child_fixture_text("flows_missing_pay_date.csv"), "flows_missing_pay_date.csv");

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("pay date") != std::string::npos);
    CHECK(outcome.rows.size() == 1);
}

TEST_CASE("a flow figure that is not a number is refused, and names its heading", child_tags) {
    const auto outcome = read_flows(child_fixture_text("flows_figure_not_a_number.csv"),
                                    "flows_figure_not_a_number.csv");

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("Amount") != std::string::npos);
}

TEST_CASE("a stressed flow states each figure in three parts", child_tags) {
    const auto outcome =
        read_stressed_flows(child_family_sample("stresstest_flows"), "stresstest_flows.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE_FALSE(outcome.rows.empty());

    const auto& first = outcome.rows.front();
    CHECK(first.trade_id == "Trade1");
    CHECK(first.scenario_label == "BASE");
    REQUIRE(first.amount_base.has_value());
    REQUIRE(first.amount_scenario.has_value());
    REQUIRE(first.amount_difference.has_value());
    CHECK(first.amount_base.value() == 733286.315738);
    CHECK(first.amount_difference.value() == 0.0);
    // The corpus states the floor and cap volatilities without a difference, so
    // the difference is optional and their base is still carried.
}

TEST_CASE("a stressed flow states a base and a scenario, and the difference when it has one",
          child_tags) {
    const auto outcome =
        read_stressed_flows(child_fixture_text("stresstest_flows_partial_triple.csv"),
                            "stresstest_flows_partial_triple.csv");

    REQUIRE_FALSE(outcome.refusals.empty());
    CHECK(outcome.refusals.front().find("Amount_Base") != std::string::npos);
    CHECK(outcome.refusals.front().find("Amount_Scen") != std::string::npos);
    CHECK(outcome.rows.empty());
}

TEST_CASE("a calibration flow keeps the build it belongs to", child_tags) {
    const auto outcome =
        read_calibration_flows(child_family_sample("todaysmarketcalibration_cashflows"),
                               "todaysmarketcalibration_cashflows.csv");

    REQUIRE(outcome.refusals.empty());
    REQUIRE_FALSE(outcome.rows.empty());

    const auto& first = outcome.rows.front();
    CHECK(first.market_object_type == "yieldCurve");
    CHECK(first.market_object_id == "USD1D");
    CHECK(first.pillar_date == "2016-02-08");
    CHECK(first.cashflow_no == "1");
    CHECK(first.pay_date == "2016-02-08");
    REQUIRE(first.amount.has_value());
    CHECK(first.amount.value() == 37.3333333332);
}
