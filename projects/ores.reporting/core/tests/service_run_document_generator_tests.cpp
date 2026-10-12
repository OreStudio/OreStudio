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
#include "ores.reporting.core/service/run_document_generator.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

// The decision these pin: what an exported run asks ORE to do. A wrong answer
// either runs an analytic the configuration did not ask for, names an input file
// the export does not write, or loses a parameter on the way.

using namespace ores::reporting::service;

namespace {

const std::string tags("[service][run_document_generator]");

ores::reporting::messaging::run_analytic
analytic(const std::string& type, int order, const std::string& active = "Y") {
    ores::reporting::messaging::run_analytic a;
    a.analytic.analytic_type_code = type;
    a.analytic.display_order = order;
    a.analytic.active = active;
    return a;
}

run_document_inputs inputs() {
    run_document_inputs in;
    in.asof_date = "2026-10-09";
    in.risk.base_currency = "GBP";
    in.analytics = {analytic("curves", 1)};
    in.bound_types = {"conventions", "curve_configuration", "todays_market", "pricing_engines"};
    return in;
}

std::vector<std::string> codes(const ores::reporting::messaging::run_document& d) {
    std::vector<std::string> r;
    for (const auto& a : d.analytics)
        r.push_back(a.analytic.analytic_type_code);
    return r;
}

}

TEST_CASE("a configuration with no analytic is refused", tags) {
    auto in = inputs();
    in.analytics.clear();
    CHECK_THROWS_AS(generate_run_document(in), std::invalid_argument);
}

TEST_CASE("a configuration whose analytics are all off is refused", tags) {
    auto in = inputs();
    in.analytics = {analytic("npv", 1, "N")};
    CHECK_THROWS_AS(generate_run_document(in), std::invalid_argument);
}

TEST_CASE("the analytics of the configuration are the analytics of the run, in order", tags) {
    auto in = inputs();
    in.analytics = {analytic("portfolioDetails", 1),
                    analytic("npv", 2),
                    analytic("saccr", 3),
                    analytic("historicalSimulationVar", 4)};
    const std::vector<std::string> expected{
        "portfolioDetails", "npv", "saccr", "historicalSimulationVar"};
    CHECK(codes(generate_run_document(in)) == expected);
}

TEST_CASE("an analytic that is off stays in the run with its flag", tags) {
    auto in = inputs();
    in.analytics = {analytic("npv", 1), analytic("cashflow", 2, "N")};
    const auto d = generate_run_document(in);
    REQUIRE(d.analytics.size() == 2);
    CHECK(d.analytics[1].analytic.active == "N");
}

TEST_CASE("the parameters of an analytic reach the run with their values and order", tags) {
    auto in = inputs();
    auto pfe = analytic("pfe", 1);
    pfe.parameters = {{"quantile", "0.95", 1}, {"calculationType", "Symmetric", 2}};
    in.analytics = {analytic("simulation", 1), pfe};
    const auto d = generate_run_document(in);
    REQUIRE(d.analytics.size() == 2);
    REQUIRE(d.analytics[1].parameters.size() == 2);
    CHECK(d.analytics[1].parameters[0].name == "quantile");
    CHECK(d.analytics[1].parameters[0].value == "0.95");
    CHECK(d.analytics[1].parameters[1].name == "calculationType");
}

TEST_CASE("the setup names a file for each bound type and none for the others", tags) {
    auto in = inputs();
    in.bound_types = {"curve_configuration", "todays_market"};
    const auto& s = generate_run_document(in).setup;
    CHECK(s.curve_config_file == "curveconfig.xml");
    CHECK(s.market_config_file == "todaysmarket.xml");
    CHECK_FALSE(s.conventions_file);
    CHECK_FALSE(s.pricing_engines_file);
}

TEST_CASE("the setup carries the as-of date and the base currency", tags) {
    const auto& s = generate_run_document(inputs()).setup;
    CHECK(s.asof_date == "2026-10-09");
    CHECK(s.base_currency == "GBP");
}

TEST_CASE("a run with no as-of date names none", tags) {
    auto in = inputs();
    in.asof_date.clear();
    CHECK_FALSE(generate_run_document(in).setup.asof_date);
}

TEST_CASE("the run prices against the named market configuration", tags) {
    auto in = inputs();
    in.market_configuration = "overnight";
    const auto d = generate_run_document(in);
    REQUIRE(d.market_bindings.size() == 1);
    CHECK(d.market_bindings[0].role == "pricing");
    CHECK(d.market_bindings[0].configuration_name == "overnight");
}
