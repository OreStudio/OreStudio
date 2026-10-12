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
// either runs an analytic the report did not ask for or names an input file
// the export does not write.

using namespace ores::reporting::service;

namespace {

const std::string tags("[service][run_document_generator]");

run_document_inputs inputs() {
    run_document_inputs in;
    in.asof_date = "2026-10-09";
    in.risk.base_currency = "GBP";
    in.risk.npv_enabled = 0;
    in.risk.cashflow_enabled = 0;
    in.risk.curves_enabled = 1;
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

TEST_CASE("a risk configuration with no switch set is refused", tags) {
    auto in = inputs();
    in.risk.curves_enabled = 0;
    CHECK_THROWS_AS(generate_run_document(in), std::invalid_argument);
}

TEST_CASE("the portfolio details switch runs the portfolio details analytic", tags) {
    auto in = inputs();
    in.risk.curves_enabled = 0;
    in.risk.portfolio_details_enabled = 1;
    const std::vector<std::string> expected{"portfolioDetails"};
    CHECK(codes(generate_run_document(in)) == expected);
}

TEST_CASE("each switch set adds its analytic, in run order", tags) {
    auto in = inputs();
    in.risk.curves_enabled = 0;
    in.risk.xva_enabled = 1;
    in.risk.npv_enabled = 1;
    in.risk.sensitivity_enabled = 1;
    const std::vector<std::string> expected{"npv", "sensitivity", "xva"};
    CHECK(codes(generate_run_document(in)) == expected);
}

TEST_CASE("the initial margin switch runs the initial margin analytic", tags) {
    auto in = inputs();
    in.risk.curves_enabled = 0;
    in.risk.initial_margin_enabled = 1;
    const std::vector<std::string> expected{"initialMargin"};
    CHECK(codes(generate_run_document(in)) == expected);
}

TEST_CASE("every analytic is active and numbered from one", tags) {
    auto in = inputs();
    in.risk.curves_enabled = 0;
    in.risk.npv_enabled = 1;
    in.risk.cashflow_enabled = 1;
    const auto d = generate_run_document(in);
    REQUIRE(d.analytics.size() == 2);
    CHECK(d.analytics[0].analytic.active == "Y");
    CHECK(d.analytics[0].analytic.display_order == 1);
    CHECK(d.analytics[1].analytic.display_order == 2);
}

TEST_CASE("each analytic switch runs the analytic type of its name", tags) {
    struct row {
        int ores::reporting::domain::risk_report_config::*enabled;
        const char* type;
    };
    const row rows[] = {
        {&ores::reporting::domain::risk_report_config::saccr_enabled, "saccr"},
        {&ores::reporting::domain::risk_report_config::calibration_enabled, "calibration"},
        {&ores::reporting::domain::risk_report_config::credit_curves_enabled, "creditcurves"},
        {&ores::reporting::domain::risk_report_config::historical_var_enabled,
         "historicalSimulationVar"},
        {&ores::reporting::domain::risk_report_config::pnl_explain_enabled, "pnlExplain"},
    };
    for (const auto& r : rows) {
        auto in = inputs();
        in.risk.curves_enabled = 0;
        in.risk.*r.enabled = 1;
        INFO(r.type);
        const std::vector<std::string> expected{r.type};
        CHECK(codes(generate_run_document(in)) == expected);
    }
}

TEST_CASE("the value-at-risk switch runs the parametric value-at-risk analytic", tags) {
    auto in = inputs();
    in.risk.curves_enabled = 0;
    in.risk.parametric_var_enabled = 1;
    const std::vector<std::string> expected{"parametricVar"};
    CHECK(codes(generate_run_document(in)) == expected);
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

TEST_CASE("the run prices against the named market configuration", tags) {
    auto in = inputs();
    in.market_configuration = "overnight";
    const auto d = generate_run_document(in);
    REQUIRE(d.market_bindings.size() == 1);
    CHECK(d.market_bindings[0].role == "pricing");
    CHECK(d.market_bindings[0].configuration_name == "overnight");
}

TEST_CASE("a run with no as-of date names none", tags) {
    auto in = inputs();
    in.asof_date.clear();
    CHECK_FALSE(generate_run_document(in).setup.asof_date);
}
