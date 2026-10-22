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
#include <optional>
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

ores::reporting::domain::file_type setup_file(const std::string& parameter,
                                              const std::string& configuration_type,
                                              const std::string& name) {
    ores::reporting::domain::file_type t;
    t.code = "setup." + parameter;
    t.ore_parameter = parameter;
    t.kind = "input_document";
    t.configuration_type_code = configuration_type;
    t.name_pattern = name;
    return t;
}

ores::reporting::domain::file_type setup_data_file(const std::string& parameter,
                                                   const std::string& name) {
    ores::reporting::domain::file_type t;
    t.code = "setup." + parameter;
    t.ore_parameter = parameter;
    t.kind = "data_file";
    t.direction = "input";
    t.name_pattern = name;
    return t;
}

ores::reporting::domain::file_type analytic_file(const std::string& analytic,
                                                 const std::string& parameter,
                                                 const std::string& kind,
                                                 const std::string& name,
                                                 const std::string& writer = "") {
    ores::reporting::domain::file_type t;
    t.code = analytic + "." + parameter;
    t.analytic_type_code = analytic;
    t.ore_parameter = parameter;
    t.kind = kind;
    t.name_pattern = name;
    t.is_default = true;
    if (!writer.empty())
        t.produced_by_file_type_code = writer;
    return t;
}

std::vector<ores::reporting::domain::file_type> file_types() {
    return {setup_file("conventionsFile", "conventions", "conventions.xml"),
            setup_file("curveConfigFile", "curve_configuration", "curveconfig.xml"),
            setup_file("marketConfigFile", "todays_market", "todaysmarket.xml"),
            setup_file("pricingEnginesFile", "pricing_engines", "pricingengine.xml"),
            analytic_file("npv", "outputFileName", "output_file", "npv.csv"),
            analytic_file("sensitivity", "sensitivityOutputFile", "output_file", "sensitivity.csv"),
            analytic_file("parametricVar",
                          "sensitivityInputFile",
                          "intermediate_file",
                          "sensitivity.csv",
                          "sensitivity.sensitivityOutputFile")};
}

const ores::reporting::messaging::run_parameter*
parameter_of(const ores::reporting::messaging::run_analytic& a, const std::string& name) {
    for (const auto& p : a.parameters)
        if (p.name == name)
            return &p;
    return nullptr;
}

run_document_inputs inputs() {
    run_document_inputs in;
    in.risk.market_data_type = "date";
    in.risk.market_data_date = "2026-10-09";
    in.risk.base_currency = "GBP";
    ores::reporting::domain::configuration_market_binding pricing;
    pricing.role = "pricing";
    pricing.market_configuration_name = "default";
    pricing.position = 1;
    in.market_bindings = {pricing};
    in.analytics = {analytic("curves", 1)};
    in.bound_types = {"conventions", "curve_configuration", "todays_market", "pricing_engines"};
    in.file_types = file_types();
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

TEST_CASE("the run names a file for each bound type and none for the others", tags) {
    auto in = inputs();
    in.bound_types = {"curve_configuration", "todays_market"};
    const auto& files = generate_run_document(in).files;
    REQUIRE(files.size() == 2);
    CHECK(files[0].parameter == "curveConfigFile");
    CHECK(files[0].name == "curveconfig.xml");
    CHECK(files[1].parameter == "marketConfigFile");
    CHECK(files[1].name == "todaysmarket.xml");
}

TEST_CASE("the run names the data files that belong to no configuration", tags) {
    auto in = inputs();
    in.file_types.push_back(setup_data_file("portfolioFile", "portfolio.xml"));
    in.file_types.push_back(setup_data_file("marketDataFile", "market.txt"));
    in.file_types.push_back(setup_data_file("fixingDataFile", "fixings.txt"));

    const auto& files = generate_run_document(in).files;
    REQUIRE(files.size() == 7);
    CHECK(files[4].parameter == "portfolioFile");
    CHECK(files[4].name == "portfolio.xml");
    CHECK(files[5].parameter == "marketDataFile");
    CHECK(files[5].name == "market.txt");
    CHECK(files[6].parameter == "fixingDataFile");
    CHECK(files[6].name == "fixings.txt");
}

TEST_CASE("the run names a data file of an analytic once, by the analytic", tags) {
    auto in = inputs();
    in.file_types.push_back(analytic_file("curves", "curvesOutputFile", "data_file", "curves.csv"));

    const auto& files = generate_run_document(in).files;
    for (const auto& f : files)
        CHECK(f.parameter != "curvesOutputFile");
}

TEST_CASE("the run carries the choices of the configuration as they are", tags) {
    const auto& s = generate_run_document(inputs()).settings;
    CHECK(s.market_data_type == "date");
    CHECK(s.market_data_date == "2026-10-09");
    CHECK(s.base_currency == std::optional<std::string>("GBP"));
}

TEST_CASE("the run carries the engine settings the definition binds", tags) {
    auto in = inputs();
    ores::reporting::messaging::run_engine_settings engine;
    engine.execution.emplace();
    engine.execution->n_threads = 4;
    in.engine = engine;

    const auto& r = generate_run_document(in);
    REQUIRE(r.engine_settings);
    REQUIRE(r.engine_settings->execution);
    CHECK(r.engine_settings->execution->n_threads == std::optional<int>(4));
    CHECK_FALSE(r.engine_settings->fixings);
}

TEST_CASE("a run with no engine settings carries none and the engine defaults hold", tags) {
    CHECK_FALSE(generate_run_document(inputs()).engine_settings);
}

TEST_CASE("the run carries the market bindings of the configuration", tags) {
    auto in = inputs();
    in.market_bindings[0].market_configuration_name = "overnight";
    const auto d = generate_run_document(in);
    REQUIRE(d.market_bindings.size() == 1);
    CHECK(d.market_bindings[0].role == "pricing");
    CHECK(d.market_bindings[0].market_configuration_name == "overnight");
}

TEST_CASE("a configuration with no market binding makes a run with none", tags) {
    auto in = inputs();
    in.market_bindings.clear();
    CHECK(generate_run_document(in).market_bindings.empty());
}

TEST_CASE("an end of day run values the portfolio on the weekday before the run date", tags) {
    auto in = inputs();
    in.risk.market_data_type = "eod";
    in.risk.market_data_date.clear();
    in.run_date = "2026-10-12";
    const auto& s = generate_run_document(in).settings;
    CHECK(s.market_data_type == "date");
    CHECK(s.market_data_date == "2026-10-09");
}

TEST_CASE("a live run values the portfolio on the run date", tags) {
    auto in = inputs();
    in.risk.market_data_type = "live";
    in.risk.market_data_date.clear();
    in.run_date = "2026-10-12";
    CHECK(generate_run_document(in).settings.market_data_date == "2026-10-12");
}

TEST_CASE("a configuration that names its date keeps it whatever the run date", tags) {
    auto in = inputs();
    in.run_date = "2030-01-01";
    CHECK(generate_run_document(in).settings.market_data_date == "2026-10-09");
}

TEST_CASE("a run date that is no date is refused for an end of day run", tags) {
    auto in = inputs();
    in.risk.market_data_type = "eod";
    in.risk.market_data_date.clear();
    in.run_date = "yesterday";
    CHECK_THROWS_AS(generate_run_document(in), std::invalid_argument);
}

TEST_CASE("a bound type that no file type names is refused", tags) {
    auto in = inputs();
    in.bound_types.push_back("netting");
    CHECK_THROWS_AS(generate_run_document(in), std::invalid_argument);
}

TEST_CASE("the risk configuration binding names no file", tags) {
    auto in = inputs();
    in.bound_types = {"risk_report_config", "conventions"};
    const auto& files = generate_run_document(in).files;
    REQUIRE(files.size() == 1);
    CHECK(files[0].parameter == "conventionsFile");
}

TEST_CASE("the engine settings binding names no file", tags) {
    auto in = inputs();
    in.bound_types = {"engine_settings", "conventions"};
    const auto& files = generate_run_document(in).files;
    REQUIRE(files.size() == 1);
    CHECK(files[0].parameter == "conventionsFile");
}

TEST_CASE("an analytic names the file it writes with the well known name", tags) {
    auto in = inputs();
    in.analytics = {analytic("npv", 1)};
    const auto d = generate_run_document(in);
    const auto* p = parameter_of(d.analytics[0], "outputFileName");
    REQUIRE(p != nullptr);
    CHECK(p->value == "npv.csv");
}

TEST_CASE("a name the configuration states is kept", tags) {
    auto in = inputs();
    auto npv = analytic("npv", 1);
    npv.parameters = {{"outputFileName", "mine.csv", 1}};
    in.analytics = {npv};
    const auto d = generate_run_document(in);
    REQUIRE(d.analytics[0].parameters.size() == 1);
    CHECK(d.analytics[0].parameters[0].value == "mine.csv");
}

TEST_CASE("an analytic that is off names no file", tags) {
    auto in = inputs();
    in.analytics = {analytic("curves", 1), analytic("npv", 2, "N")};
    const auto d = generate_run_document(in);
    CHECK(d.analytics[1].parameters.empty());
}

TEST_CASE("a reader takes the name of the analytic that writes the file", tags) {
    auto in = inputs();
    in.analytics = {analytic("sensitivity", 1), analytic("parametricVar", 2)};
    const auto d = generate_run_document(in);
    const auto* written = parameter_of(d.analytics[0], "sensitivityOutputFile");
    const auto* read = parameter_of(d.analytics[1], "sensitivityInputFile");
    REQUIRE(written != nullptr);
    REQUIRE(read != nullptr);
    CHECK(read->value == written->value);
}

TEST_CASE("a reader follows the name its writer was given", tags) {
    auto in = inputs();
    auto sensitivity = analytic("sensitivity", 1);
    sensitivity.parameters = {{"sensitivityOutputFile", "mine.csv", 1}};
    in.analytics = {sensitivity, analytic("parametricVar", 2)};
    const auto d = generate_run_document(in);
    const auto* read = parameter_of(d.analytics[1], "sensitivityInputFile");
    REQUIRE(read != nullptr);
    CHECK(read->value == "mine.csv");
}

TEST_CASE("a reader whose writer is not in the run names no file", tags) {
    auto in = inputs();
    in.analytics = {analytic("npv", 1), analytic("parametricVar", 2)};
    const auto d = generate_run_document(in);
    CHECK(parameter_of(d.analytics[1], "sensitivityInputFile") == nullptr);
}

TEST_CASE("a reader whose writer is switched off names no file", tags) {
    auto in = inputs();
    in.analytics = {analytic("sensitivity", 1, "N"), analytic("parametricVar", 2)};
    const auto d = generate_run_document(in);
    CHECK(parameter_of(d.analytics[1], "sensitivityInputFile") == nullptr);
}
