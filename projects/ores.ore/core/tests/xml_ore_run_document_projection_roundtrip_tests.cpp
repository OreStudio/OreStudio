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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/run_document_projection.hpp"
#include "ores.ore.core/xml/document_kind.hpp"
#include "ores.ore.core/xml/record.hpp"
#include "ores.ore.core/xml/record_diff.hpp"
#include "ores.testing/project_root.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <vector>

/**
 * @file xml_ore_run_document_projection_roundtrip_tests.cpp
 * @brief The run document, projected and projected back.
 */

namespace {

using namespace ores::logging;
using namespace ores::ore::domain;

namespace fs = std::filesystem;

fs::path corpus_root() {
    return ores::testing::project_root::resolve("external/ore/examples");
}

std::string read_file(const fs::path& path) {
    std::ifstream file(path, std::ios::binary);
    return std::string(std::istreambuf_iterator<char>(file), std::istreambuf_iterator<char>());
}

namespace xg = ores::ore::xml::generated::ore;
using ores::ore::xml::record_reader;

/// The Setup parameters as a name to value map, in the last-occurrence rule.
std::map<std::string, std::string> setup_of(const xg::ore& d) {
    std::map<std::string, std::string> out;
    const auto root = record_reader(*xg::ore_document().root, &d);
    const auto setup = root.child("Setup");
    if (!setup)
        return out;
    for (const auto& p : setup->children("Parameter"))
        out[p.text("name").value_or("")] = p.own_text().value_or("");
    return out;
}

/// The Setup parameters a round trip is expected to keep.
///
/// A parameter the model does not hold is dropped, and an engine flag is written in
/// one spelling whatever spelling the document used.
std::map<std::string, std::string> kept_setup_of(const xg::ore& d) {
    static const std::set<std::string> flags = {"implyTodaysFixings",
                                                "includeTodaysCashFlows",
                                                "includeReferenceDateEvents",
                                                "enrichIndexFixings",
                                                "lazyMarketBuilding",
                                                "buildFailedTrades",
                                                "continueOnError",
                                                "useAtParCouponsCurves",
                                                "useAtParCouponsTrades"};
    auto out = setup_of(d);
    for (const auto& name : run_document_projection::map_setup(d).dropped)
        out.erase(name);
    for (auto& [name, value] : out) {
        if (flags.contains(name))
            value = (value == "Y" || value == "true") ? "true" : "false";
    }
    return out;
}

/// The analytics, in document order, as a canonical rendering.
///
/// The parameters of one analytic are compared by name and value rather than by
/// position: the projection writes the active flag first, which is where ORE
/// writes it, and a document may have written it anywhere.
std::vector<std::string> analytics_of(const xg::ore& d) {
    std::vector<std::string> out;
    const auto root = record_reader(*xg::ore_document().root, &d);
    const auto analytics = root.child("Analytics");
    if (!analytics)
        return out;
    for (const auto& element : analytics->children("Analytic")) {
        std::map<std::string, std::string> parameters;
        for (const auto& p : element.children("Parameter"))
            parameters[p.text("name").value_or("")] = p.own_text().value_or("");
        std::string text = "type=" + element.text("type").value_or("") + "{";
        for (const auto& [name, value] : parameters)
            text += name + "=" + value + ";";
        out.push_back(text);
    }
    return out;
}

/// Reads, projects, projects back and writes one document, naming a failure.
///
/// The Setup parameters are compared by name and value rather than by position:
/// the projection writes them in a fixed order, which is what the mapping has
/// always done, because the entity is a row of columns with no order of its own.
std::string round_trip(const std::string& text, const std::string& path) {
    try {
        const auto original = run_document_projection::read(text, path);
        const auto mapped = run_document_projection::map(original);
        const auto rebuilt = run_document_projection::reverse(mapped);
        const auto exported =
            run_document_projection::read(run_document_projection::write(rebuilt), path);

        if (kept_setup_of(original) != setup_of(exported))
            return path + ": the Setup parameters differ";
        if (analytics_of(original) != analytics_of(exported))
            return path + ": the analytics differ";
        const auto* root = xg::ore_document().root;
        if ((original.markets.present != 0) != (exported.markets.present != 0))
            return path + ": the Markets element differs in presence";
        const auto* markets_type = ores::ore::xml::find_child_type(*root, "Markets");
        if (const auto found = ores::ore::xml::compare_records(
                *markets_type, &original.markets, &exported.markets))
            return path + ": Markets " + found->path + ": " + found->detail;
        return {};
    } catch (const std::exception& e) {
        return path + ": " + e.what();
    }
}

}

TEST_CASE("every_run_document_in_the_corpus_round_trips", "[ore][xml][roundtrip][ore][run_document]") {
    auto lg(make_logger(test_suite));

    std::size_t documents = 0;
    std::string failures;
    for (const auto& entry : fs::recursive_directory_iterator(corpus_root())) {
        if (!entry.is_regular_file() || entry.path().extension() != ".xml")
            continue;
        if (ores::ore::xml::read_root_element(entry.path()) != "ORE")
            continue;
        ++documents;
        const auto detail = round_trip(read_file(entry.path()), entry.path().string());
        if (!detail.empty())
            failures += detail + "\n";
    }

    INFO("documents: " << documents);
    INFO(failures);
    CHECK(documents == 422);
    CHECK(failures.empty());
}

TEST_CASE("the_projection_states_the_positions_the_database_sorts_by", "[ore][xml][roundtrip][ore][run_document]") {
    auto lg(make_logger(test_suite));

    const auto original = run_document_projection::read(R"(
<ORE>
  <Setup>
    <Parameter name="asofDate">2026-01-02</Parameter>
    <Parameter name="inputPath">Input</Parameter>
    <Parameter name="nThreads">4</Parameter>
    <Parameter name="continueOnError">false</Parameter>
    <Parameter name="conventionsFile">conventions.xml</Parameter>
    <Parameter name="continueOnError">Y</Parameter>
  </Setup>
  <Markets>
    <Parameter name="EQ">EUR</Parameter>
    <Parameter name="FX">USD</Parameter>
  </Markets>
  <Analytics>
    <Analytic type="pnl"><Parameter name="active">Y</Parameter><Parameter name="a">1</Parameter></Analytic>
    <Analytic type="risk"><Parameter name="b">2</Parameter><Parameter name="active">N</Parameter></Analytic>
  </Analytics>
</ORE>)");

    const auto mapped = run_document_projection::map(original);
    // The as-of date is the one parameter with a column pair.
    CHECK(mapped.settings.market_data_type == "date");
    CHECK(mapped.settings.market_data_date == "2026-01-02");
    // A choice and a file name go to different places.
    REQUIRE(mapped.engine_settings);
    REQUIRE(mapped.engine_settings->execution);
    CHECK(mapped.engine_settings->execution->n_threads == std::optional<int>(4));
    CHECK_FALSE(mapped.engine_settings->fixings);
    CHECK(mapped.files.size() == 1);
    CHECK(mapped.files.front().parameter == "conventionsFile");
    // A name stated twice is read from its last occurrence.
    REQUIRE(mapped.engine_settings->building);
    CHECK(mapped.engine_settings->building->continue_on_error == std::optional<bool>(true));

    REQUIRE(mapped.analytics.size() == 2);
    for (std::size_t i = 0; i < mapped.analytics.size(); ++i)
        CHECK(mapped.analytics[i].analytic.display_order == static_cast<int>(i + 1));
    CHECK(mapped.analytics[0].analytic.active == "Y");
    CHECK(mapped.analytics[1].analytic.active == "N");
    REQUIRE(mapped.analytics[0].parameters.size() == 1);
    CHECK(mapped.analytics[0].parameters.front().position == 2);

    REQUIRE(mapped.market_bindings.size() == 2);
    CHECK(mapped.market_bindings[0].role == "EQ");
    CHECK(mapped.market_bindings[0].position == 1);
    CHECK(mapped.market_bindings[1].position == 2);

    // The writing puts each analytic's active flag before its other parameters.
    const auto written = run_document_projection::write(run_document_projection::reverse(mapped));
    const auto active_at = written.find("<Parameter name=\"active\">");
    const auto next_at = written.find("<Parameter name=\"a\">");
    REQUIRE(active_at != std::string::npos);
    REQUIRE(next_at != std::string::npos);
    CHECK(active_at < next_at);
}

TEST_CASE("a_setup_parameter_the_model_does_not_hold_is_dropped_with_a_warning", "[ore][xml][roundtrip][ore][run_document]") {
    auto lg(make_logger(test_suite));

    const auto original = run_document_projection::read(R"(<ORE>
  <Setup>
    <Parameter name="inputPath">a</Parameter>
    <Parameter name="notAParameter">b</Parameter>
    <Parameter name="fixingCutoff">2026-01-01</Parameter>
    <Parameter name="dateCalendar">TARGET</Parameter>
  </Setup>
  <Analytics/>
</ORE>)");

    const auto mapped = run_document_projection::map_setup(original);
    CHECK(mapped.dropped ==
          std::vector<std::string>{"notAParameter", "fixingCutoff", "dateCalendar"});
    CHECK_FALSE(mapped.engine_settings);

    const auto warnings = run_document_projection::dropped_parameter_warnings(original);
    REQUIRE(warnings.size() == 3);
    CHECK_THAT(warnings[0], Catch::Matchers::ContainsSubstring("'notAParameter'"));
    CHECK_THAT(warnings[1], Catch::Matchers::ContainsSubstring("'fixingCutoff'"));
    CHECK_THAT(warnings[1], Catch::Matchers::ContainsSubstring("numbers may differ"));
    CHECK_THAT(warnings[2], Catch::Matchers::ContainsSubstring("'dateCalendar'"));
}

TEST_CASE("an_engine_setting_with_a_value_ore_does_not_read_is_refused", "[ore][xml][roundtrip][ore][run_document]") {
    auto lg(make_logger(test_suite));

    const auto document = [](std::string_view parameter) {
        return run_document_projection::read(std::format(R"(<ORE>
  <Setup>
    <Parameter name="inputPath">a</Parameter>
    {}
  </Setup>
  <Analytics/>
</ORE>)",
                                                         parameter));
    };
    CHECK_THROWS_WITH(
        run_document_projection::map(
            document("<Parameter name=\"observationModel\">Move</Parameter>")),
        Catch::Matchers::ContainsSubstring("is not one of None, Disable, Defer or Unregister"));
    CHECK_THROWS_WITH(run_document_projection::map(
                          document("<Parameter name=\"lazyMarketBuilding\">maybe</Parameter>")),
                      Catch::Matchers::ContainsSubstring("is not a flag"));
}

TEST_CASE("a_setup_parameter_that_is_not_a_number_is_refused", "[ore][xml][roundtrip][ore][run_document]") {
    auto lg(make_logger(test_suite));

    const auto document = [](std::string_view parameter) {
        return run_document_projection::read(std::format(R"(<ORE>
  <Setup>
    <Parameter name="inputPath">a</Parameter>
    {}
  </Setup>
  <Analytics/>
</ORE>)",
                                                         parameter));
    };
    CHECK_THROWS_WITH(
        run_document_projection::map(document("<Parameter name=\"nThreads\">many</Parameter>")),
        Catch::Matchers::ContainsSubstring("is not an integer"));
    CHECK_THROWS_WITH(
        run_document_projection::map(document("<Parameter name=\"logMask\"></Parameter>")),
        Catch::Matchers::ContainsSubstring("is not an integer"));
}
