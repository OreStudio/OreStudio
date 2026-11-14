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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/run_package.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>

/**
 * @file xml_run_package_missing_files_tests.cpp
 * @brief What a package owes the run document it carries.
 */

namespace {

const std::string run_package_missing_files_test_suite("ores::ore::core::tests");
const std::string run_package_missing_files_tags("[ore][run_package]");

}

TEST_CASE("names_the_files_the_run_document_holds_and_the_input_does_not",
          run_package_missing_files_tags) {
    auto lg(ores::logging::make_logger(run_package_missing_files_test_suite));

    // The sensitivity analytic states a scenario market of its own, and the input
    // set carries the document's configuration but not that file. The engine would
    // stop at it before reading a trade.
    std::map<std::string, std::string> pack;
    pack["ore.xml"] = R"(<ORE>
  <Setup>
    <Parameter name="asofDate">2026-01-02</Parameter>
    <Parameter name="conventionsFile">conventions.xml</Parameter>
    <Parameter name="marketDataFile">market.txt</Parameter>
  </Setup>
  <Analytics>
    <Analytic type="sensitivity">
      <Parameter name="active">Y</Parameter>
      <Parameter name="sensitivityConfigFile">sensitivity.xml</Parameter>
      <Parameter name="marketConfigFile">simulation.xml</Parameter>
      <Parameter name="recalibrateModels">Y</Parameter>
    </Analytic>
  </Analytics>
</ORE>)";
    pack["conventions.xml"] = "<Conventions/>";
    pack["market.txt"] = "2026-01-02 1.0";

    const auto missing = ores::ore::xml::missing_named_files(pack);

    REQUIRE(missing.size() == 2);
    CHECK(missing[0] == "sensitivity.xml");
    CHECK(missing[1] == "simulation.xml");
}

TEST_CASE("holds_nothing_missing_when_every_named_file_is_present",
          run_package_missing_files_tags) {
    auto lg(ores::logging::make_logger(run_package_missing_files_test_suite));
    std::map<std::string, std::string> pack;
    pack["ore.xml"] = R"(<ORE>
  <Setup>
    <Parameter name="asofDate">2026-01-02</Parameter>
    <Parameter name="nThreads">1</Parameter>
    <Parameter name="conventionsFile">conventions.xml</Parameter>
  </Setup>
  <Analytics>
    <Analytic type="npv"><Parameter name="active">Y</Parameter></Analytic>
  </Analytics>
</ORE>)";
    pack["conventions.xml"] = "<Conventions/>";

    // A flag, a date and a count are not file names.
    CHECK(ores::ore::xml::missing_named_files(pack).empty());
}

TEST_CASE("drops_the_analytic_whose_files_the_input_does_not_carry",
          run_package_missing_files_tags) {
    auto lg(ores::logging::make_logger(run_package_missing_files_test_suite));
    std::map<std::string, std::string> pack;
    pack["ore.xml"] = R"(<ORE>
  <Setup>
    <Parameter name="asofDate">2026-01-02</Parameter>
    <Parameter name="conventionsFile">conventions.xml</Parameter>
  </Setup>
  <Analytics>
    <Analytic type="npv">
      <Parameter name="active">Y</Parameter>
      <Parameter name="outputFileName">npv.csv</Parameter>
    </Analytic>
    <Analytic type="sensitivity">
      <Parameter name="active">Y</Parameter>
      <Parameter name="marketConfigFile">simulation.xml</Parameter>
      <Parameter name="scenarioOutputFile">scenario.csv</Parameter>
    </Analytic>
  </Analytics>
</ORE>)";
    pack["conventions.xml"] = "<Conventions/>";

    const auto pruned = ores::ore::xml::without_unloadable_analytics(pack);

    // The analytic that cannot load is gone, and the run that could price stays.
    REQUIRE(pruned.dropped.size() == 1);
    CHECK(pruned.dropped[0] == "sensitivity");
    CHECK(ores::ore::xml::missing_named_files(pruned.files).empty());
    CHECK(pruned.files.at("ore.xml").find("sensitivity") == std::string::npos);
    // A file the analytic writes is not a file it reads, so npv keeps its output
    // name and stays in the document.
    CHECK(pruned.files.at("ore.xml").find("npv") != std::string::npos);
    CHECK(pruned.files.at("ore.xml").find("npv.csv") != std::string::npos);
}

TEST_CASE("hands_back_an_input_whose_analytics_all_load", run_package_missing_files_tags) {
    auto lg(ores::logging::make_logger(run_package_missing_files_test_suite));
    std::map<std::string, std::string> pack;
    pack["ore.xml"] = R"(<ORE>
  <Setup>
    <Parameter name="conventionsFile">conventions.xml</Parameter>
  </Setup>
  <Analytics>
    <Analytic type="npv"><Parameter name="active">Y</Parameter></Analytic>
  </Analytics>
</ORE>)";
    pack["conventions.xml"] = "<Conventions/>";

    const auto pruned = ores::ore::xml::without_unloadable_analytics(pack);

    CHECK(pruned.dropped.empty());
    CHECK(pruned.files.at("ore.xml") == pack.at("ore.xml"));
}
