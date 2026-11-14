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
#include "ores.ore.core/xml/run_package.hpp"
#include <catch2/catch_test_macros.hpp>
#include <map>
#include <string>

/**
 * @file xml_run_package_missing_files_tests.cpp
 * @brief What a package owes the run document it carries.
 */

TEST_CASE("the files a run document names and the input does not hold are named",
          "[ore][run_package]") {
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

TEST_CASE("an input that holds every file its run document names holds nothing missing",
          "[ore][run_package]") {
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

TEST_CASE("an analytic whose files the input does not carry is dropped from the document",
          "[ore][run_package]") {
    std::map<std::string, std::string> pack;
    pack["ore.xml"] = R"(<ORE>
  <Setup>
    <Parameter name="asofDate">2026-01-02</Parameter>
    <Parameter name="conventionsFile">conventions.xml</Parameter>
  </Setup>
  <Analytics>
    <Analytic type="npv"><Parameter name="active">Y</Parameter></Analytic>
    <Analytic type="sensitivity">
      <Parameter name="active">Y</Parameter>
      <Parameter name="marketConfigFile">simulation.xml</Parameter>
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
    CHECK(pruned.files.at("ore.xml").find("npv") != std::string::npos);
}

TEST_CASE("an input whose analytics all load is handed back whole", "[ore][run_package]") {
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
