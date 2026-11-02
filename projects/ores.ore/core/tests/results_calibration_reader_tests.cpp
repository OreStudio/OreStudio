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
#include "ores.ore.core/results/calibration_reader.hpp"
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.testing/test_files.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>

namespace {

const std::string calibration_tags("[ore][results][calibration_reader]");

using ores::ore::results::family_named;
using ores::ore::results::is_calibration_family;
using ores::ore::results::read_calibration;

const std::string heading = "#MarketObjectType,MarketObjectId,ResultId,ResultKey1,ResultKey2,"
                            "ResultKey3,ResultType,ResultValue\n";

}

TEST_CASE("a calibration keeps text as text and a double as a figure", calibration_tags) {
    const auto* family = family_named("todaysmarketcalibration");
    REQUIRE(family != nullptr);
    CHECK(is_calibration_family(*family));

    const auto outcome = read_calibration(
        heading + "yieldCurve,USD1D,dayCounter,,,,string,Actual/365 (Fixed)\n"
                  "yieldCurve,USD1D,time,2016-02-08,MM/RATE/USD/0D/1D,,double,0.00821918\n",
        *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 2);

    const auto& text = outcome.rows[0];
    CHECK(text.market_object_type == "yieldCurve");
    CHECK(text.market_object_id == "USD1D");
    CHECK(text.result_name == "dayCounter");
    CHECK(text.result_key_1.empty());
    CHECK(text.value == "Actual/365 (Fixed)");
    CHECK_FALSE(text.figure.has_value());
    CHECK(text.line == 2);

    const auto& pillar = outcome.rows[1];
    CHECK(pillar.result_key_1 == "2016-02-08");
    CHECK(pillar.result_key_2 == "MM/RATE/USD/0D/1D");
    CHECK(pillar.result_key_3.empty());
    CHECK(pillar.result_type == "double");
    REQUIRE(pillar.figure.has_value());
    CHECK(*pillar.figure == 0.00821918);
    CHECK(pillar.line == 3);
}

TEST_CASE("a calibration double that is not a figure is refused by line", calibration_tags) {
    const auto* family = family_named("todaysmarketcalibration");
    REQUIRE(family != nullptr);

    const auto outcome =
        read_calibration(heading + "yieldCurve,USD1D,time,,,,double,soon\n", *family);

    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.refusals.front().find("line 2") != std::string::npos);
    CHECK(outcome.rows.empty());
}

TEST_CASE("a calibration double the engine left empty has no figure and is not refused",
          calibration_tags) {
    const auto* family = family_named("todaysmarketcalibration");
    REQUIRE(family != nullptr);

    const auto outcome =
        read_calibration(heading + "yieldCurve,USD1D,time,,,,double,\n", *family);

    REQUIRE(outcome.refusals.empty());
    REQUIRE(outcome.rows.size() == 1);
    CHECK(outcome.rows.front().value.empty());
    CHECK_FALSE(outcome.rows.front().figure.has_value());
}

TEST_CASE("a calibration value that names no market object is refused", calibration_tags) {
    const auto* family = family_named("todaysmarketcalibration");
    REQUIRE(family != nullptr);

    const auto outcome = read_calibration(heading + ",USD1D,currency,,,,string,USD\n", *family);

    REQUIRE(outcome.refusals.size() == 1);
    CHECK(outcome.rows.empty());
}

TEST_CASE("the checked-in calibration sample reads whole, one row for each line",
          calibration_tags) {
    const auto* family = family_named("todaysmarketcalibration");
    REQUIRE(family != nullptr);
    const auto relative =
        std::filesystem::path(family->sample_file).lexically_relative("external/ore");
    const auto csv = ores::testing::read_file(ores::testing::ore_path(relative.string()));

    const auto outcome = read_calibration(csv, *family);

    REQUIRE(outcome.refusals.empty());
    CHECK(outcome.rows.size() == 308);
}
