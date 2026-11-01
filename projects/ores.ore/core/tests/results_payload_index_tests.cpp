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
#include "ores.ore.core/results/output_catalogue.hpp"
#include "ores.ore.core/results/payload_index.hpp"
#include "ores.testing/test_files.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>
#include <vector>

namespace {

const std::string payload_tags("[ore][results][payload_index]");

using ores::ore::results::family_named;
using ores::ore::results::index_payload;
using ores::ore::results::payload_index_value_limit;

std::string payload_sample_text(const std::string& sample_file) {
    const auto relative = std::filesystem::path(sample_file).lexically_relative("external/ore");
    return ores::testing::read_file(ores::testing::ore_path(relative.string()));
}

}

TEST_CASE("a cube's index names the columns and the dates it covers", payload_tags) {
    const auto* family = family_named("stress_scenarios");
    REQUIRE(family != nullptr);

    std::vector<std::string> refusals;
    const auto index = index_payload(payload_sample_text(family->sample_file), *family, refusals);

    REQUIRE(refusals.empty());
    CHECK(index.rows > 0);

    // The index states the headings the file states, in its order. A sample is
    // one run's cube, which is narrower than the family's union of columns.
    REQUIRE(index.columns.size() > 3);
    CHECK(index.columns[0] == "Date");
    CHECK(index.columns[1] == "Scenario");
    CHECK(index.columns[2] == "Numeraire");
    CHECK(index.columns.size() < family->columns.size());

    // The catalogue names the key columns, so the index records their values and
    // nothing else: a date, a scenario and the figures the cube holds.
    REQUIRE(index.keys.size() == 2);
    CHECK(index.keys[0].first == "Date");
    CHECK_FALSE(index.keys[0].second.empty());
    CHECK(index.keys[1].first == "Scenario");
    CHECK_FALSE(index.keys[1].second.empty());
    CHECK_FALSE(index.truncated);
}

TEST_CASE("a cube whose key states more values than the index holds says so", payload_tags) {
    const auto* family = family_named("stress_scenarios");
    REQUIRE(family != nullptr);

    std::string csv = "#Date,Scenario,Numeraire,DiscountCurve/USD/0\n";
    for (std::size_t i = 0; i < payload_index_value_limit + 10; ++i)
        csv += std::to_string(i) + ",BASE,1.0,0.99\n";

    std::vector<std::string> refusals;
    const auto index = index_payload(csv, *family, refusals);

    REQUIRE(refusals.empty());
    CHECK(index.rows == payload_index_value_limit + 10);
    REQUIRE(index.keys.size() == 2);
    CHECK(index.keys[0].second.size() == payload_index_value_limit);
    CHECK(index.truncated);
}

TEST_CASE("a key the file does not state is left out of the index", payload_tags) {
    const auto* family = family_named("scenario");
    REQUIRE(family != nullptr);

    // The catalogue says a scenario row is keyed by its trade and its factor; a
    // file that states only the trade is indexed by what it does state.
    std::vector<std::string> refusals;
    const auto index = index_payload("#TradeId,Up/Down,Base NPV\nT1,Up,1.0\n", *family, refusals);

    REQUIRE(refusals.empty());
    CHECK(index.rows == 1);
    REQUIRE(index.keys.size() == 1);
    CHECK(index.keys[0].first == "TradeId");
    CHECK(index.keys[0].second == std::vector<std::string>{"T1"});
}

TEST_CASE("a file with no heading is refused", payload_tags) {
    const auto* family = family_named("rawcube");
    REQUIRE(family != nullptr);

    std::vector<std::string> refusals;
    const auto index = index_payload("", *family, refusals);

    REQUIRE_FALSE(refusals.empty());
    CHECK(index.columns.empty());
}
