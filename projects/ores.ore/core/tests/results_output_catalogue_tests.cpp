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
#include "ores.testing/test_files.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

namespace {

const std::string catalogue_tags("[ore][results][output_catalogue]");

using ores::ore::results::column_named;
using ores::ore::results::family_named;
using ores::ore::results::family_of_file;
using ores::ore::results::output_column;
using ores::ore::results::output_column_kind;
using ores::ore::results::output_families;

std::vector<std::string> headings_of(const std::filesystem::path& path) {
    std::ifstream in(path);
    std::string line;
    std::getline(in, line);
    if (!line.empty() && line.front() == '#')
        line.erase(line.begin());

    std::vector<std::string> headings;
    std::string heading;
    for (const auto c : line) {
        if (c == ',') {
            headings.push_back(heading);
            heading.clear();
        } else if (c != '\r') {
            heading.push_back(c);
        }
    }
    headings.push_back(heading);
    return headings;
}

bool is_column(const std::vector<output_column>& columns, const std::string& name) {
    return std::ranges::any_of(
        columns, [&name](const output_column& column) { return column.name == name; });
}

}

TEST_CASE("every sample output belongs to a family", catalogue_tags) {
    const auto root = ores::testing::ore_path("examples");
    REQUIRE(std::filesystem::exists(root));

    std::size_t files = 0;
    for (const auto& suite : std::filesystem::directory_iterator(root)) {
        const auto expected = suite.path() / "ExpectedOutput";
        if (!std::filesystem::is_directory(expected))
            continue;
        for (const auto& entry : std::filesystem::recursive_directory_iterator(expected)) {
            if (!entry.is_regular_file() || entry.path().extension() != ".csv")
                continue;
            ++files;
            INFO("the file is " << entry.path().string());
            CHECK(family_of_file(entry.path().filename().string()) != nullptr);
        }
    }
    CHECK(files > 400);
}

TEST_CASE("a family covers the file names it states", catalogue_tags) {
    const auto* npv = family_of_file("npv.csv");
    REQUIRE(npv != nullptr);
    CHECK(npv->code == "npv");

    const auto* exposure = family_of_file("exposure_trade_Swap_20y_1.csv");
    REQUIRE(exposure != nullptr);
    CHECK(exposure->code == "exposure-trade");

    CHECK(family_named("curves") != nullptr);
    CHECK(family_named("no_such_family") == nullptr);
    CHECK(family_of_file("no_such_output.csv") == nullptr);
}

TEST_CASE("a family names the parameter that writes it, or says the engine does", catalogue_tags) {
    const auto* npv = family_named("npv");
    REQUIRE(npv != nullptr);
    CHECK(npv->file_parameter == "npv.outputFileName");

    const auto* exposure = family_named("exposure-trade");
    REQUIRE(exposure != nullptr);
    CHECK(exposure->file_parameter.empty());
}

TEST_CASE("a family states every heading its sample states", catalogue_tags) {
    for (const auto& code : {"npv", "flows", "curves", "exposure-trade"}) {
        const auto* family = family_named(code);
        REQUIRE(family != nullptr);
        INFO("the family is " << code);

        const auto sample = std::filesystem::path(family->sample_file);
        const auto relative = sample.lexically_relative("external/ore");
        REQUIRE_FALSE(relative.empty());
        const auto headings = headings_of(ores::testing::ore_path(relative.string()));
        REQUIRE_FALSE(headings.empty());

        for (const auto& heading : headings) {
            INFO("the heading is " << heading);
            CHECK(is_column(family->columns, heading));
        }
        for (const auto& key : family->key_columns) {
            INFO("the key is " << key);
            CHECK(is_column(family->columns, key));
        }
    }
}

TEST_CASE("a family states each column once, and its sample exists", catalogue_tags) {
    for (const auto& family : output_families()) {
        INFO("the family is " << family.code);
        auto names = family.columns;
        std::ranges::sort(names, {}, &output_column::name);
        CHECK(std::ranges::adjacent_find(names, {}, &output_column::name) == names.end());

        CHECK(family.header_variants > 0);
        const auto relative =
            std::filesystem::path(family.sample_file).lexically_relative("external/ore");
        REQUIRE_FALSE(relative.empty());
        CHECK(std::filesystem::exists(ores::testing::ore_path(relative.string())));
    }
}

TEST_CASE("a decimal column is a figure and a key is text or a date", catalogue_tags) {
    const auto* npv = family_named("npv");
    REQUIRE(npv != nullptr);

    const auto* npv_column = column_named(*npv, "NPV");
    REQUIRE(npv_column != nullptr);
    CHECK(npv_column->kind == output_column_kind::decimal);

    CHECK(std::ranges::find(npv->key_columns, std::string("TradeId")) != npv->key_columns.end());
}
