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
#include "ores.ore.core/results/output_manifest.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>

namespace {

const std::string manifest_tags("[ore][results][output_manifest]");

using ores::ore::results::family_code_of;
using ores::ore::results::is_market_state_file;
using ores::ore::results::is_result_file;
using ores::ore::results::manifest_from_json;
using ores::ore::results::output_manifest;
using ores::ore::results::output_manifest_file;

output_manifest_file file_named(const std::string& name) {
    return output_manifest_file{.name = name,
                                .family = family_code_of(name),
                                .bytes = 120,
                                .compressed_bytes = 80,
                                .sha256 = "ab12",
                                .key = "compute/output/r/" + name + ".gz"};
}

}

TEST_CASE("a manifest reads back as it was written", manifest_tags) {
    const output_manifest written{
        .result_id = "r",
        .files = {file_named("netcube.csv"), file_named("npv.csv"), file_named("notes.txt")}};

    const auto read = manifest_from_json(to_json(written));

    CHECK(read.result_id == "r");
    REQUIRE(read.files.size() == 3);
    for (std::size_t i = 0; i < read.files.size(); ++i) {
        CHECK(read.files[i].name == written.files[i].name);
        CHECK(read.files[i].family == written.files[i].family);
        CHECK(read.files[i].bytes == 120);
        CHECK(read.files[i].compressed_bytes == 80);
        CHECK(read.files[i].sha256 == "ab12");
        CHECK(read.files[i].key == written.files[i].key);
    }
}

TEST_CASE("a file is state, a result, or neither, as its family says", manifest_tags) {
    CHECK(is_market_state_file(file_named("netcube.csv")));
    CHECK_FALSE(is_result_file(file_named("netcube.csv")));

    CHECK(is_result_file(file_named("npv.csv")));
    CHECK_FALSE(is_market_state_file(file_named("npv.csv")));

    const auto unknown = file_named("notes.txt");
    CHECK(unknown.family.empty());
    CHECK_FALSE(is_result_file(unknown));
    CHECK_FALSE(is_market_state_file(unknown));
}

TEST_CASE("text that is not a manifest is refused", manifest_tags) {
    CHECK_THROWS_AS(manifest_from_json("not json"), std::runtime_error);
    CHECK_THROWS_AS(manifest_from_json(R"({"result_id":"r"})"), std::runtime_error);
    CHECK_THROWS_AS(manifest_from_json(R"({"result_id":"r","files":[{"name":"npv.csv"}]})"),
                    std::runtime_error);
}
