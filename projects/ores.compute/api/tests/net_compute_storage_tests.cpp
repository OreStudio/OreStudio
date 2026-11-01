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
#include "ores.compute.api/net/compute_storage.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>

namespace {

const std::string tags("[net][compute_storage]");

using ores::compute::net::compute_storage;

const std::string a_result = "7d1f6f0e-2a8b-4c0e-9a8e-0b6a4a1c2d3e";
const std::string another_result = "7d1f6f0e-2a8b-4c0e-9a8e-0b6a4a1c2d3f";

}

TEST_CASE("a result's output files and manifest fall under its own grant", tags) {
    const auto prefix = compute_storage::output_files_key(a_result);

    CHECK(compute_storage::output_file_key(a_result, "npv.csv") ==
          "compute/output/" + a_result + "/npv.csv.gz");
    CHECK(compute_storage::output_manifest_key(a_result) ==
          "compute/output/" + a_result + "/manifest.json.gz");
    CHECK(compute_storage::output_file_key(a_result, "npv.csv").starts_with(prefix));
    CHECK(compute_storage::output_manifest_key(a_result).starts_with(prefix));
}

TEST_CASE("one result's grant does not reach another result's files", tags) {
    const auto prefix = compute_storage::output_files_key(a_result);

    CHECK_FALSE(compute_storage::output_file_key(another_result, "npv.csv").starts_with(prefix));
    CHECK_FALSE(compute_storage::output_manifest_key(another_result).starts_with(prefix));
    CHECK_FALSE(compute_storage::output_key(a_result).starts_with(prefix));
}

TEST_CASE("a file name that would leave the result is refused", tags) {
    CHECK_THROWS_AS(compute_storage::output_file_key(a_result, "../npv.csv"),
                    std::invalid_argument);
    CHECK_THROWS_AS(compute_storage::output_file_key(a_result, "sub/npv.csv"),
                    std::invalid_argument);
}
