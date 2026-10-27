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
#include "ores.compute.core/messaging/assignment_grants.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

using ores::compute::messaging::grant_for_path;
using ores::compute::messaging::storage_grants_for;

const std::string package = "/api/v1/storage/ores/compute/packages/ORE-1/ore.tar.gz";
const std::string input = "/api/v1/storage/ores/compute/input/job.tar.gz";
const std::string output = "/api/v1/storage/ores/compute/output/result.tar.gz";

}

TEST_CASE("grant_for_path_names_the_bucket_the_key_and_the_operation", "[messaging]") {
    const auto grant = grant_for_path(package, "get");

    REQUIRE(grant.has_value());
    CHECK(grant->bucket == "ores");
    CHECK(grant->key_prefix == "compute/packages/ORE-1/ore.tar.gz");
    CHECK(grant->op == "get");
}

TEST_CASE("grant_for_path_refuses_a_path_that_names_no_object", "[messaging]") {
    CHECK(!grant_for_path("", "get").has_value());
    CHECK(!grant_for_path("/somewhere/else", "get").has_value());
    CHECK(!grant_for_path("/api/v1/storage/ores", "get").has_value());
    CHECK(!grant_for_path("/api/v1/storage/ores/", "get").has_value());
}

TEST_CASE("storage_grants_for_reads_the_package_and_the_input_and_writes_the_output", "[messaging]") {
    const auto grants = storage_grants_for(package, input, output);

    REQUIRE(grants.has_value());
    REQUIRE(grants->size() == 3);
    CHECK((*grants)[0].op == "get");
    CHECK((*grants)[0].key_prefix == "compute/packages/ORE-1/ore.tar.gz");
    CHECK((*grants)[1].op == "get");
    CHECK((*grants)[1].key_prefix == "compute/input/job.tar.gz");
    CHECK((*grants)[2].op == "put");
    CHECK((*grants)[2].key_prefix == "compute/output/result.tar.gz");
}

TEST_CASE("storage_grants_for_is_refused_when_any_path_names_no_object", "[messaging]") {
    // A job with a missing grant cannot run, so the whole set is refused.
    CHECK(!storage_grants_for("", input, output).has_value());
    CHECK(!storage_grants_for(package, "nope", output).has_value());
    CHECK(!storage_grants_for(package, input, "").has_value());
}
