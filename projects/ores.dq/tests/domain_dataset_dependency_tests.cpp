/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/domain/dataset_dependency.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/dataset_dependency_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/dataset_dependency_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::dataset_dependency;
using namespace ores::logging;

TEST_CASE("create_dataset_dependency_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    dataset_dependency sut;
    sut.dataset_code = "iso.countries";
    sut.dependency_code = "assets.country_flags";
    sut.role = "visual_assets";
    sut.recorded_by = "admin";
    sut.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Dataset dependency: " << sut;

    CHECK(sut.dataset_code == "iso.countries");
    CHECK(sut.dependency_code == "assets.country_flags");
    CHECK(sut.role == "visual_assets");
    CHECK(sut.recorded_by == "admin");
}

TEST_CASE("dataset_dependency_uses_standard_codes", tags) {
    auto lg(make_logger(test_suite));

    // Test typical dataset dependency relationships
    dataset_dependency dep1;
    dep1.dataset_code = "iso.currencies";
    dep1.dependency_code = "assets.country_flags";
    dep1.role = "visual_assets";
    dep1.recorded_by = "system";

    dataset_dependency dep2;
    dep2.dataset_code = "crypto.reference";
    dep2.dependency_code = "assets.crypto_icons";
    dep2.role = "visual_assets";
    dep2.recorded_by = "system";

    CHECK(dep1.dataset_code == "iso.currencies");
    CHECK(dep1.dependency_code == "assets.country_flags");
    CHECK(dep1.role == "visual_assets");
    CHECK(dep2.dataset_code == "crypto.reference");
    CHECK(dep2.dependency_code == "assets.crypto_icons");

    BOOST_LOG_SEV(lg, info) << "Standard dataset codes work correctly";
}

TEST_CASE("dataset_dependency_supports_custom_codes", tags) {
    auto lg(make_logger(test_suite));

    // Test that custom codes work
    dataset_dependency sut;
    sut.dataset_code = "custom.my_dataset";
    sut.dependency_code = "custom.reference_data";
    sut.role = "reference_data";
    sut.recorded_by = "user123";
    sut.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Custom dataset dependency: " << sut;

    CHECK(sut.dataset_code == "custom.my_dataset");
    CHECK(sut.dependency_code == "custom.reference_data");
    CHECK(sut.role == "reference_data");
}

TEST_CASE("dataset_dependency_convert_to_table", tags) {
    auto lg(make_logger(test_suite));

    dataset_dependency dep1;
    dep1.dataset_code = "iso.countries";
    dep1.dependency_code = "assets.country_flags";
    dep1.role = "visual_assets";
    dep1.recorded_by = "admin";
    dep1.recorded_at = std::chrono::system_clock::now();

    dataset_dependency dep2;
    dep2.dataset_code = "crypto.reference";
    dep2.dependency_code = "assets.crypto_icons";
    dep2.role = "visual_assets";
    dep2.recorded_by = "admin";
    dep2.recorded_at = std::chrono::system_clock::now();

    std::vector<dataset_dependency> deps = {dep1, dep2};
    auto table = convert_to_table(deps);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Dataset Code"));
    CHECK(table.contains("Depends On"));
    CHECK(table.contains("Role"));
    CHECK(table.contains("iso.countries"));
    CHECK(table.contains("assets.country_flags"));
    CHECK(table.contains("visual_assets"));
}
