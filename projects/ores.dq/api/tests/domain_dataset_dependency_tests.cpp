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
#include "ores.dq.api/domain/dataset_dependency.hpp"
#include "ores.dq.api/domain/dataset_dependency_json_io.hpp" // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"
#include <catch2/catch_test_macros.hpp>

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
    sut.modified_by = "admin";
    sut.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Dataset dependency: " << sut;

    CHECK(sut.dataset_code == "iso.countries");
    CHECK(sut.dependency_code == "assets.country_flags");
    CHECK(sut.role == "visual_assets");
    CHECK(sut.modified_by == "admin");
}

TEST_CASE("dataset_dependency_uses_standard_codes", tags) {
    auto lg(make_logger(test_suite));

    // Test typical dataset dependency relationships
    dataset_dependency dep1;
    dep1.dataset_code = "iso.currencies";
    dep1.dependency_code = "assets.country_flags";
    dep1.role = "visual_assets";
    dep1.modified_by = "system";

    dataset_dependency dep2;
    dep2.dataset_code = "crypto.large";
    dep2.dependency_code = "assets.crypto_icons";
    dep2.role = "visual_assets";
    dep2.modified_by = "system";

    CHECK(dep1.dataset_code == "iso.currencies");
    CHECK(dep1.dependency_code == "assets.country_flags");
    CHECK(dep1.role == "visual_assets");
    CHECK(dep2.dataset_code == "crypto.large");
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
    sut.modified_by = "user123";
    sut.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Custom dataset dependency: " << sut;

    CHECK(sut.dataset_code == "custom.my_dataset");
    CHECK(sut.dependency_code == "custom.reference_data");
    CHECK(sut.role == "reference_data");
}
