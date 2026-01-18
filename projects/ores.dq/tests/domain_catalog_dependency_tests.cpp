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
#include "ores.dq/domain/catalog_dependency.hpp"
#include "ores.dq/domain/catalog_names.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/catalog_dependency_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/catalog_dependency_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::catalog_dependency;
using namespace ores::dq::domain::catalog_names;
using namespace ores::logging;

TEST_CASE("create_catalog_dependency_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    catalog_dependency sut;
    sut.catalog_name = "Trade Data";
    sut.dependency_name = std::string(iso_reference_data);
    sut.recorded_by = "admin";
    sut.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Catalog dependency: " << sut;

    CHECK(sut.catalog_name == "Trade Data");
    CHECK(sut.dependency_name == "ISO Reference Data");
    CHECK(sut.recorded_by == "admin");
}

TEST_CASE("catalog_dependency_uses_well_known_names", tags) {
    auto lg(make_logger(test_suite));

    // Test that well-known catalog names can be used
    catalog_dependency dep1;
    dep1.catalog_name = std::string(trade_data);
    dep1.dependency_name = std::string(iso_reference_data);
    dep1.recorded_by = "system";

    catalog_dependency dep2;
    dep2.catalog_name = std::string(risk_analytics);
    dep2.dependency_name = std::string(trade_data);
    dep2.recorded_by = "system";

    CHECK(dep1.catalog_name == "Trade Data");
    CHECK(dep1.dependency_name == "ISO Reference Data");
    CHECK(dep2.catalog_name == "Risk Analytics");
    CHECK(dep2.dependency_name == "Trade Data");

    BOOST_LOG_SEV(lg, info) << "Well-known catalog names work correctly";
}

TEST_CASE("catalog_dependency_supports_user_defined_catalogs", tags) {
    auto lg(make_logger(test_suite));

    // Test that user-defined catalog names work
    catalog_dependency sut;
    sut.catalog_name = "My Custom Catalog";
    sut.dependency_name = "Another Custom Catalog";
    sut.recorded_by = "user123";
    sut.recorded_at = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "User-defined catalog dependency: " << sut;

    CHECK(sut.catalog_name == "My Custom Catalog");
    CHECK(sut.dependency_name == "Another Custom Catalog");
}

TEST_CASE("catalog_dependency_convert_to_table", tags) {
    auto lg(make_logger(test_suite));

    catalog_dependency dep1;
    dep1.catalog_name = "Trade Data";
    dep1.dependency_name = std::string(iso_reference_data);
    dep1.recorded_by = "admin";
    dep1.recorded_at = std::chrono::system_clock::now();

    catalog_dependency dep2;
    dep2.catalog_name = "Risk Analytics";
    dep2.dependency_name = "Trade Data";
    dep2.recorded_by = "admin";
    dep2.recorded_at = std::chrono::system_clock::now();

    std::vector<catalog_dependency> deps = {dep1, dep2};
    auto table = convert_to_table(deps);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Catalog"));
    CHECK(table.contains("Depends On"));
    CHECK(table.contains("Trade Data"));
    CHECK(table.contains("ISO Reference Data"));
    CHECK(table.contains("Risk Analytics"));
}
