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
#include "ores.dq/domain/catalog.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/catalog_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/catalog_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::catalog;
using namespace ores::logging;

TEST_CASE("create_catalog_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    catalog sut;
    sut.version = 1;
    sut.name = "ISO Standards";
    sut.description = "Datasets conforming to ISO standards";
    sut.owner = "Standards Team";
    sut.modified_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Catalog: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.name == "ISO Standards");
    CHECK(sut.description == "Datasets conforming to ISO standards");
    CHECK(sut.owner == "Standards Team");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("catalog_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    catalog cat;
    cat.version = 1;
    cat.name = "FpML Standards";
    cat.description = "FpML-based financial products";
    cat.modified_by = "system";

    std::vector<catalog> catalogs = {cat};
    auto table = convert_to_table(catalogs);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Name"));
    CHECK(table.contains("Description"));
    CHECK(table.contains("FpML Standards"));
    CHECK(table.contains("FpML-based financial products"));
}
