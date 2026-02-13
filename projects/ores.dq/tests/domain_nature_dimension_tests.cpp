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
#include "ores.dq/domain/nature_dimension.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/nature_dimension_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/nature_dimension_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::nature_dimension;
using namespace ores::logging;

TEST_CASE("create_nature_dimension_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    nature_dimension sut;
    sut.version = 1;
    sut.code = "raw";
    sut.description = "Unprocessed data as received";
    sut.modified_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Nature dimension: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "raw");
    CHECK(sut.description == "Unprocessed data as received");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("nature_dimension_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    nature_dimension nd;
    nd.version = 1;
    nd.code = "derived";
    nd.description = "Data calculated from other datasets";
    nd.modified_by = "system";

    std::vector<nature_dimension> dimensions = {nd};
    auto table = convert_to_table(dimensions);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Code"));
    CHECK(table.contains("Description"));
    CHECK(table.contains("derived"));
    CHECK(table.contains("Data calculated from other datasets"));
}
