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
#include "ores.dq/domain/coding_scheme_authority_type.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/coding_scheme_authority_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/coding_scheme_authority_type_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::coding_scheme_authority_type;
using namespace ores::logging;

TEST_CASE("create_coding_scheme_authority_type_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    coding_scheme_authority_type sut;
    sut.version = 1;
    sut.code = "ISO";
    sut.description = "International Organization for Standardization";
    sut.recorded_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Coding scheme authority type: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "ISO");
    CHECK(sut.description == "International Organization for Standardization");
    CHECK(sut.recorded_by == "admin");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("coding_scheme_authority_type_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    coding_scheme_authority_type csat;
    csat.version = 1;
    csat.code = "ISDA";
    csat.description = "International Swaps and Derivatives Association";
    csat.recorded_by = "system";

    std::vector<coding_scheme_authority_type> types = {csat};
    auto table = convert_to_table(types);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Code"));
    CHECK(table.contains("Description"));
    CHECK(table.contains("ISDA"));
    CHECK(table.contains("International Swaps and Derivatives Association"));
}
