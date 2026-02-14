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
#include "ores.dq/domain/coding_scheme.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/coding_scheme_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/coding_scheme_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::coding_scheme;
using namespace ores::logging;

TEST_CASE("create_coding_scheme_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    coding_scheme sut;
    sut.version = 1;
    sut.code = "ISO-4217";
    sut.authority_type = "ISO";
    sut.subject_area_name = "Currencies";
    sut.domain_name = "Reference Data";
    sut.description = "ISO 4217 currency codes";
    sut.uri = "https://www.iso.org/iso-4217-currency-codes.html";
    sut.modified_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Coding scheme: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "ISO-4217");
    CHECK(sut.authority_type == "ISO");
    CHECK(sut.subject_area_name == "Currencies");
    CHECK(sut.domain_name == "Reference Data");
    CHECK(sut.uri == "https://www.iso.org/iso-4217-currency-codes.html");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("coding_scheme_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    coding_scheme cs;
    cs.version = 1;
    cs.code = "ISO-3166";
    cs.authority_type = "ISO";
    cs.subject_area_name = "Countries";
    cs.domain_name = "Reference Data";
    cs.description = "ISO 3166 country codes";
    cs.modified_by = "system";

    std::vector<coding_scheme> schemes = {cs};
    auto table = convert_to_table(schemes);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Code"));
    CHECK(table.contains("Description"));
    CHECK(table.contains("ISO-3166"));
    CHECK(table.contains("ISO 3166 country codes"));
}
