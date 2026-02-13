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
#include "ores.dq/domain/subject_area.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/subject_area_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/subject_area_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::subject_area;
using namespace ores::logging;

TEST_CASE("create_subject_area_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    subject_area sut;
    sut.version = 1;
    sut.name = "Currencies";
    sut.domain_name = "Reference Data";
    sut.description = "Currency codes and related information";
    sut.modified_by = "admin";
    sut.change_commentary = "Initial creation";
    BOOST_LOG_SEV(lg, info) << "Subject area: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.name == "Currencies");
    CHECK(sut.domain_name == "Reference Data");
    CHECK(sut.description == "Currency codes and related information");
    CHECK(sut.modified_by == "admin");
    CHECK(sut.change_commentary == "Initial creation");
}

TEST_CASE("subject_area_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    subject_area sa;
    sa.version = 1;
    sa.name = "Countries";
    sa.domain_name = "Reference Data";
    sa.description = "Country codes and geographic data";
    sa.modified_by = "system";

    std::vector<subject_area> areas = {sa};
    auto table = convert_to_table(areas);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Name"));
    CHECK(table.contains("Description"));
    CHECK(table.contains("Countries"));
    CHECK(table.contains("Country codes and geographic data"));
}
