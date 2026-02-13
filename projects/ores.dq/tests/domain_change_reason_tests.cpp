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
#include "ores.dq/domain/change_reason.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/change_reason_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/change_reason_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::change_reason;
using namespace ores::logging;

TEST_CASE("create_change_reason_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    change_reason sut;
    sut.version = 1;
    sut.code = "static_data.front_office_error";
    sut.description = "Front office data entry error";
    sut.category_code = "static_data";
    sut.applies_to_amend = true;
    sut.applies_to_delete = false;
    sut.requires_commentary = true;
    sut.display_order = 10;
    sut.modified_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Change reason: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "static_data.front_office_error");
    CHECK(sut.description == "Front office data entry error");
    CHECK(sut.category_code == "static_data");
    CHECK(sut.applies_to_amend == true);
    CHECK(sut.applies_to_delete == false);
    CHECK(sut.requires_commentary == true);
    CHECK(sut.display_order == 10);
    CHECK(sut.modified_by == "admin");
}

TEST_CASE("change_reason_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    change_reason cr;
    cr.version = 1;
    cr.code = "system.new";
    cr.description = "Initial record creation";
    cr.category_code = "system";
    cr.modified_by = "system";

    std::vector<change_reason> reasons = {cr};
    auto table = convert_to_table(reasons);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Code"));
    CHECK(table.contains("Description"));
    CHECK(table.contains("system.new"));
    CHECK(table.contains("Initial record creation"));
}
