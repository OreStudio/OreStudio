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
#include "ores.dq/domain/change_reason_category.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/change_reason_category_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/change_reason_category_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::change_reason_category;
using namespace ores::logging;

TEST_CASE("create_change_reason_category_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    change_reason_category sut;
    sut.version = 1;
    sut.code = "static_data";
    sut.description = "Static/reference data changes";
    sut.recorded_by = "admin";
    BOOST_LOG_SEV(lg, info) << "Change reason category: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "static_data");
    CHECK(sut.description == "Static/reference data changes");
}

TEST_CASE("change_reason_category_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    change_reason_category cat;
    cat.version = 1;
    cat.code = "system";
    cat.description = "System-level operations";
    cat.recorded_by = "system";

    std::vector<change_reason_category> categories = {cat};
    auto table = convert_to_table(categories);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.find("system") != std::string::npos);
}
