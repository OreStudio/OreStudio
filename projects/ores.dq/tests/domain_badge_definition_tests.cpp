/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.dq/domain/badge_definition.hpp"

#include <vector>
#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.dq/domain/badge_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq/domain/badge_definition_table.hpp"

namespace {

const std::string_view test_suite("ores.dq.tests");
const std::string tags("[domain]");

}

using ores::dq::domain::badge_definition;
using ores::dq::domain::convert_to_table;
using namespace ores::logging;

TEST_CASE("create_badge_definition_with_valid_fields", tags) {
    auto lg(make_logger(test_suite));

    badge_definition sut;
    sut.version = 1;
    sut.code = "login_online";
    sut.name = "Online";
    sut.description = "User recently logged in";
    sut.background_colour = "#22c55e";
    sut.text_colour = "#ffffff";
    sut.severity_code = "success";
    sut.display_order = 1;
    sut.modified_by = "system";
    BOOST_LOG_SEV(lg, info) << "Badge definition: " << sut;

    CHECK(sut.version == 1);
    CHECK(sut.code == "login_online");
    CHECK(sut.name == "Online");
    CHECK(sut.description == "User recently logged in");
    CHECK(sut.background_colour == "#22c55e");
    CHECK(sut.text_colour == "#ffffff");
    CHECK(sut.severity_code == "success");
    CHECK(sut.display_order == 1);
    CHECK(sut.modified_by == "system");
}

TEST_CASE("badge_definition_default_values", tags) {
    auto lg(make_logger(test_suite));

    badge_definition sut;
    BOOST_LOG_SEV(lg, info) << "Default badge definition: " << sut;

    CHECK(sut.version == 0);
    CHECK(sut.code.empty());
    CHECK(sut.name.empty());
    CHECK(sut.background_colour.empty());
    CHECK(sut.text_colour.empty());
    CHECK(sut.display_order == 0);
}

TEST_CASE("badge_definition_convert_single_to_table", tags) {
    auto lg(make_logger(test_suite));

    badge_definition bd;
    bd.code = "login_online";
    bd.name = "Online";
    bd.background_colour = "#22c55e";
    bd.text_colour = "#ffffff";
    bd.severity_code = "success";
    bd.display_order = 1;
    bd.modified_by = "system";

    std::vector<badge_definition> items = {bd};
    const auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("Code"));
    CHECK(table.contains("login_online"));
    CHECK(table.contains("Online"));
    CHECK(table.contains("#22c55e"));
    CHECK(table.contains("#ffffff"));
}

TEST_CASE("badge_definition_convert_multiple_to_table", tags) {
    auto lg(make_logger(test_suite));

    badge_definition bd1;
    bd1.code = "login_online";
    bd1.name = "Online";
    bd1.background_colour = "#22c55e";
    bd1.text_colour = "#ffffff";
    bd1.display_order = 1;

    badge_definition bd2;
    bd2.code = "account_locked";
    bd2.name = "Locked";
    bd2.background_colour = "#ef4444";
    bd2.text_colour = "#ffffff";
    bd2.display_order = 2;

    std::vector<badge_definition> items = {bd1, bd2};
    const auto table = convert_to_table(items);

    BOOST_LOG_SEV(lg, info) << "Table output:\n" << table;

    CHECK(!table.empty());
    CHECK(table.contains("login_online"));
    CHECK(table.contains("account_locked"));
    CHECK(table.contains("#22c55e"));
    CHECK(table.contains("#ef4444"));
}
