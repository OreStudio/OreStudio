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
#include "ores.security/authorization/grants.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[authorization][grants]");

using ores::security::authorization::grants;

bool covers(std::vector<std::string> granted, std::string_view required) {
    return grants(granted, required);
}

}

TEST_CASE("a_grant_covers_its_own_code", tags) {
    CHECK(covers({"refdata::parties:read"}, "refdata::parties:read"));
    CHECK_FALSE(covers({"refdata::parties:read"}, "refdata::parties:write"));
}

TEST_CASE("the_wildcard_covers_every_code", tags) {
    CHECK(covers({"*"}, "iam::tenants:create"));
}

TEST_CASE("a_component_wildcard_covers_that_component_only", tags) {
    CHECK(covers({"refdata::*"}, "refdata::parties:write"));
    CHECK_FALSE(covers({"refdata::*"}, "iam::accounts:create"));
    CHECK_FALSE(covers({"refdata::*"}, "refdatax::parties:read"));
}

TEST_CASE("an_empty_grant_covers_nothing", tags) {
    CHECK_FALSE(covers({}, "refdata::parties:read"));
}

TEST_CASE("the_grants_need_no_order", tags) {
    CHECK(covers({"trading::trades:read", "*"}, "iam::accounts:create"));
    CHECK(covers({"z::x:read", "refdata::*"}, "refdata::parties:read"));
}
