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

#include "ores.iam.core/service/tenant_code_check.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[tenant]");

using ores::iam::service::check_tenant_code;
using ores::iam::service::tenant_code_max_length;

}

TEST_CASE("check_tenant_code_accepts_the_shape_the_deployment_states", tags) {
    CHECK(check_tenant_code("northwind").empty());
    CHECK(check_tenant_code("barclays_plc").empty());
    CHECK(check_tenant_code("acme2").empty());
    CHECK(check_tenant_code(std::string(tenant_code_max_length, 'a')).empty());
}

TEST_CASE("check_tenant_code_refuses_a_code_that_is_not_the_stated_shape", tags) {
    CHECK_FALSE(check_tenant_code("").empty());
    CHECK_FALSE(check_tenant_code("Northwind").empty());
    CHECK_FALSE(check_tenant_code("2northwind").empty());
    CHECK_FALSE(check_tenant_code("_northwind").empty());
    CHECK_FALSE(check_tenant_code("north-wind").empty());
    CHECK_FALSE(check_tenant_code("north wind").empty());
    CHECK_FALSE(check_tenant_code("northwind!").empty());
    CHECK_FALSE(check_tenant_code(std::string(tenant_code_max_length + 1, 'a')).empty());
}

TEST_CASE("check_tenant_code_states_which_rule_a_code_broke", tags) {
    CHECK(check_tenant_code("Northwind").find("lowercase letter") != std::string::npos);
    CHECK(check_tenant_code("north-wind").find("lowercase letters, digits") != std::string::npos);
    CHECK(check_tenant_code(std::string(tenant_code_max_length + 1, 'a'))
              .find(std::to_string(tenant_code_max_length)) != std::string::npos);
}
