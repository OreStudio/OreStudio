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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol_codec_roundtrip_tests.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.api/domain/role_codec.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::iam::messaging::assign_role_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::assign_role_request>();
}

TEST_CASE("ores::iam::messaging::assign_role_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::assign_role_response>();
}

TEST_CASE("ores::iam::messaging::assign_role_by_name_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::assign_role_by_name_response>();
}

TEST_CASE("ores::iam::messaging::assign_role_by_name_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::assign_role_by_name_request>();
}

TEST_CASE("ores::iam::messaging::revoke_role_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::revoke_role_request>();
}

TEST_CASE("ores::iam::messaging::revoke_role_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::revoke_role_response>();
}

TEST_CASE("ores::iam::messaging::revoke_role_by_name_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::revoke_role_by_name_response>();
}

TEST_CASE("ores::iam::messaging::revoke_role_by_name_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::revoke_role_by_name_request>();
}

TEST_CASE("ores::iam::messaging::get_account_roles_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_account_roles_request>();
}

TEST_CASE("ores::iam::messaging::get_my_roles_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_roles_request>();
}

TEST_CASE("ores::iam::messaging::permission_resource_row", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::permission_resource_row>();
}

TEST_CASE("ores::iam::messaging::permission_area_count", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::permission_area_count>();
}

TEST_CASE("ores::iam::messaging::list_account_permissions_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::list_account_permissions_request>();
}

TEST_CASE("ores::iam::messaging::list_my_permissions_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::list_my_permissions_request>();
}

TEST_CASE("ores::iam::messaging::permission_page_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::permission_page_response>();
}

TEST_CASE("ores::iam::messaging::list_role_permissions_page_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::list_role_permissions_page_request>();
}

TEST_CASE("ores::iam::messaging::role_page_row", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::role_page_row>();
}

TEST_CASE("ores::iam::messaging::list_roles_page_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::list_roles_page_request>();
}

TEST_CASE("ores::iam::messaging::role_page_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::role_page_response>();
}

TEST_CASE("ores::iam::messaging::role_holder", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::role_holder>();
}

TEST_CASE("ores::iam::messaging::list_role_holders_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::list_role_holders_request>();
}

TEST_CASE("ores::iam::messaging::role_holders_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::role_holders_response>();
}

TEST_CASE("ores::iam::messaging::account_role_access", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::account_role_access>();
}

TEST_CASE("ores::iam::messaging::get_account_roles_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_account_roles_response>();
}

TEST_CASE("ores::iam::messaging::get_role_permissions_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_role_permissions_request>();
}

TEST_CASE("ores::iam::messaging::get_role_permissions_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_role_permissions_response>();
}

TEST_CASE("ores::iam::messaging::put_role_permissions_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::put_role_permissions_request>();
}

TEST_CASE("ores::iam::messaging::suggest_role_commands_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::suggest_role_commands_request>();
}

TEST_CASE("ores::iam::messaging::suggest_role_commands_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::suggest_role_commands_response>();
}
