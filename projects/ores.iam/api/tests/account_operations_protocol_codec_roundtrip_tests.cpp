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
#include "ores.iam.api/domain/account_codec.hpp"
#include "ores.iam.api/domain/account_contact_information_codec.hpp"
#include "ores.iam.api/domain/login_info_codec.hpp"
#include "ores.iam.api/domain/session_codec.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::iam::messaging::publish_accounts_from_dq_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::publish_accounts_from_dq_request>();
}

TEST_CASE("ores::iam::messaging::save_account_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::save_account_request>();
}

TEST_CASE("ores::iam::messaging::update_account_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::update_account_request>();
}

TEST_CASE("ores::iam::messaging::update_account_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::update_account_response>();
}

TEST_CASE("ores::iam::messaging::save_account_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::save_account_response>();
}

TEST_CASE("ores::iam::messaging::delete_account_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::delete_account_request>();
}

TEST_CASE("ores::iam::messaging::delete_account_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::delete_account_response>();
}

TEST_CASE("ores::iam::messaging::account_operation_result", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::account_operation_result>();
}

TEST_CASE("ores::iam::messaging::lock_account_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::lock_account_request>();
}

TEST_CASE("ores::iam::messaging::lock_account_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::lock_account_response>();
}

TEST_CASE("ores::iam::messaging::unlock_account_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::unlock_account_request>();
}

TEST_CASE("ores::iam::messaging::unlock_account_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::unlock_account_response>();
}

TEST_CASE("ores::iam::messaging::reset_password_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::reset_password_request>();
}

TEST_CASE("ores::iam::messaging::reset_password_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::reset_password_response>();
}

TEST_CASE("ores::iam::messaging::change_password_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::change_password_request>();
}

TEST_CASE("ores::iam::messaging::change_password_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::change_password_response>();
}

TEST_CASE("ores::iam::messaging::update_my_email_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::update_my_email_request>();
}

TEST_CASE("ores::iam::messaging::update_my_email_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::update_my_email_response>();
}

TEST_CASE("ores::iam::messaging::set_my_default_party_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::set_my_default_party_request>();
}

TEST_CASE("ores::iam::messaging::set_my_default_party_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::set_my_default_party_response>();
}

TEST_CASE("ores::iam::messaging::select_party_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::select_party_request>();
}

TEST_CASE("ores::iam::messaging::switch_party_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::switch_party_request>();
}

TEST_CASE("ores::iam::messaging::select_party_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::select_party_response>();
}

TEST_CASE("ores::iam::messaging::change_password_request_typed", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::change_password_request_typed>();
}

TEST_CASE("ores::iam::messaging::update_self_account_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::update_self_account_request>();
}

TEST_CASE("ores::iam::messaging::update_self_account_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::update_self_account_response>();
}

TEST_CASE("ores::iam::messaging::update_self_account_contact_information_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::iam::messaging::update_self_account_contact_information_request>();
}

TEST_CASE("ores::iam::messaging::update_self_account_contact_information_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::iam::messaging::update_self_account_contact_information_response>();
}

TEST_CASE("ores::iam::messaging::get_my_account_contact_information_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::iam::messaging::get_my_account_contact_information_request>();
}

TEST_CASE("ores::iam::messaging::get_my_account_contact_information_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::iam::messaging::get_my_account_contact_information_response>();
}

TEST_CASE("ores::iam::messaging::get_my_account_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_account_request>();
}

TEST_CASE("ores::iam::messaging::get_my_account_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_account_response>();
}

TEST_CASE("ores::iam::messaging::get_my_login_info_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_login_info_request>();
}

TEST_CASE("ores::iam::messaging::get_my_login_info_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_login_info_response>();
}

TEST_CASE("ores::iam::messaging::get_my_sessions_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_sessions_request>();
}

TEST_CASE("ores::iam::messaging::get_my_sessions_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_sessions_response>();
}

TEST_CASE("ores::iam::messaging::my_party", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::my_party>();
}

TEST_CASE("ores::iam::messaging::get_my_parties_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_parties_request>();
}

TEST_CASE("ores::iam::messaging::get_my_parties_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_my_parties_response>();
}

TEST_CASE("ores::iam::messaging::set_reporting_line_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::set_reporting_line_request>();
}

TEST_CASE("ores::iam::messaging::set_reporting_line_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::set_reporting_line_response>();
}

TEST_CASE("ores::iam::messaging::reporting_tree_party", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::reporting_tree_party>();
}

TEST_CASE("ores::iam::messaging::reporting_tree_node", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::reporting_tree_node>();
}

TEST_CASE("ores::iam::messaging::get_reporting_tree_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_reporting_tree_request>();
}

TEST_CASE("ores::iam::messaging::get_reporting_tree_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::get_reporting_tree_response>();
}

TEST_CASE("ores::iam::messaging::attach_account_pictures_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::attach_account_pictures_request>();
}

TEST_CASE("ores::iam::messaging::attach_account_pictures_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::iam::messaging::attach_account_pictures_response>();
}
