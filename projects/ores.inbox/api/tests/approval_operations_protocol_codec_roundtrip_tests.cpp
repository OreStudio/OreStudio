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
#include "ores.inbox.api/domain/approval_request_codec.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::inbox::messaging::raise_approval_request_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::raise_approval_request_request>();
}

TEST_CASE("ores::inbox::messaging::raise_approval_request_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::raise_approval_request_response>();
}

TEST_CASE("ores::inbox::messaging::withdraw_approval_request_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::withdraw_approval_request_request>();
}

TEST_CASE("ores::inbox::messaging::withdraw_approval_request_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::withdraw_approval_request_response>();
}

TEST_CASE("ores::inbox::messaging::decide_approval_request_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::decide_approval_request_request>();
}

TEST_CASE("ores::inbox::messaging::decide_approval_request_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::decide_approval_request_response>();
}

TEST_CASE("ores::inbox::messaging::list_approval_queue_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::list_approval_queue_request>();
}

TEST_CASE("ores::inbox::messaging::list_approval_queue_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::list_approval_queue_response>();
}

TEST_CASE("ores::inbox::messaging::list_my_approval_requests_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::list_my_approval_requests_request>();
}

TEST_CASE("ores::inbox::messaging::list_my_approval_requests_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::list_my_approval_requests_response>();
}

TEST_CASE("ores::inbox::messaging::get_approval_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::get_approval_request>();
}

TEST_CASE("ores::inbox::messaging::get_approval_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::get_approval_response>();
}

TEST_CASE("ores::inbox::messaging::expire_overdue_approvals_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::expire_overdue_approvals_request>();
}

TEST_CASE("ores::inbox::messaging::expire_overdue_approvals_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::expire_overdue_approvals_response>();
}

TEST_CASE("ores::inbox::messaging::remind_expiring_approvals_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::remind_expiring_approvals_request>();
}

TEST_CASE("ores::inbox::messaging::remind_expiring_approvals_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::remind_expiring_approvals_response>();
}

TEST_CASE("ores::inbox::messaging::retry_stalled_applies_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::retry_stalled_applies_request>();
}

TEST_CASE("ores::inbox::messaging::retry_stalled_applies_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::retry_stalled_applies_response>();
}

TEST_CASE("ores::inbox::messaging::approval_request_field", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::approval_request_field>();
}

TEST_CASE("ores::inbox::messaging::approval_request_version", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::approval_request_version>();
}

TEST_CASE("ores::inbox::messaging::get_approval_history_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::get_approval_history_request>();
}

TEST_CASE("ores::inbox::messaging::get_approval_history_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::get_approval_history_response>();
}

TEST_CASE("ores::inbox::messaging::approval_part_progress", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::approval_part_progress>();
}

TEST_CASE("ores::inbox::messaging::get_approval_parts_request", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::get_approval_parts_request>();
}

TEST_CASE("ores::inbox::messaging::get_approval_parts_response", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::get_approval_parts_response>();
}
