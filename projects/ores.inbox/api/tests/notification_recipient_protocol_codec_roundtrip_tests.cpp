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
#include "ores.inbox.api/domain/notification_recipient_codec.hpp"
#include "ores.inbox.api/messaging/notification_recipient_protocol.hpp"
#include "ores.testing/wire_roundtrip.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <catch2/catch_test_macros.hpp>

TEST_CASE("ores::inbox::messaging::notification_recipient_key", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::notification_recipient_key>();
}

TEST_CASE("ores::inbox::messaging::notification_recipient_write", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::notification_recipient_write>();
}

TEST_CASE("ores::inbox::messaging::notification_recipient_change", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::notification_recipient_change>();
}

TEST_CASE("ores::inbox::messaging::notification_recipient_removal",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::notification_recipient_removal>();
}

TEST_CASE("ores::inbox::messaging::notification_recipient_lookup", "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::notification_recipient_lookup>();
}

TEST_CASE("ores::inbox::messaging::notification_recipients_filter",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<ores::inbox::messaging::notification_recipients_filter>();
}

TEST_CASE("ores::inbox::messaging::list_notification_recipients_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::list_notification_recipients_request>();
}

TEST_CASE("ores::inbox::messaging::list_notification_recipients_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::list_notification_recipients_response>();
}

TEST_CASE("ores::inbox::messaging::get_notification_recipient_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::get_notification_recipient_request>();
}

TEST_CASE("ores::inbox::messaging::get_notification_recipient_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::get_notification_recipient_response>();
}

TEST_CASE("ores::inbox::messaging::get_many_notification_recipients_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::get_many_notification_recipients_request>();
}

TEST_CASE("ores::inbox::messaging::get_many_notification_recipients_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::get_many_notification_recipients_response>();
}

TEST_CASE("ores::inbox::messaging::put_notification_recipient_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::put_notification_recipient_request>();
}

TEST_CASE("ores::inbox::messaging::put_notification_recipient_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::put_notification_recipient_response>();
}

TEST_CASE("ores::inbox::messaging::put_many_notification_recipients_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::put_many_notification_recipients_request>();
}

TEST_CASE("ores::inbox::messaging::put_many_notification_recipients_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::put_many_notification_recipients_response>();
}

TEST_CASE("ores::inbox::messaging::delete_notification_recipient_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::delete_notification_recipient_request>();
}

TEST_CASE("ores::inbox::messaging::delete_notification_recipient_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::delete_notification_recipient_response>();
}

TEST_CASE("ores::inbox::messaging::delete_many_notification_recipients_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::delete_many_notification_recipients_request>();
}

TEST_CASE("ores::inbox::messaging::delete_many_notification_recipients_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::delete_many_notification_recipients_response>();
}

TEST_CASE("ores::inbox::messaging::list_by_account_id_notification_recipients_request",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::list_by_account_id_notification_recipients_request>();
}

TEST_CASE("ores::inbox::messaging::list_by_account_id_notification_recipients_response",
          "[codec][roundtrip][protocol]") {
    ores::testing::check_wire_roundtrip<
        ores::inbox::messaging::list_by_account_id_notification_recipients_response>();
}
