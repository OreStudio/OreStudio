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
#include <type_traits>
#include <catch2/catch_test_macros.hpp>
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.comms/messaging/subscription_protocol.hpp"
#include "ores.comms/messaging/handshake_protocol.hpp"

namespace {

const std::string tags("[messaging][traits]");

using namespace ores::comms::messaging;

}

// =============================================================================
// Concept satisfaction tests
// =============================================================================

TEST_CASE("has_message_traits concept is satisfied for subscribe_request",
          tags.c_str()) {
    STATIC_REQUIRE(has_message_traits<subscribe_request>);
}

TEST_CASE("has_message_traits concept is satisfied for unsubscribe_request",
          tags.c_str()) {
    STATIC_REQUIRE(has_message_traits<unsubscribe_request>);
}

TEST_CASE("has_message_traits concept is satisfied for handshake_request",
          tags.c_str()) {
    STATIC_REQUIRE(has_message_traits<handshake_request>);
}

TEST_CASE("has_message_traits concept is NOT satisfied for types without traits",
          tags.c_str()) {
    // notification_message doesn't have traits because it's a push message
    // without a request/response pair
    STATIC_REQUIRE_FALSE(has_message_traits<notification_message>);
}

// =============================================================================
// Traits correctness tests
// =============================================================================

TEST_CASE("message_traits for subscribe_request provides correct types",
          tags.c_str()) {
    using traits = message_traits<subscribe_request>;

    STATIC_REQUIRE(std::is_same_v<traits::request_type, subscribe_request>);
    STATIC_REQUIRE(std::is_same_v<traits::response_type, subscribe_response>);
    STATIC_REQUIRE(traits::request_message_type == message_type::subscribe_request);
}

TEST_CASE("message_traits for unsubscribe_request provides correct types",
          tags.c_str()) {
    using traits = message_traits<unsubscribe_request>;

    STATIC_REQUIRE(std::is_same_v<traits::request_type, unsubscribe_request>);
    STATIC_REQUIRE(std::is_same_v<traits::response_type, unsubscribe_response>);
    STATIC_REQUIRE(traits::request_message_type == message_type::unsubscribe_request);
}

TEST_CASE("message_traits for handshake_request provides correct types",
          tags.c_str()) {
    using traits = message_traits<handshake_request>;

    STATIC_REQUIRE(std::is_same_v<traits::request_type, handshake_request>);
    STATIC_REQUIRE(std::is_same_v<traits::response_type, handshake_response>);
    STATIC_REQUIRE(traits::request_message_type == message_type::handshake_request);
}

// =============================================================================
// Response type convention test
// =============================================================================

TEST_CASE("Response message_type follows request + 1 convention",
          tags.c_str()) {
    // Verify the convention that response type = request type + 1
    constexpr auto subscribe_req_value =
        static_cast<std::uint16_t>(message_type::subscribe_request);
    constexpr auto subscribe_resp_value =
        static_cast<std::uint16_t>(message_type::subscribe_response);
    STATIC_REQUIRE(subscribe_resp_value == subscribe_req_value + 1);

    constexpr auto unsubscribe_req_value =
        static_cast<std::uint16_t>(message_type::unsubscribe_request);
    constexpr auto unsubscribe_resp_value =
        static_cast<std::uint16_t>(message_type::unsubscribe_response);
    STATIC_REQUIRE(unsubscribe_resp_value == unsubscribe_req_value + 1);

    constexpr auto handshake_req_value =
        static_cast<std::uint16_t>(message_type::handshake_request);
    constexpr auto handshake_resp_value =
        static_cast<std::uint16_t>(message_type::handshake_response);
    STATIC_REQUIRE(handshake_resp_value == handshake_req_value + 1);
}
