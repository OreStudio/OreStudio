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
#include <chrono>
#include <catch2/catch_test_macros.hpp>
#include <boost/asio/io_context.hpp>
#include "ores.testing/run_coroutine_test.hpp"
#include "ores.comms/messaging/subscription_protocol.hpp"
#include "ores.comms/service/subscription_manager.hpp"
#include "ores.comms/service/subscription_handler.hpp"

namespace {

const std::string tags("[messaging][subscription]");

using namespace ores::comms::messaging;

// ============================================================================
// subscribe_request tests
// ============================================================================

TEST_CASE("subscribe_request roundtrip serialization", tags) {
    subscribe_request original;
    original.event_type = "ores.risk.currency_changed";

    auto bytes = original.serialize();
    auto result = subscribe_request::deserialize(bytes);

    REQUIRE(result.has_value());
    REQUIRE(result->event_type == original.event_type);
}

TEST_CASE("subscribe_request deserialize fails on empty data", tags) {
    std::span<const std::byte> empty;
    auto result = subscribe_request::deserialize(empty);

    REQUIRE_FALSE(result.has_value());
}

TEST_CASE("subscribe_request stream operator", tags) {
    subscribe_request req;
    req.event_type = "test.event";

    std::ostringstream oss;
    oss << req;

    REQUIRE(oss.str().find("test.event") != std::string::npos);
}

// ============================================================================
// subscribe_response tests
// ============================================================================

TEST_CASE("subscribe_response roundtrip serialization success case", tags) {
    subscribe_response original;
    original.success = true;
    original.message = "Successfully subscribed";

    auto bytes = original.serialize();
    auto result = subscribe_response::deserialize(bytes);

    REQUIRE(result.has_value());
    REQUIRE(result->success == original.success);
    REQUIRE(result->message == original.message);
}

TEST_CASE("subscribe_response roundtrip serialization failure case", tags) {
    subscribe_response original;
    original.success = false;
    original.message = "Subscription failed: unknown event type";

    auto bytes = original.serialize();
    auto result = subscribe_response::deserialize(bytes);

    REQUIRE(result.has_value());
    REQUIRE(result->success == false);
    REQUIRE(result->message == original.message);
}

TEST_CASE("subscribe_response stream operator", tags) {
    subscribe_response resp;
    resp.success = true;
    resp.message = "OK";

    std::ostringstream oss;
    oss << resp;

    REQUIRE(oss.str().find("success=1") != std::string::npos);
}

// ============================================================================
// unsubscribe_request tests
// ============================================================================

TEST_CASE("unsubscribe_request roundtrip serialization", tags) {
    unsubscribe_request original;
    original.event_type = "ores.iam.account_changed";

    auto bytes = original.serialize();
    auto result = unsubscribe_request::deserialize(bytes);

    REQUIRE(result.has_value());
    REQUIRE(result->event_type == original.event_type);
}

TEST_CASE("unsubscribe_request stream operator", tags) {
    unsubscribe_request req;
    req.event_type = "test.event";

    std::ostringstream oss;
    oss << req;

    REQUIRE(oss.str().find("test.event") != std::string::npos);
}

// ============================================================================
// unsubscribe_response tests
// ============================================================================

TEST_CASE("unsubscribe_response roundtrip serialization", tags) {
    unsubscribe_response original;
    original.success = true;
    original.message = "Unsubscribed successfully";

    auto bytes = original.serialize();
    auto result = unsubscribe_response::deserialize(bytes);

    REQUIRE(result.has_value());
    REQUIRE(result->success == original.success);
    REQUIRE(result->message == original.message);
}

// ============================================================================
// notification_message tests
// ============================================================================

TEST_CASE("notification_message roundtrip serialization", tags) {
    notification_message original;
    original.event_type = "ores.risk.currency_changed";
    original.timestamp = std::chrono::system_clock::now();

    auto bytes = original.serialize();
    auto result = notification_message::deserialize(bytes);

    REQUIRE(result.has_value());
    REQUIRE(result->event_type == original.event_type);

    // Timestamps should match at millisecond precision (that's how we serialize)
    auto orig_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        original.timestamp.time_since_epoch()).count();
    auto result_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        result->timestamp.time_since_epoch()).count();
    REQUIRE(result_ms == orig_ms);
}

TEST_CASE("notification_message preserves timestamp across serialization", tags) {
    notification_message original;
    original.event_type = "test.event";

    // Use a specific timestamp
    auto epoch_ms = std::chrono::milliseconds(1700000000000); // Some fixed timestamp
    original.timestamp = std::chrono::system_clock::time_point(epoch_ms);

    auto bytes = original.serialize();
    auto result = notification_message::deserialize(bytes);

    REQUIRE(result.has_value());
    REQUIRE(result->timestamp == original.timestamp);
}

TEST_CASE("notification_message stream operator", tags) {
    notification_message msg;
    msg.event_type = "test.event";
    msg.timestamp = std::chrono::system_clock::time_point(
        std::chrono::milliseconds(1234567890));

    std::ostringstream oss;
    oss << msg;

    REQUIRE(oss.str().find("test.event") != std::string::npos);
    REQUIRE(oss.str().find("1234567890") != std::string::npos);
}

// ============================================================================
// subscription_handler tests
// ============================================================================

TEST_CASE("subscription_handler handles subscribe request for registered session", tags) {
    boost::asio::io_context io_ctx;
    ores::testing::run_coroutine_test(io_ctx, []()
        -> boost::asio::awaitable<void> {
        auto mgr = std::make_shared<ores::comms::service::subscription_manager>();
        ores::comms::service::subscription_handler handler(mgr);

        // Register the session first
        mgr->register_session("192.168.1.1:12345",
            [](const std::string&, auto) { return true; });

        // Create and serialize a subscribe request
        subscribe_request req;
        req.event_type = "test.event";
        auto payload = req.serialize();

        auto result = co_await handler.handle_message(
            message_type::subscribe_request,
            payload,
            "192.168.1.1:12345");

        REQUIRE(result.has_value());

        // Deserialize response
        auto resp = subscribe_response::deserialize(*result);
        REQUIRE(resp.has_value());
        REQUIRE(resp->success == true);

        // Verify subscription was recorded
        REQUIRE(mgr->subscriber_count("test.event") == 1);
    });
}

TEST_CASE("subscription_handler handles subscribe request for unregistered session", tags) {
    boost::asio::io_context io_ctx;
    ores::testing::run_coroutine_test(io_ctx, []()
        -> boost::asio::awaitable<void> {
        auto mgr = std::make_shared<ores::comms::service::subscription_manager>();
        ores::comms::service::subscription_handler handler(mgr);

        // Don't register the session

        subscribe_request req;
        req.event_type = "test.event";
        auto payload = req.serialize();

        auto result = co_await handler.handle_message(
            message_type::subscribe_request,
            payload,
            "192.168.1.1:12345");

        REQUIRE(result.has_value());

        auto resp = subscribe_response::deserialize(*result);
        REQUIRE(resp.has_value());
        REQUIRE(resp->success == false);
        REQUIRE(mgr->subscriber_count("test.event") == 0);
    });
}

TEST_CASE("subscription_handler handles unsubscribe request", tags) {
    boost::asio::io_context io_ctx;
    ores::testing::run_coroutine_test(io_ctx, []()
        -> boost::asio::awaitable<void> {
        auto mgr = std::make_shared<ores::comms::service::subscription_manager>();
        ores::comms::service::subscription_handler handler(mgr);

        // Register and subscribe
        mgr->register_session("192.168.1.1:12345",
            [](const std::string&, auto) { return true; });
        mgr->subscribe("192.168.1.1:12345", "test.event");

        REQUIRE(mgr->subscriber_count("test.event") == 1);

        // Unsubscribe
        unsubscribe_request req;
        req.event_type = "test.event";
        auto payload = req.serialize();

        auto result = co_await handler.handle_message(
            message_type::unsubscribe_request,
            payload,
            "192.168.1.1:12345");

        REQUIRE(result.has_value());

        auto resp = unsubscribe_response::deserialize(*result);
        REQUIRE(resp.has_value());
        REQUIRE(resp->success == true);
        REQUIRE(mgr->subscriber_count("test.event") == 0);
    });
}

TEST_CASE("subscription_handler returns error for unknown message type", tags) {
    boost::asio::io_context io_ctx;
    ores::testing::run_coroutine_test(io_ctx, []()
        -> boost::asio::awaitable<void> {
        auto mgr = std::make_shared<ores::comms::service::subscription_manager>();
        ores::comms::service::subscription_handler handler(mgr);

        std::vector<std::byte> payload;

        auto result = co_await handler.handle_message(
            message_type::ping, // Wrong message type for this handler
            payload,
            "192.168.1.1:12345");

        REQUIRE_FALSE(result.has_value());
        REQUIRE(result.error() == error_code::invalid_message_type);
    });
}

}
