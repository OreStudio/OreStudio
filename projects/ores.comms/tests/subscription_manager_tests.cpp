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
#include "ores.comms/service/subscription_manager.hpp"

namespace {

const std::string tags("[service][subscription]");

using ores::comms::service::subscription_manager;

TEST_CASE("subscription_manager starts empty", tags) {
    subscription_manager mgr;

    REQUIRE(mgr.session_count() == 0);
    REQUIRE(mgr.subscriber_count("test.event") == 0);
}

TEST_CASE("subscription_manager can register and unregister sessions", tags) {
    subscription_manager mgr;

    // Register a session with a no-op callback
    mgr.register_session("session1", [](const std::string&, auto) { return true; });

    REQUIRE(mgr.session_count() == 1);

    mgr.unregister_session("session1");

    REQUIRE(mgr.session_count() == 0);
}

TEST_CASE("subscription_manager can subscribe session to event", tags) {
    subscription_manager mgr;

    mgr.register_session("session1", [](const std::string&, auto) { return true; });

    REQUIRE(mgr.subscribe("session1", "test.event"));
    REQUIRE(mgr.subscriber_count("test.event") == 1);

    auto subs = mgr.get_subscriptions("session1");
    REQUIRE(subs.size() == 1);
    REQUIRE(subs[0] == "test.event");
}

TEST_CASE("subscription_manager subscribe fails for unknown session", tags) {
    subscription_manager mgr;

    REQUIRE_FALSE(mgr.subscribe("unknown", "test.event"));
    REQUIRE(mgr.subscriber_count("test.event") == 0);
}

TEST_CASE("subscription_manager can unsubscribe session from event", tags) {
    subscription_manager mgr;

    mgr.register_session("session1", [](const std::string&, auto) { return true; });
    mgr.subscribe("session1", "test.event");

    REQUIRE(mgr.unsubscribe("session1", "test.event"));
    REQUIRE(mgr.subscriber_count("test.event") == 0);

    auto subs = mgr.get_subscriptions("session1");
    REQUIRE(subs.empty());
}

TEST_CASE("subscription_manager unsubscribe returns false when not subscribed", tags) {
    subscription_manager mgr;

    mgr.register_session("session1", [](const std::string&, auto) { return true; });

    REQUIRE_FALSE(mgr.unsubscribe("session1", "test.event"));
}

TEST_CASE("subscription_manager unregistering session removes subscriptions", tags) {
    subscription_manager mgr;

    mgr.register_session("session1", [](const std::string&, auto) { return true; });
    mgr.subscribe("session1", "event1");
    mgr.subscribe("session1", "event2");

    REQUIRE(mgr.subscriber_count("event1") == 1);
    REQUIRE(mgr.subscriber_count("event2") == 1);

    mgr.unregister_session("session1");

    REQUIRE(mgr.subscriber_count("event1") == 0);
    REQUIRE(mgr.subscriber_count("event2") == 0);
}

TEST_CASE("subscription_manager notify invokes callback for subscribers", tags) {
    subscription_manager mgr;

    int callback_count = 0;
    std::string received_event_type;
    std::chrono::system_clock::time_point received_timestamp;

    mgr.register_session("session1",
        [&](const std::string& event_type, auto ts) {
            ++callback_count;
            received_event_type = event_type;
            received_timestamp = ts;
            return true;
        });

    mgr.subscribe("session1", "test.event");

    auto now = std::chrono::system_clock::now();
    auto notified = mgr.notify("test.event", now);

    REQUIRE(notified == 1);
    REQUIRE(callback_count == 1);
    REQUIRE(received_event_type == "test.event");
    REQUIRE(received_timestamp == now);
}

TEST_CASE("subscription_manager notify does not invoke for unsubscribed events", tags) {
    subscription_manager mgr;

    int callback_count = 0;
    mgr.register_session("session1",
        [&](const std::string&, auto) {
            ++callback_count;
            return true;
        });

    mgr.subscribe("session1", "subscribed.event");

    auto notified = mgr.notify("other.event", std::chrono::system_clock::now());

    REQUIRE(notified == 0);
    REQUIRE(callback_count == 0);
}

TEST_CASE("subscription_manager notify returns count of successful notifications", tags) {
    subscription_manager mgr;

    int session1_calls = 0;
    int session2_calls = 0;

    mgr.register_session("session1",
        [&](const std::string&, auto) {
            ++session1_calls;
            return true;
        });

    mgr.register_session("session2",
        [&](const std::string&, auto) {
            ++session2_calls;
            return false; // Simulate failed notification
        });

    mgr.subscribe("session1", "test.event");
    mgr.subscribe("session2", "test.event");

    auto notified = mgr.notify("test.event", std::chrono::system_clock::now());

    // Only session1 succeeded
    REQUIRE(notified == 1);
    REQUIRE(session1_calls == 1);
    REQUIRE(session2_calls == 1);
}

TEST_CASE("subscription_manager multiple sessions can subscribe to same event", tags) {
    subscription_manager mgr;

    int total_calls = 0;

    mgr.register_session("session1",
        [&](const std::string&, auto) { ++total_calls; return true; });
    mgr.register_session("session2",
        [&](const std::string&, auto) { ++total_calls; return true; });
    mgr.register_session("session3",
        [&](const std::string&, auto) { ++total_calls; return true; });

    mgr.subscribe("session1", "shared.event");
    mgr.subscribe("session2", "shared.event");
    mgr.subscribe("session3", "shared.event");

    REQUIRE(mgr.subscriber_count("shared.event") == 3);

    auto notified = mgr.notify("shared.event", std::chrono::system_clock::now());

    REQUIRE(notified == 3);
    REQUIRE(total_calls == 3);
}

TEST_CASE("subscription_manager session can subscribe to multiple events", tags) {
    subscription_manager mgr;

    int total_calls = 0;

    mgr.register_session("session1",
        [&](const std::string&, auto) { ++total_calls; return true; });

    mgr.subscribe("session1", "event1");
    mgr.subscribe("session1", "event2");
    mgr.subscribe("session1", "event3");

    auto subs = mgr.get_subscriptions("session1");
    REQUIRE(subs.size() == 3);

    mgr.notify("event1", std::chrono::system_clock::now());
    mgr.notify("event2", std::chrono::system_clock::now());
    mgr.notify("event3", std::chrono::system_clock::now());

    REQUIRE(total_calls == 3);
}

TEST_CASE("subscription_manager re-registering session updates callback", tags) {
    subscription_manager mgr;

    int first_callback_count = 0;
    int second_callback_count = 0;

    mgr.register_session("session1",
        [&](const std::string&, auto) { ++first_callback_count; return true; });
    mgr.subscribe("session1", "test.event");

    // Re-register with new callback
    mgr.register_session("session1",
        [&](const std::string&, auto) { ++second_callback_count; return true; });

    mgr.notify("test.event", std::chrono::system_clock::now());

    // Only second callback should be invoked
    REQUIRE(first_callback_count == 0);
    REQUIRE(second_callback_count == 1);
}

TEST_CASE("subscription_manager duplicate subscription is idempotent", tags) {
    subscription_manager mgr;

    mgr.register_session("session1", [](const std::string&, auto) { return true; });

    REQUIRE(mgr.subscribe("session1", "test.event"));
    REQUIRE(mgr.subscribe("session1", "test.event")); // Should succeed (idempotent)

    // Should still only be counted once
    REQUIRE(mgr.subscriber_count("test.event") == 1);

    auto subs = mgr.get_subscriptions("session1");
    REQUIRE(subs.size() == 1);
}

TEST_CASE("subscription_manager get_subscriptions returns empty for unknown session", tags) {
    subscription_manager mgr;

    auto subs = mgr.get_subscriptions("unknown");
    REQUIRE(subs.empty());
}

TEST_CASE("subscription_manager callback exception is handled gracefully", tags) {
    subscription_manager mgr;

    int good_callback_count = 0;

    mgr.register_session("session1",
        [](const std::string&, auto) -> bool {
            throw std::runtime_error("test exception");
        });

    mgr.register_session("session2",
        [&](const std::string&, auto) {
            ++good_callback_count;
            return true;
        });

    mgr.subscribe("session1", "test.event");
    mgr.subscribe("session2", "test.event");

    // Should not throw, and should continue to invoke other callbacks
    auto notified = mgr.notify("test.event", std::chrono::system_clock::now());

    REQUIRE(notified == 1); // Only session2 succeeded
    REQUIRE(good_callback_count == 1);
}

}
