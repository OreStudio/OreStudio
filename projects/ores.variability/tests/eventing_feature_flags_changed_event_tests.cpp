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
#include "ores.variability/eventing/feature_flags_changed_event.hpp"
#include "ores.eventing/domain/event_traits.hpp"
#include "ores.eventing/service/event_bus.hpp"

#include <thread>
#include <catch2/catch_test_macros.hpp>
#include <faker-cxx/faker.h> // IWYU pragma: keep.
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.variability.tests");
const std::string tags("[eventing]");

}

using ores::variability::eventing::feature_flags_changed_event;
using ores::eventing::domain::event_traits;
using ores::eventing::domain::has_event_traits;
using namespace ores::logging;

TEST_CASE("event_traits_feature_flags_changed_event", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Testing feature_flags_changed_event traits";

    REQUIRE(event_traits<feature_flags_changed_event>::name ==
            "ores.variability.feature_flags_changed");
    STATIC_REQUIRE(has_event_traits<feature_flags_changed_event>);
}

TEST_CASE("create_feature_flags_changed_event", tags) {
    auto lg(make_logger(test_suite));

    feature_flags_changed_event sut;
    sut.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Feature flags changed event created";

    // Verify timestamp is recent (within last second)
    const auto now = std::chrono::system_clock::now();
    CHECK(sut.timestamp <= now);
    CHECK(now - sut.timestamp < std::chrono::seconds(2)); // Allow a small delta for robustness
}

TEST_CASE("create_feature_flags_changed_event_with_past_timestamp", tags) {
    auto lg(make_logger(test_suite));

    feature_flags_changed_event sut;
    // Set timestamp to 1 hour ago
    sut.timestamp = std::chrono::system_clock::now() - std::chrono::hours(1);

    BOOST_LOG_SEV(lg, info) << "Feature flags changed event with past timestamp";

    const auto now = std::chrono::system_clock::now();
    const auto diff = std::chrono::duration_cast<std::chrono::minutes>(
        now - sut.timestamp).count();
    CHECK(diff >= 59);
    CHECK(diff <= 61);
}

TEST_CASE("create_feature_flags_changed_event_with_faker_timestamp", tags) {
    auto lg(make_logger(test_suite));

    for (int i = 0; i < 3; ++i) {
        feature_flags_changed_event sut;
        // Random hours in the past (0 to 168 hours = 1 week)
        const auto hours_ago = faker::number::integer(0, 168);
        sut.timestamp = std::chrono::system_clock::now() -
            std::chrono::hours(hours_ago);

        BOOST_LOG_SEV(lg, info) << "Feature flags changed event " << i
            << " with timestamp " << hours_ago << " hours ago";

        // Verify the timestamp is valid (in the past or now)
        CHECK(sut.timestamp <= std::chrono::system_clock::now());
    }
}

TEST_CASE("event_bus_feature_flags_changed_subscription", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    bool event_received = false;
    std::chrono::system_clock::time_point received_timestamp;

    auto sub = bus.subscribe<feature_flags_changed_event>(
        [&](const feature_flags_changed_event& e) {
            event_received = true;
            received_timestamp = e.timestamp;
        });

    feature_flags_changed_event event;
    event.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Publishing feature_flags_changed_event";
    bus.publish(event);

    CHECK(event_received);
    CHECK(received_timestamp == event.timestamp);
}

TEST_CASE("event_bus_multiple_feature_flags_changed_events", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    int events_received = 0;
    std::vector<std::chrono::system_clock::time_point> received_timestamps;

    auto sub = bus.subscribe<feature_flags_changed_event>(
        [&](const feature_flags_changed_event& e) {
            events_received++;
            received_timestamps.push_back(e.timestamp);
        });

    // Publish multiple events
    const int event_count = 5;
    std::vector<std::chrono::system_clock::time_point> sent_timestamps;
    for (int i = 0; i < event_count; ++i) {
        feature_flags_changed_event event;
        event.timestamp = std::chrono::system_clock::now() +
            std::chrono::milliseconds(i * 100);
        sent_timestamps.push_back(event.timestamp);

        BOOST_LOG_SEV(lg, info) << "Publishing event " << i;
        bus.publish(event);
    }

    CHECK(events_received == event_count);
    REQUIRE(received_timestamps.size() == sent_timestamps.size());
    for (size_t i = 0; i < sent_timestamps.size(); ++i) {
        CHECK(received_timestamps[i] == sent_timestamps[i]);
    }
}

TEST_CASE("event_bus_feature_flags_changed_multiple_subscribers", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    int subscriber1_count = 0;
    int subscriber2_count = 0;
    int subscriber3_count = 0;

    auto sub1 = bus.subscribe<feature_flags_changed_event>(
        [&](const feature_flags_changed_event&) {
            subscriber1_count++;
        });

    auto sub2 = bus.subscribe<feature_flags_changed_event>(
        [&](const feature_flags_changed_event&) {
            subscriber2_count++;
        });

    auto sub3 = bus.subscribe<feature_flags_changed_event>(
        [&](const feature_flags_changed_event&) {
            subscriber3_count++;
        });

    feature_flags_changed_event event;
    event.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Publishing to multiple subscribers";
    bus.publish(event);

    CHECK(subscriber1_count == 1);
    CHECK(subscriber2_count == 1);
    CHECK(subscriber3_count == 1);
}

TEST_CASE("event_bus_unsubscribe_feature_flags_changed", tags) {
    auto lg(make_logger(test_suite));

    ores::eventing::service::event_bus bus;
    int events_received = 0;

    {
        auto sub = bus.subscribe<feature_flags_changed_event>(
            [&](const feature_flags_changed_event&) {
                events_received++;
            });

        feature_flags_changed_event event1;
        event1.timestamp = std::chrono::system_clock::now();
        bus.publish(event1);

        CHECK(events_received == 1);
    } // sub goes out of scope, unsubscribing

    // Publish another event after unsubscribe
    feature_flags_changed_event event2;
    event2.timestamp = std::chrono::system_clock::now();
    bus.publish(event2);

    // Should still be 1 since we unsubscribed
    CHECK(events_received == 1);
}

TEST_CASE("feature_flags_changed_event_timestamp_comparison", tags) {
    auto lg(make_logger(test_suite));

    feature_flags_changed_event earlier;
    earlier.timestamp = std::chrono::system_clock::now();

    // Small delay to ensure different timestamps
    std::this_thread::sleep_for(std::chrono::milliseconds(10));

    feature_flags_changed_event later;
    later.timestamp = std::chrono::system_clock::now();

    BOOST_LOG_SEV(lg, info) << "Comparing event timestamps";

    CHECK(earlier.timestamp < later.timestamp);
    CHECK(later.timestamp > earlier.timestamp);
    CHECK(earlier.timestamp != later.timestamp);
}
