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
#include "ores.eventing/service/event_bus.hpp"

#include <catch2/catch_test_macros.hpp>
#include <atomic>
#include <thread>
#include <chrono>
#include <string>
#include <vector>

namespace {

const std::string tags("[event_bus]");

// Test event types
struct test_event_a {
    int value;
    std::string message;
};

struct test_event_b {
    double data;
};

struct test_event_empty {};

}

using namespace ores::eventing::service;

TEST_CASE("event_bus_subscribe_and_publish", tags) {
    event_bus bus;

    bool handler_called = false;
    int received_value = 0;
    std::string received_message;

    auto sub = bus.subscribe<test_event_a>([&](const test_event_a& e) {
        handler_called = true;
        received_value = e.value;
        received_message = e.message;
    });

    REQUIRE(sub.is_active());
    REQUIRE(bus.subscriber_count<test_event_a>() == 1);

    bus.publish(test_event_a{42, "hello"});

    REQUIRE(handler_called);
    REQUIRE(received_value == 42);
    REQUIRE(received_message == "hello");
}

TEST_CASE("event_bus_multiple_subscribers", tags) {
    event_bus bus;

    int call_count = 0;
    std::vector<int> received_values;

    auto sub1 = bus.subscribe<test_event_a>([&](const test_event_a& e) {
        call_count++;
        received_values.push_back(e.value);
    });

    auto sub2 = bus.subscribe<test_event_a>([&](const test_event_a& e) {
        call_count++;
        received_values.push_back(e.value * 10);
    });

    REQUIRE(bus.subscriber_count<test_event_a>() == 2);

    bus.publish(test_event_a{5, "test"});

    REQUIRE(call_count == 2);
    REQUIRE(received_values.size() == 2);
    REQUIRE(received_values[0] == 5);
    REQUIRE(received_values[1] == 50);
}

TEST_CASE("event_bus_different_event_types", tags) {
    event_bus bus;

    bool a_called = false;
    bool b_called = false;

    auto sub_a = bus.subscribe<test_event_a>([&](const test_event_a&) {
        a_called = true;
    });

    auto sub_b = bus.subscribe<test_event_b>([&](const test_event_b&) {
        b_called = true;
    });

    REQUIRE(bus.subscriber_count<test_event_a>() == 1);
    REQUIRE(bus.subscriber_count<test_event_b>() == 1);

    bus.publish(test_event_a{1, "a"});

    REQUIRE(a_called);
    REQUIRE_FALSE(b_called);

    a_called = false;
    bus.publish(test_event_b{3.14});

    REQUIRE_FALSE(a_called);
    REQUIRE(b_called);
}

TEST_CASE("event_bus_unsubscribe_on_destruction", tags) {
    event_bus bus;

    int call_count = 0;

    {
        auto sub = bus.subscribe<test_event_a>([&](const test_event_a&) {
            call_count++;
        });

        REQUIRE(bus.subscriber_count<test_event_a>() == 1);

        bus.publish(test_event_a{1, "first"});
        REQUIRE(call_count == 1);
    }

    // Subscription should be removed after sub goes out of scope
    REQUIRE(bus.subscriber_count<test_event_a>() == 0);

    bus.publish(test_event_a{2, "second"});
    REQUIRE(call_count == 1); // Should not be incremented
}

TEST_CASE("event_bus_manual_unsubscribe", tags) {
    event_bus bus;

    int call_count = 0;

    auto sub = bus.subscribe<test_event_a>([&](const test_event_a&) {
        call_count++;
    });

    REQUIRE(sub.is_active());
    REQUIRE(bus.subscriber_count<test_event_a>() == 1);

    bus.publish(test_event_a{1, "first"});
    REQUIRE(call_count == 1);

    sub.unsubscribe();

    REQUIRE_FALSE(sub.is_active());
    REQUIRE(bus.subscriber_count<test_event_a>() == 0);

    bus.publish(test_event_a{2, "second"});
    REQUIRE(call_count == 1); // Should not be incremented
}

TEST_CASE("event_bus_subscription_move", tags) {
    event_bus bus;

    int call_count = 0;

    subscription sub1 = bus.subscribe<test_event_a>([&](const test_event_a&) {
        call_count++;
    });

    REQUIRE(sub1.is_active());
    REQUIRE(bus.subscriber_count<test_event_a>() == 1);

    subscription sub2 = std::move(sub1);

    REQUIRE_FALSE(sub1.is_active());
    REQUIRE(sub2.is_active());
    REQUIRE(bus.subscriber_count<test_event_a>() == 1);

    bus.publish(test_event_a{1, "test"});
    REQUIRE(call_count == 1);

    sub2.unsubscribe();
    REQUIRE(bus.subscriber_count<test_event_a>() == 0);
}

TEST_CASE("event_bus_no_subscribers", tags) {
    event_bus bus;

    REQUIRE(bus.subscriber_count<test_event_a>() == 0);

    // Should not crash when publishing with no subscribers
    bus.publish(test_event_a{1, "nobody listening"});

    REQUIRE(bus.subscriber_count<test_event_a>() == 0);
}

TEST_CASE("event_bus_empty_event", tags) {
    event_bus bus;

    bool called = false;

    auto sub = bus.subscribe<test_event_empty>([&](const test_event_empty&) {
        called = true;
    });

    bus.publish(test_event_empty{});

    REQUIRE(called);
}

TEST_CASE("event_bus_handler_exception_does_not_break_other_handlers", tags) {
    event_bus bus;

    int call_count = 0;

    auto sub1 = bus.subscribe<test_event_a>([&](const test_event_a&) {
        call_count++;
        throw std::runtime_error("handler 1 throws");
    });

    auto sub2 = bus.subscribe<test_event_a>([&](const test_event_a&) {
        call_count++;
    });

    // Publishing should not throw, and second handler should still be called
    REQUIRE_NOTHROW(bus.publish(test_event_a{1, "test"}));
    REQUIRE(call_count == 2);
}

TEST_CASE("event_bus_thread_safety", tags) {
    event_bus bus;

    std::atomic<int> total_received{0};
    constexpr int num_threads = 4;
    constexpr int events_per_thread = 100;

    // Create a subscriber that counts events
    auto sub = bus.subscribe<test_event_a>([&](const test_event_a& e) {
        total_received += e.value;
    });

    // Launch multiple publisher threads
    std::vector<std::thread> threads;
    for (int t = 0; t < num_threads; ++t) {
        threads.emplace_back([&bus]() {
            for (int i = 0; i < events_per_thread; ++i) {
                bus.publish(test_event_a{1, "concurrent"});
            }
        });
    }

    for (auto& t : threads) {
        t.join();
    }

    REQUIRE(total_received == num_threads * events_per_thread);
}

TEST_CASE("event_bus_concurrent_subscribe_unsubscribe", tags) {
    event_bus bus;

    std::atomic<bool> running{true};
    std::atomic<int> publish_count{0};

    // Thread that continuously publishes
    std::thread publisher([&]() {
        while (running) {
            bus.publish(test_event_a{1, "pub"});
            publish_count++;
        }
    });

    // Thread that subscribes and unsubscribes repeatedly
    std::thread subscriber([&]() {
        for (int i = 0; i < 50; ++i) {
            auto sub = bus.subscribe<test_event_a>([](const test_event_a&) {});
            std::this_thread::sleep_for(std::chrono::microseconds(100));
            sub.unsubscribe();
        }
    });

    subscriber.join();
    running = false;
    publisher.join();

    // Should complete without deadlock or crash
    REQUIRE(publish_count > 0);
}
