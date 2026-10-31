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
#include "ores.http.core/stream/change_hub.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <thread>
#include <vector>

using namespace ores::http::stream;
using namespace ores::logging;

namespace {

/// Reads one buffer's worth from the source on a running io_context.
std::string read_once(change_stream_source& source, std::size_t bytes) {
    std::string out;
    boost::asio::io_context io;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> {
            std::vector<char> buffer(bytes);
            const auto taken = co_await source.read(std::span(buffer.data(), buffer.size()));
            out.assign(buffer.data(), taken);
        },
        boost::asio::detached);
    io.run();
    return out;
}

}

TEST_CASE("a_frame_reaches_every_open_stream", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that one frame reaches every subscription";

    change_hub hub;
    auto first = hub.subscribe();
    auto second = hub.subscribe();
    REQUIRE(hub.open_streams() == 2);

    hub.publish("data: one\n\n");

    REQUIRE(first->size() == 1);
    REQUIRE(second->size() == 1);
    REQUIRE(first->try_pop() == std::optional<std::string>("data: one\n\n"));
    REQUIRE(second->try_pop() == std::optional<std::string>("data: one\n\n"));
}

TEST_CASE("a_stream_that_has_ended_is_forgotten_by_the_next_publish", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a closed stream does not accumulate";

    change_hub hub;
    auto ended = hub.subscribe();
    ended->close();
    auto open = hub.subscribe();

    hub.publish("data: one\n\n");

    REQUIRE(hub.open_streams() == 1);
    REQUIRE(ended->size() == 0);
    REQUIRE(open->size() == 1);
}

TEST_CASE("a_stream_that_is_not_read_drops_its_oldest_frames", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a stream's queue is bounded";

    change_hub hub(3);
    auto slow = hub.subscribe();

    for (auto index = 0; index < 5; ++index)
        hub.publish("frame " + std::to_string(index));

    REQUIRE(slow->size() == 3);
    // The newest survive: a screen that fell behind wants where the world is now.
    REQUIRE(slow->try_pop() == std::optional<std::string>("frame 2"));
    REQUIRE(slow->try_pop() == std::optional<std::string>("frame 3"));
    REQUIRE(slow->try_pop() == std::optional<std::string>("frame 4"));
    REQUIRE(!slow->try_pop().has_value());
}

TEST_CASE("the_source_writes_a_queued_frame", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the source writes what the hub published";

    auto subscription = std::make_shared<change_subscription>(8);
    change_stream_source source(subscription, std::chrono::milliseconds(50));
    subscription->push("data: hello\n\n");

    REQUIRE(read_once(source, 4096) == "data: hello\n\n");
}

TEST_CASE("the_source_writes_a_keep_alive_when_nothing_arrives", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a quiet stream is kept alive";

    auto subscription = std::make_shared<change_subscription>(8);
    change_stream_source source(subscription, std::chrono::milliseconds(10));

    const auto started = std::chrono::steady_clock::now();
    REQUIRE(read_once(source, 4096) == ": keep-alive\n\n");
    // The keep-alive waits for the interval rather than spinning on the queue.
    REQUIRE(std::chrono::steady_clock::now() - started >= std::chrono::milliseconds(10));
}

TEST_CASE("a_frame_ends_the_wait_before_the_keep_alive_interval", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a frame does not wait out the keep-alive";

    auto subscription = std::make_shared<change_subscription>(8);
    change_stream_source source(subscription, std::chrono::seconds(30));

    boost::asio::io_context io;
    std::string out;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> {
            std::string buffer(64, '\0');
            const auto taken = co_await source.read(std::span(buffer.data(), buffer.size()));
            out.assign(buffer.data(), taken);
        },
        boost::asio::detached);

    // The publish wakes the wait, which a keep-alive of thirty seconds would
    // otherwise have held for the whole interval.
    std::thread publisher([&] {
        std::this_thread::sleep_for(std::chrono::milliseconds(20));
        subscription->push("data: woke\n\n");
    });
    io.run();
    publisher.join();

    REQUIRE(out == "data: woke\n\n");
}

TEST_CASE("the_source_ends_when_its_subscription_closes", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a closed subscription ends the source";

    auto subscription = std::make_shared<change_subscription>(8);
    change_stream_source source(subscription, std::chrono::milliseconds(10));
    subscription->close();

    REQUIRE(read_once(source, 4096).empty());
}
