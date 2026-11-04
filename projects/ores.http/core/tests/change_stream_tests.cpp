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

/// The tenant every stream hears beside its own.
const std::string stream_system_tenant = "ffffffff-ffff-ffff-ffff-ffffffffffff";

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

    change_hub hub(stream_system_tenant);
    auto first = hub.subscribe({.tenant = "acme"});
    auto second = hub.subscribe({.tenant = "acme"});
    REQUIRE(hub.open_streams() == 2);

    hub.publish("acme", "", "data: one\n\n");

    REQUIRE(first->size() == 1);
    REQUIRE(second->size() == 1);
    REQUIRE(first->try_pop() == std::optional<std::string>("id: 1\ndata: one\n\n"));
    REQUIRE(second->try_pop() == std::optional<std::string>("id: 1\ndata: one\n\n"));
}

TEST_CASE("a_stream_that_has_ended_is_forgotten_by_the_next_publish", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a closed stream does not accumulate";

    change_hub hub(stream_system_tenant);
    auto ended = hub.subscribe({.tenant = "acme"});
    ended->close();
    auto open = hub.subscribe({.tenant = "acme"});

    hub.publish("acme", "", "data: one\n\n");

    REQUIRE(hub.open_streams() == 1);
    REQUIRE(ended->size() == 0);
    REQUIRE(open->size() == 1);
}

TEST_CASE("a_stream_that_is_not_read_drops_its_oldest_frames", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a stream's queue is bounded";

    change_hub hub(stream_system_tenant, 3);
    auto slow = hub.subscribe({.tenant = "acme"});

    for (auto index = 0; index < 5; ++index)
        hub.publish("acme", "", "frame " + std::to_string(index));

    REQUIRE(slow->size() == 3);
    // The newest survive: a screen that fell behind wants where the world is now.
    REQUIRE(slow->try_pop() == std::optional<std::string>("id: 3\nframe 2"));
    REQUIRE(slow->try_pop() == std::optional<std::string>("id: 4\nframe 3"));
    REQUIRE(slow->try_pop() == std::optional<std::string>("id: 5\nframe 4"));
    REQUIRE(!slow->try_pop().has_value());
}

TEST_CASE("the_source_writes_a_queued_frame", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the source writes what the hub published";

    auto subscription = std::make_shared<change_subscription>(stream_scope{.tenant = "acme"}, 8);
    change_stream_source source(subscription, std::chrono::milliseconds(50));
    subscription->push("data: hello\n\n");

    REQUIRE(read_once(source, 4096) == "data: hello\n\n");
}

TEST_CASE("the_source_writes_a_keep_alive_when_nothing_arrives", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a quiet stream is kept alive";

    auto subscription = std::make_shared<change_subscription>(stream_scope{.tenant = "acme"}, 8);
    change_stream_source source(subscription, std::chrono::milliseconds(10));

    const auto started = std::chrono::steady_clock::now();
    REQUIRE(read_once(source, 4096) == ": keep-alive\n\n");
    // The keep-alive waits for the interval rather than spinning on the queue.
    REQUIRE(std::chrono::steady_clock::now() - started >= std::chrono::milliseconds(10));
}

TEST_CASE("a_frame_ends_the_wait_before_the_keep_alive_interval", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a frame does not wait out the keep-alive";

    auto subscription = std::make_shared<change_subscription>(stream_scope{.tenant = "acme"}, 8);
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

    auto subscription = std::make_shared<change_subscription>(stream_scope{.tenant = "acme"}, 8);
    change_stream_source source(subscription, std::chrono::milliseconds(10));
    subscription->close();

    REQUIRE(read_once(source, 4096).empty());
}

TEST_CASE("a_frame_reaches_only_the_streams_that_hear_its_tenant", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a stream is kept to its own tenant";

    change_hub hub(stream_system_tenant);
    auto acme = hub.subscribe({.tenant = "acme"});
    auto globex = hub.subscribe({.tenant = "globex"});

    hub.publish("acme", "", "data: acme moved\n\n");

    REQUIRE(acme->size() == 1);
    REQUIRE(globex->size() == 0);
}

TEST_CASE("a_system_tenant_frame_reaches_every_stream", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the shared catalogue reaches every tenant";

    change_hub hub(stream_system_tenant);
    auto acme = hub.subscribe({.tenant = "acme"});
    auto globex = hub.subscribe({.tenant = "globex"});

    hub.publish(stream_system_tenant, "", "data: currencies moved\n\n");

    REQUIRE(acme->size() == 1);
    REQUIRE(globex->size() == 1);
}

TEST_CASE("a_stream_whose_source_ended_leaves_the_hub", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a client that is gone is not held";

    change_hub hub(stream_system_tenant);
    {
        change_stream_source source(hub.subscribe({.tenant = "acme"}), std::chrono::milliseconds(10));
        REQUIRE(hub.open_streams() == 1);
    }

    // The response, and with it the source, was destroyed when the client left.
    REQUIRE(hub.open_streams() == 0);

    // And nothing is queued for it any more: the next frame has one home.
    hub.publish("acme", "", "data: one\n\n");
    REQUIRE(hub.open_streams() == 0);
}

TEST_CASE("every_frame_carries_an_id_and_the_ids_ascend", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a frame states which frame it is";

    change_hub hub(stream_system_tenant);
    auto stream = hub.subscribe({.tenant = "acme"});

    hub.publish("acme", "", "data: one\n\n");
    hub.publish("acme", "", "data: two\n\n");

    REQUIRE(stream->try_pop() == std::optional<std::string>("id: 1\ndata: one\n\n"));
    REQUIRE(stream->try_pop() == std::optional<std::string>("id: 2\ndata: two\n\n"));
}

TEST_CASE("a_resuming_client_is_given_the_frames_it_missed", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that Last-Event-ID replays what was missed";

    change_hub hub(stream_system_tenant);
    hub.publish("acme", "", "data: one\n\n");
    hub.publish("acme", "", "data: two\n\n");
    hub.publish("acme", "", "data: three\n\n");

    auto resumed = hub.subscribe({.tenant = "acme"}, change_hub::resume_point{.last_seen = 1});

    REQUIRE(resumed->size() == 2);
    REQUIRE(resumed->try_pop() == std::optional<std::string>("id: 2\ndata: two\n\n"));
    REQUIRE(resumed->try_pop() == std::optional<std::string>("id: 3\ndata: three\n\n"));
}

TEST_CASE("a_resuming_client_hears_only_its_own_tenant_and_the_system_one", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a replay cannot cross tenants";

    change_hub hub(stream_system_tenant);
    hub.publish("acme", "", "data: acme moved\n\n");
    hub.publish("globex", "", "data: globex moved\n\n");
    hub.publish(stream_system_tenant, "", "data: the catalogue moved\n\n");

    auto resumed = hub.subscribe({.tenant = "acme"}, change_hub::resume_point{.last_seen = 0});

    REQUIRE(resumed->size() == 2);
    REQUIRE(resumed->try_pop()->find("acme moved") != std::string::npos);
    REQUIRE(resumed->try_pop()->find("catalogue moved") != std::string::npos);
}

TEST_CASE("a_resuming_client_outside_the_window_is_told_to_resync", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an old id is answered with a resync";

    change_hub hub(stream_system_tenant);
    // An id this process never issued: the client comes from a previous life.
    auto resumed = hub.subscribe({.tenant = "acme"}, change_hub::resume_point{.last_seen = 99});

    REQUIRE(resumed->size() == 1);
    REQUIRE(resumed->try_pop() == std::optional<std::string>("event: resync\ndata: {}\n\n"));
}

TEST_CASE("a_member_hears_their_party_and_the_tenant_wide_changes",
          "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a party's changes stay that party's";

    change_hub hub(stream_system_tenant);
    auto ops = hub.subscribe({.tenant = "acme", .party = "ops"});

    hub.publish("acme", "ops", "data: ops moved\n\n");
    hub.publish("acme", "sales", "data: sales moved\n\n");
    hub.publish("acme", "", "data: the tenant moved\n\n");

    REQUIRE(ops->size() == 2);
    REQUIRE(ops->try_pop()->find("ops moved") != std::string::npos);
    REQUIRE(ops->try_pop()->find("tenant moved") != std::string::npos);
}

TEST_CASE("an_administrator_hears_every_party_of_the_tenant", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an administrator is not narrowed by party";

    change_hub hub(stream_system_tenant);
    auto admin = hub.subscribe(
        {.tenant = "acme", .party = "ops", .sees_all_parties = true});

    hub.publish("acme", "ops", "data: ops moved\n\n");
    hub.publish("acme", "sales", "data: sales moved\n\n");
    hub.publish("globex", "sales", "data: another tenant moved\n\n");

    REQUIRE(admin->size() == 2);
}

TEST_CASE("a_replay_respects_the_parties_the_caller_works_in", "[change_stream]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a resume cannot widen the party";

    change_hub hub(stream_system_tenant);
    hub.publish("acme", "ops", "data: ops moved\n\n");
    hub.publish("acme", "sales", "data: sales moved\n\n");
    hub.publish("acme", "", "data: the tenant moved\n\n");

    auto resumed = hub.subscribe(
        {.tenant = "acme", .party = "sales"}, change_hub::resume_point{.last_seen = 0});

    REQUIRE(resumed->size() == 2);
    REQUIRE(resumed->try_pop()->find("sales moved") != std::string::npos);
    REQUIRE(resumed->try_pop()->find("tenant moved") != std::string::npos);
}
