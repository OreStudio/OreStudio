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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http/routes/synthetic/feed_tick_buffers.hpp"
#include "ores.http/routes/synthetic/feed_tick_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <map>
#include <memory>
#include <optional>
#include <string>
#include <thread>
#include <vector>

using ores::http::domain::http_method;
using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::routes::synthetic::feed_tick_buffers;
using ores::http::routes::synthetic::feed_tick_routes;
using ores::marketdata::messaging::market_tick;
using namespace ores::logging;
using namespace std::chrono_literals;

namespace {

/**
 * A transport that hands each subject's ticks to the buffer by hand, and counts
 * the subscriptions still open.
 */
struct hand_transport final {
    std::map<std::string, feed_tick_buffers::tick_handler> handlers;
    std::shared_ptr<int> open = std::make_shared<int>(0);

    feed_tick_buffers::subscriber subscriber() {
        return [this](const std::string& subject, feed_tick_buffers::tick_handler on_tick) {
            handlers[subject] = std::move(on_tick);
            ++*open;
            return std::shared_ptr<void>(nullptr, [open = open](void*) { --*open; });
        };
    }

    void send(const std::string& source,
              const std::string& value,
              std::chrono::system_clock::time_point at) {
        market_tick tick;
        tick.value = value;
        tick.observation_time = at;
        tick.oresmd_uri = "oresmd://" + source;
        handlers.at("synthetic.v1.ops.feed_tick." + source)(tick);
    }
};

const auto noon = std::chrono::sys_days{std::chrono::year{2026} / 11 / 11} + 12h;

http_response tick_answer(const std::string& path, http_request req, const std::string& pattern) {
    ores::nats::service::nats_client session;
    hand_transport transport;
    auto table = std::make_shared<ores::http::net::router>();
    feed_tick_routes::register_routes(table,
                                      std::make_shared<ores::http::openapi::endpoint_registry>(),
                                      session,
                                      std::make_shared<feed_tick_buffers>(transport.subscriber()));
    const auto route = table->match(http_method::get, path, req.path_params);
    REQUIRE(route.has_value());
    REQUIRE(route->pattern == pattern);
    REQUIRE(route->requires_auth);
    boost::asio::io_context io;
    std::optional<http_response> answer;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> { answer = co_await route->handler(req); },
        boost::asio::detached);
    io.run();
    REQUIRE(answer.has_value());
    return *answer;
}

http_request tick_query(std::map<std::string, std::string> params) {
    http_request req;
    for (auto& [name, value] : params)
        req.query_params[name] = value;
    return req;
}

}

TEST_CASE("feed_tick_buffers_keep_the_newest_ticks_of_a_watched_source", "[feed_ticks]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing what a source's buffer keeps";

    hand_transport transport;
    feed_tick_buffers buffers(transport.subscriber());
    buffers.watch("EURUSD");
    buffers.watch("EURUSD");
    CHECK(*transport.open == 1);
    CHECK(buffers.size() == 1);

    transport.send("EURUSD", "1.1", noon);
    transport.send("EURUSD", "not a price", noon + 1s);
    transport.send("EURUSD", "inf", noon + 2s);
    transport.send("EURUSD", "1.2", noon + 3s);
    const auto two = buffers.ticks("EURUSD", 10);
    REQUIRE(two.size() == 2);
    CHECK(two[0].value == 1.1);
    CHECK(two[1].value == 1.2);
    CHECK(two[1].uri == "oresmd://EURUSD");
    CHECK(buffers.ticks("EURUSD", 1).front().value == 1.2);
    CHECK(buffers.ticks("GBPUSD", 10).empty());

    for (std::size_t i = 0; i < feed_tick_buffers::capacity + 5; ++i)
        transport.send("EURUSD", std::to_string(i), noon + std::chrono::seconds(10 + i));
    const auto full = buffers.ticks("EURUSD", 1000);
    CHECK(full.size() == feed_tick_buffers::capacity);
    CHECK(full.back().value == static_cast<double>(feed_tick_buffers::capacity + 4));
}

TEST_CASE("feed_tick_buffers_answer_arrivals_from_an_instant", "[feed_ticks]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the arrival times a raster reads";

    hand_transport transport;
    feed_tick_buffers buffers(transport.subscriber());
    buffers.watch("EURUSD");
    for (int i = 0; i < 5; ++i)
        transport.send("EURUSD", "1", noon + std::chrono::seconds(i));
    CHECK(buffers.arrival_times("EURUSD", noon + 2s).size() == 3);
    const auto last_two = buffers.arrival_times("EURUSD", noon, 2);
    REQUIRE(last_two.size() == 2);
    CHECK(last_two.front() == noon + 3s);
    CHECK(feed_tick_routes::instant_text(noon + 1500ms) == "2026-11-11T12:00:01.500Z");
}

TEST_CASE("feed_tick_buffers_stop_listening_to_sources_no_longer_named", "[feed_ticks]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that dropped sources stop their subscriptions";

    hand_transport transport;
    feed_tick_buffers buffers(transport.subscriber());
    buffers.watch("EURUSD");
    buffers.watch("GBPUSD");
    buffers.watch("USDJPY");
    CHECK(*transport.open == 3);

    buffers.keep_only({"GBPUSD"});
    CHECK(*transport.open == 1);
    CHECK(buffers.size() == 1);
    CHECK(buffers.ticks("EURUSD", 10).empty());

    // A tick that reaches a dropped source's handler writes to a buffer no one reads.
    transport.send("EURUSD", "1.3", noon);
    CHECK(buffers.ticks("EURUSD", 10).empty());

    buffers.keep_only({});
    CHECK(*transport.open == 0);
}

TEST_CASE("feed_tick_buffers_hear_at_most_a_thousand_sources", "[feed_ticks]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the bound on the sources heard";

    hand_transport transport;
    feed_tick_buffers buffers(transport.subscriber());
    for (std::size_t i = 0; i < feed_tick_buffers::most_sources; ++i)
        CHECK(buffers.watch("source-" + std::to_string(i)));
    CHECK_FALSE(buffers.watch("one-too-many"));
    CHECK(buffers.watch("source-0"));
    CHECK(*transport.open == static_cast<int>(feed_tick_buffers::most_sources));

    buffers.keep_only({"source-0"});
    CHECK(buffers.watch("one-too-many"));
}

TEST_CASE("feed_tick_buffers_take_ticks_while_sources_come_and_go", "[feed_ticks]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing ticks that arrive while reads drop and watch sources";

    hand_transport transport;
    feed_tick_buffers buffers(transport.subscriber());
    buffers.watch("EURUSD");
    // The handler is taken once, as a transport holds it, and called from another thread.
    const auto handler = transport.handlers.at("synthetic.v1.ops.feed_tick.EURUSD");
    std::thread ticking([&handler] {
        market_tick tick;
        tick.value = "1.1";
        tick.observation_time = noon;
        for (int i = 0; i < 20000; ++i)
            handler(tick);
    });
    // Reads take the run the ticks fill, while other sources come and go.
    for (int i = 0; i < 2000; ++i) {
        CHECK(buffers.ticks("EURUSD", 10).size() <= 10);
        (void)buffers.arrival_times("EURUSD", noon);
        buffers.watch("GBPUSD");
        buffers.keep_only({"EURUSD"});
    }
    // Dropped while ticks may still arrive: they go to a run no one reads.
    buffers.keep_only({});
    ticking.join();
    CHECK(buffers.size() == 0);
    CHECK(buffers.ticks("EURUSD", 10).empty());
}

TEST_CASE("feed_tick_routes_refuse_a_malformed_request", "[feed_ticks]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing the tick and raster refusals";

    const std::string ticks = "/api/synthetic/feeds/{id}/ticks";
    const std::string feed = "/api/synthetic/feeds/00000000-0000-0000-0000-00000000000a/ticks";
    CHECK(tick_answer("/api/synthetic/feeds/nope/ticks", http_request{}, ticks).status ==
          http_status::bad_request);
    CHECK(tick_answer(feed, tick_query({{"limit", "0"}}), ticks).status ==
          http_status::bad_request);
    CHECK(tick_answer(feed, tick_query({{"limit", "501"}}), ticks).status ==
          http_status::bad_request);
    CHECK(tick_answer(feed, tick_query({{"resource", "folders"}}), ticks).status ==
          http_status::bad_request);
    CHECK(tick_answer(feed, tick_query({{"colour", "red"}}), ticks).status ==
          http_status::bad_request);

    const std::string raster = "/api/synthetic/feed-raster";
    CHECK(tick_answer(raster, tick_query({{"seconds", "0"}}), raster).status ==
          http_status::bad_request);
    CHECK(tick_answer(raster, tick_query({{"seconds", "601"}}), raster).status ==
          http_status::bad_request);
    CHECK(tick_answer(raster, tick_query({{"lanes", "5"}}), raster).status ==
          http_status::bad_request);
}

TEST_CASE("feed_tick_routes_forward_a_well_formed_request", "[feed_ticks]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an accepted tick read reaches the service";

    // The client is not connected, so an accepted read fails at the forward step.
    const std::string ticks = "/api/synthetic/feeds/{id}/ticks";
    const std::string feed = "/api/synthetic/feeds/00000000-0000-0000-0000-00000000000a/ticks";
    CHECK(tick_answer(feed, http_request{}, ticks).status == http_status::bad_gateway);
    CHECK(tick_answer(feed, tick_query({{"resource", "ir-curve-feeds"}, {"limit", "500"}}), ticks)
              .status == http_status::bad_gateway);
    CHECK(tick_answer("/api/synthetic/feed-raster",
                      tick_query({{"seconds", "600"}}),
                      "/api/synthetic/feed-raster")
              .status == http_status::bad_gateway);
}
