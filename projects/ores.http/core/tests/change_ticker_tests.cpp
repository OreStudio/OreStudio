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
#include "ores.http.core/stream/change_ticker.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>

using namespace ores::http::stream;
using namespace ores::logging;

TEST_CASE("a_change_recorded_between_ticks_reaches_the_stream", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a tick carries a recorded change to the stream";

    change_coalescer coalescer;
    change_hub hub;
    auto stream = hub.subscribe();
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(5));

    boost::asio::io_context io;
    boost::asio::co_spawn(io, [&] { return ticker.run(io.get_executor()); },
                          boost::asio::detached);
    coalescer.record({.tenant = "acme", .party = "ops", .entity = "book", .id = "b1"});

    io.run_for(std::chrono::milliseconds(100));
    ticker.stop();
    io.run();

    REQUIRE(stream->size() == 1);
    const auto frame = stream->try_pop();
    REQUIRE(frame.has_value());
    REQUIRE(frame->find("event: book") != std::string::npos);
    REQUIRE(frame->find("\"b1\"") != std::string::npos);
}

TEST_CASE("a_stopped_ticker_publishes_nothing_further", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a stopped ticker is silent";

    change_coalescer coalescer;
    change_hub hub;
    auto stream = hub.subscribe();
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(5));

    boost::asio::io_context io;
    boost::asio::co_spawn(io, [&] { return ticker.run(io.get_executor()); },
                          boost::asio::detached);
    io.run_for(std::chrono::milliseconds(40));
    ticker.stop();
    io.run();
    REQUIRE(!ticker.running());

    coalescer.record({.tenant = "acme", .party = "ops", .entity = "book", .id = "b1"});
    io.restart();
    io.run_for(std::chrono::milliseconds(40));

    REQUIRE(stream->size() == 0);
}

TEST_CASE("a_tick_with_nothing_recorded_writes_nothing", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an idle tick is silent";

    change_coalescer coalescer;
    change_hub hub;
    auto stream = hub.subscribe();
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(5));

    boost::asio::io_context io;
    boost::asio::co_spawn(io, [&] { return ticker.run(io.get_executor()); },
                          boost::asio::detached);
    io.run_for(std::chrono::milliseconds(40));
    ticker.stop();
    io.run();

    REQUIRE(stream->size() == 0);
}
