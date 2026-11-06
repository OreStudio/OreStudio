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

namespace {

/// The tenant every stream hears beside its own.
const std::string ticker_system_tenant = "ffffffff-ffff-ffff-ffff-ffffffffffff";

}

TEST_CASE("a_change_recorded_between_ticks_reaches_the_stream", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a tick carries a recorded change to the stream";

    change_coalescer coalescer;
    change_hub hub(ticker_system_tenant);
    auto stream = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(5));

    boost::asio::io_context io;
    boost::asio::co_spawn(io, [&] { return ticker.run(io.get_executor()); }, boost::asio::detached);
    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});

    io.run_for(std::chrono::milliseconds(100));
    ticker.stop();
    io.run();

    REQUIRE(stream->size() == 1);
    const auto frame = stream->try_pop();
    REQUIRE(frame.has_value());
    REQUIRE(frame->find("event: iam.book") != std::string::npos);
    REQUIRE(frame->find("\"tenant\":\"acme\"") != std::string::npos);
}

TEST_CASE("a_stopped_ticker_publishes_nothing_further", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a stopped ticker is silent";

    change_coalescer coalescer;
    change_hub hub(ticker_system_tenant);
    auto stream = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(5));

    boost::asio::io_context io;
    boost::asio::co_spawn(io, [&] { return ticker.run(io.get_executor()); }, boost::asio::detached);
    io.run_for(std::chrono::milliseconds(40));
    ticker.stop();
    io.run();
    REQUIRE(!ticker.running());

    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});
    io.restart();
    io.run_for(std::chrono::milliseconds(40));

    REQUIRE(stream->size() == 0);
}

TEST_CASE("a_tick_with_nothing_recorded_writes_nothing", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an idle tick is silent";

    change_coalescer coalescer;
    change_hub hub(ticker_system_tenant);
    auto stream = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(5));

    boost::asio::io_context io;
    boost::asio::co_spawn(io, [&] { return ticker.run(io.get_executor()); }, boost::asio::detached);
    io.run_for(std::chrono::milliseconds(40));
    ticker.stop();
    io.run();

    REQUIRE(stream->size() == 0);
}

TEST_CASE("a_change_inside_the_window_does_not_extend_it", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a window closes one interval after it opened";

    change_coalescer coalescer;
    change_hub hub(ticker_system_tenant);
    auto stream = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(100));

    boost::asio::io_context io;
    boost::asio::co_spawn(io, [&] { return ticker.run(io.get_executor()); }, boost::asio::detached);

    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});
    io.run_for(std::chrono::milliseconds(80));

    // A change that lands inside the window is gathered into it, not waited for.
    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});
    io.run_for(std::chrono::milliseconds(60));
    ticker.stop();
    io.run();

    // A window pushed back by the second change would still be open at 140 ms.
    REQUIRE(stream->size() == 1);
    REQUIRE(stream->try_pop()->find("event: iam.book") != std::string::npos);
}

TEST_CASE("a_closed_window_opens_again_on_the_next_change", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the clock returns to waiting after a window";

    change_coalescer coalescer;
    change_hub hub(ticker_system_tenant);
    auto stream = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(10));

    boost::asio::io_context io;
    boost::asio::co_spawn(io, [&] { return ticker.run(io.get_executor()); }, boost::asio::detached);

    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "book"});
    io.run_for(std::chrono::milliseconds(40));

    coalescer.record({.tenant = "acme", .party = "ops", .component = "iam", .entity = "trade"});
    io.run_for(std::chrono::milliseconds(40));
    ticker.stop();
    io.run();

    REQUIRE(stream->size() == 2);
    REQUIRE(stream->try_pop()->find("event: iam.book") != std::string::npos);
    REQUIRE(stream->try_pop()->find("event: iam.trade") != std::string::npos);
}

TEST_CASE("a_ticker_stopped_before_it_ran_returns_at_once", "[change_ticker]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a stop before the loop is not lost";

    change_coalescer coalescer;
    change_hub hub(ticker_system_tenant);
    auto stream = hub.subscribe({.tenant = "acme", .party = "ops"});
    change_ticker ticker(coalescer, hub, std::chrono::milliseconds(5));

    auto returned = false;
    boost::asio::io_context io;
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> {
            co_await ticker.run(io.get_executor());
            returned = true;
        },
        boost::asio::detached);

    // The stop lands before the loop has an executor to post to.
    ticker.stop();
    io.run_for(std::chrono::milliseconds(50));

    REQUIRE(returned);
    REQUIRE(stream->size() == 0);
}
