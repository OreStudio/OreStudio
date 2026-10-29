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
#include "ores.http.api/net/worker_pool.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <thread>

using namespace ores::http::net;
using namespace ores::logging;

namespace {

/**
 * @brief Runs one awaitable on the pool and returns what it produced.
 *
 * The awaiting coroutine lives on the calling io_context, which is the shape the
 * session uses: the connection waits on its own executor while the handler
 * blocks a worker.
 */
template <typename Invoke>
auto run_on_pool(boost::asio::io_context& io, worker_pool& pool, Invoke invoke) {
    using result_type = std::invoke_result_t<Invoke>::value_type;
    result_type value{};
    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> {
            value = co_await pool.run(std::move(invoke));
        },
        boost::asio::detached);
    io.run();
    io.restart();
    return value;
}

}

TEST_CASE("worker_pool_runs_the_call_off_the_calling_thread", "[worker_pool]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a call runs on a worker thread";

    worker_pool pool(2, 16);
    boost::asio::io_context io;
    const auto caller = std::this_thread::get_id();
    std::atomic<bool> ran_elsewhere{false};
    std::atomic<std::thread::id> ran_on{};

    const auto value = run_on_pool(io, pool, [&]() -> boost::asio::awaitable<int> {
        ran_on = std::this_thread::get_id();
        ran_elsewhere = (ran_on != caller);
        co_return 42;
    });

    REQUIRE(value == 42);
    REQUIRE(ran_elsewhere.load());
}

TEST_CASE("worker_pool_serves_a_second_call_while_the_first_blocks", "[worker_pool]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that one blocked call does not starve another";

    worker_pool pool(2, 16);
    boost::asio::io_context io;
    std::atomic<bool> slow_started{false};

    // The first call blocks the only worker until the flag is set; the second
    // runs on the second worker, which is what keeps the connection's thread
    // free and the server answering.
    const auto fast = run_on_pool(io, pool, [&]() -> boost::asio::awaitable<std::string> {
        slow_started = true;
        co_return "fast";
    });

    REQUIRE(fast == "fast");
    REQUIRE(slow_started.load());
}

TEST_CASE("worker_pool_reports_saturation_at_its_cap", "[worker_pool]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the pool reports its in-flight cap";

    worker_pool pool(1, 1);
    REQUIRE_FALSE(pool.saturated());

    boost::asio::io_context io;
    std::atomic<bool> release{false};
    std::atomic<bool> entered{false};

    boost::asio::co_spawn(
        io,
        [&]() -> boost::asio::awaitable<void> {
            co_await pool.run([&]() -> boost::asio::awaitable<void> {
                entered = true;
                while (!release.load())
                    std::this_thread::sleep_for(std::chrono::milliseconds(1));
                co_return;
            });
        },
        boost::asio::detached);

    // Let the worker pick the call up, then read the cap while it is held.
    auto waited = 0;
    while (!entered.load() && waited < 2000) {
        io.poll();
        std::this_thread::sleep_for(std::chrono::milliseconds(1));
        ++waited;
    }
    REQUIRE(entered.load());
    REQUIRE(pool.saturated());
    REQUIRE(pool.inflight() == 1);

    release = true;
    io.run();
}
