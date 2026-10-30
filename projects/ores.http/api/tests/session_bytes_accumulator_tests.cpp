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
#include "ores.http.api/net/session_bytes_accumulator.hpp"
#include "ores.http.api/net/worker_pool.hpp"
#include "ores.logging/make_logger.hpp"
#include "test_suite.hpp"
#include <boost/asio/io_context.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <mutex>
#include <string>
#include <thread>
#include <vector>

using namespace ores::http::net;
using namespace ores::logging;

namespace {

/**
 * @brief What the sink received, and on which thread.
 *
 * The accumulator writes from a worker, so the test has to record the writing
 * thread as well as the numbers: a write that happened on the caller's thread
 * would be the defect this class exists to remove.
 */
struct sink_record final {
    std::mutex mu;
    std::vector<std::string> sessions;
    std::vector<std::uint64_t> sent;
    std::vector<std::uint64_t> received;
    std::vector<std::thread::id> threads;

    std::size_t count() {
        std::lock_guard lock(mu);
        return sessions.size();
    }

    void record(const std::string& session_id,
                std::chrono::system_clock::time_point,
                std::size_t bytes_sent,
                std::size_t bytes_received) {
        std::lock_guard lock(mu);
        sessions.push_back(session_id);
        sent.push_back(bytes_sent);
        received.push_back(bytes_received);
        threads.push_back(std::this_thread::get_id());
    }
};

}

TEST_CASE("session_bytes_accumulator_writes_the_running_total", "[session_bytes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the sink sees the total, not the last request";

    sink_record record;
    boost::asio::io_context io;
    worker_pool pool(1, 8);
    session_bytes_accumulator accumulator(
        io,
        [&record](const std::string& id,
                  std::chrono::system_clock::time_point at,
                  std::size_t sent,
                  std::size_t received) { record.record(id, at, sent, received); },
        pool,
        std::chrono::seconds(5),
        std::chrono::seconds(3600));

    const auto start = std::chrono::system_clock::now();
    accumulator.add("session-a", start, 10, 1);
    accumulator.add("session-a", start, 20, 2);
    accumulator.add("session-a", start, 30, 3);
    accumulator.flush();

    REQUIRE(record.count() == 1);
    REQUIRE(record.sent.front() == 60);
    REQUIRE(record.received.front() == 6);
    REQUIRE(record.sessions.front() == "session-a");
}

TEST_CASE("session_bytes_accumulator_writes_on_a_worker_thread", "[session_bytes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that the write leaves the calling thread";

    sink_record record;
    boost::asio::io_context io;
    worker_pool pool(1, 8);
    session_bytes_accumulator accumulator(
        io,
        [&record](const std::string& id,
                  std::chrono::system_clock::time_point at,
                  std::size_t sent,
                  std::size_t received) { record.record(id, at, sent, received); },
        pool,
        std::chrono::milliseconds(20),
        std::chrono::seconds(3600));

    const auto caller = std::this_thread::get_id();
    accumulator.start();
    accumulator.add("session-b", std::chrono::system_clock::now(), 5, 5);

    // Run the context until the timer has fired and the write has happened.
    const auto deadline = std::chrono::steady_clock::now() + std::chrono::seconds(5);
    while (record.count() == 0 && std::chrono::steady_clock::now() < deadline) {
        io.run_for(std::chrono::milliseconds(10));
        io.restart();
    }
    accumulator.stop();

    REQUIRE(record.count() >= 1);
    REQUIRE(record.sent.front() == 5);
    REQUIRE(record.threads.front() != caller);
}

TEST_CASE("session_bytes_accumulator_does_not_write_a_quiet_session", "[session_bytes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that an idle session costs nothing";

    sink_record record;
    boost::asio::io_context io;
    worker_pool pool(1, 8);
    session_bytes_accumulator accumulator(
        io,
        [&record](const std::string& id,
                  std::chrono::system_clock::time_point at,
                  std::size_t sent,
                  std::size_t received) { record.record(id, at, sent, received); },
        pool,
        std::chrono::seconds(5),
        std::chrono::seconds(3600));

    accumulator.add("session-c", std::chrono::system_clock::now(), 7, 8);
    accumulator.flush();
    REQUIRE(record.count() == 1);

    // The second flush finds nothing new, so the sink is not called again.
    accumulator.flush();
    REQUIRE(record.count() == 1);

    // One more request, and the total grows by exactly that request.
    accumulator.add("session-c", std::chrono::system_clock::now(), 3, 4);
    accumulator.flush();
    REQUIRE(record.count() == 2);
    REQUIRE(record.sent.back() == 10);
    REQUIRE(record.received.back() == 12);
}

TEST_CASE("session_bytes_accumulator_keeps_the_total_when_the_sink_fails", "[session_bytes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that a failed write costs nothing";

    auto attempts = 0;
    auto received_total = std::uint64_t{0};
    boost::asio::io_context io;
    worker_pool pool(1, 8);
    session_bytes_accumulator accumulator(
        io,
        [&attempts, &received_total](const std::string&,
                                     std::chrono::system_clock::time_point,
                                     std::size_t,
                                     std::size_t received) {
            ++attempts;
            if (attempts == 1)
                throw std::runtime_error("database is down");
            received_total = received;
        },
        pool,
        std::chrono::seconds(5),
        std::chrono::seconds(3600));

    accumulator.add("session-d", std::chrono::system_clock::now(), 1, 100);
    accumulator.flush();
    REQUIRE(attempts == 1);

    accumulator.flush();
    REQUIRE(attempts == 2);
    REQUIRE(received_total == 100);
}

TEST_CASE("session_bytes_accumulator_writes_what_is_pending_on_stop", "[session_bytes]") {
    auto lg(make_logger(test_suite));
    BOOST_LOG_SEV(lg, info) << "Testing that shutdown flushes the totals";

    sink_record record;
    boost::asio::io_context io;
    worker_pool pool(1, 8);
    session_bytes_accumulator accumulator(
        io,
        [&record](const std::string& id,
                  std::chrono::system_clock::time_point at,
                  std::size_t sent,
                  std::size_t received) { record.record(id, at, sent, received); },
        pool,
        std::chrono::seconds(3600),
        std::chrono::seconds(3600));

    accumulator.add("session-e", std::chrono::system_clock::now(), 11, 12);
    accumulator.stop();

    REQUIRE(record.count() == 1);
    REQUIRE(record.sent.front() == 11);
    REQUIRE(record.received.front() == 12);
}
