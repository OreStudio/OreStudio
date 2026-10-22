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
 */
#include "ores.platform/concurrency/stoppable_thread.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <future>
#include <string>
#include <thread>

namespace {

const std::string tags("[concurrency][stoppable_thread]");

using ores::platform::concurrency::stop_source;
using ores::platform::concurrency::stoppable_thread;
using namespace std::chrono_literals;

// A wait this long that returns in a fraction of it was woken.
constexpr auto long_wait = 30s;
constexpr auto woken_within = 2s;

}

TEST_CASE("a_wait_returns_false_when_the_span_passes_with_no_stop", tags) {
    const stop_source source;

    const auto started = std::chrono::steady_clock::now();
    CHECK_FALSE(source.wait_for(50ms));
    CHECK(std::chrono::steady_clock::now() - started >= 50ms);
    CHECK_FALSE(source.stop_requested());
}

TEST_CASE("a_wait_returns_at_once_when_a_stop_was_requested_before_it", tags) {
    const stop_source source;
    source.request_stop();

    const auto started = std::chrono::steady_clock::now();
    CHECK(source.wait_for(long_wait));
    CHECK(std::chrono::steady_clock::now() - started < woken_within);
}

TEST_CASE("a_stop_wakes_a_wait_that_is_in_progress", tags) {
    const stop_source source;
    auto waiter = std::async(std::launch::async, [source] {
        const auto started = std::chrono::steady_clock::now();
        const bool stopped = source.wait_for(long_wait);
        return std::pair{stopped, std::chrono::steady_clock::now() - started};
    });

    // The waiter is in its wait when the stop arrives, or about to be: either way it returns.
    std::this_thread::sleep_for(50ms);
    source.request_stop();

    const auto [stopped, waited] = waiter.get();
    CHECK(stopped);
    CHECK(waited < woken_within);
}

TEST_CASE("copies_of_a_stop_source_share_one_state", tags) {
    const stop_source source;
    const stop_source copy = source;

    copy.request_stop();

    CHECK(source.stop_requested());
}

TEST_CASE("a_moved_from_source_is_inert_and_does_not_wait", tags) {
    stop_source source;
    const stop_source taken = std::move(source);

    // Neither call dereferences the empty state, and the wait does not block.
    source.request_stop();
    CHECK(source.stop_requested());
    CHECK(source.wait_for(long_wait));
    CHECK_FALSE(taken.stop_requested());
}

TEST_CASE("the_destructor_stops_a_waiting_thread_and_joins_it", tags) {
    std::atomic<bool> left_the_wait = false;
    const auto started = std::chrono::steady_clock::now();
    {
        stoppable_thread thread([&left_the_wait](const stop_source& stop) {
            stop.wait_for(long_wait);
            left_the_wait = true;
        });
        std::this_thread::sleep_for(50ms);
    }

    CHECK(left_the_wait);
    CHECK(std::chrono::steady_clock::now() - started < woken_within);
}

TEST_CASE("request_stop_on_the_thread_reaches_the_callable", tags) {
    std::promise<bool> answer;
    auto answered = answer.get_future();
    stoppable_thread thread(
        [&answer](const stop_source& stop) { answer.set_value(stop.wait_for(long_wait)); });

    thread.request_stop();

    REQUIRE(answered.wait_for(woken_within) == std::future_status::ready);
    CHECK(answered.get());
    CHECK(thread.stop_requested());
}

TEST_CASE("assigning_over_a_running_thread_stops_and_joins_it_first", tags) {
    std::atomic<int> finished = 0;
    const auto make = [&finished] {
        return stoppable_thread([&finished](const stop_source& stop) {
            stop.wait_for(long_wait);
            ++finished;
        });
    };

    const auto started = std::chrono::steady_clock::now();
    stoppable_thread thread = make();
    std::this_thread::sleep_for(50ms);
    thread = make();

    CHECK(finished == 1);
    CHECK(std::chrono::steady_clock::now() - started < woken_within);
}
