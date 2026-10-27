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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/service/run_clock.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <future>
#include <thread>
#include <utility>

using namespace std::chrono_literals;
using ores::analytics::quant::service::i_run_clock;
using ores::analytics::quant::service::manual_run_clock;
using ores::analytics::quant::service::steady_run_clock;

namespace {

const auto wall_origin = std::chrono::system_clock::time_point(std::chrono::days(20000));

}

TEST_CASE("a manual clock wakes a waiter exactly when time reaches its deadline", "[run_clock]") {
    manual_run_clock clock(wall_origin);
    const auto deadline = clock.now() + 10ms;

    auto woken = std::async(std::launch::async, [&] {
        const auto why = clock.wait_until(deadline);
        return std::pair(why, clock.now());
    });
    clock.await_waiter();

    // Long enough for a waiter that woke too early to have returned.
    clock.advance(9ms);
    CHECK(woken.wait_for(50ms) == std::future_status::timeout);

    clock.advance(1ms);
    const auto [why, at] = woken.get();
    CHECK(why == i_run_clock::wake::deadline);
    CHECK(at == deadline);
}

TEST_CASE("an interrupt wakes a manual clock's waiter at once", "[run_clock]") {
    manual_run_clock clock(wall_origin);
    auto woken =
        std::async(std::launch::async, [&] { return clock.wait_until(clock.now() + 24h); });
    clock.await_waiter();

    clock.interrupt();
    CHECK(woken.get() == i_run_clock::wake::interrupted);
    CHECK(clock.now() == i_run_clock::time_point{});
}

TEST_CASE("an interrupt sent before a wait is kept for it, once", "[run_clock]") {
    manual_run_clock clock(wall_origin);
    clock.interrupt();

    CHECK(clock.wait_until(clock.now() + 24h) == i_run_clock::wake::interrupted);
    CHECK(clock.wait_until(clock.now()) == i_run_clock::wake::deadline);
}

TEST_CASE("a manual clock maps its time to the wall time it was given", "[run_clock]") {
    manual_run_clock clock(wall_origin);
    clock.advance(90s);
    CHECK(clock.wall(clock.now()) == wall_origin + 90s);
}

TEST_CASE("a steady clock waits until its deadline", "[run_clock]") {
    steady_run_clock clock;
    const auto deadline = clock.now() + 20ms;
    CHECK(clock.wait_until(deadline) == i_run_clock::wake::deadline);
    CHECK(clock.now() >= deadline);
}

TEST_CASE("an interrupt cuts a steady clock's wait short", "[run_clock]") {
    steady_run_clock clock;
    const auto start = clock.now();
    auto woken =
        std::async(std::launch::async, [&] { return clock.wait_until(clock.now() + 24h); });

    // The steady clock cannot report a waiter, so give the thread time to start
    // waiting; an interrupt sent before it waits is the kept path, tested above.
    std::this_thread::sleep_for(50ms);
    clock.interrupt();
    CHECK(woken.get() == i_run_clock::wake::interrupted);
    CHECK(clock.now() - start < 10s);
}

TEST_CASE("a steady clock stamps with the wall time", "[run_clock]") {
    steady_run_clock clock;
    const auto stamped = clock.wall(clock.now());
    const auto actual = std::chrono::system_clock::now();
    CHECK(actual - stamped < 1s);
    CHECK(stamped - actual < 1s);
}
