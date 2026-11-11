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
#include "ores.service/messaging/step_workers.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <future>
#include <latch>
#include <thread>

using ores::service::messaging::step_workers;
using namespace std::chrono_literals;

TEST_CASE("as many steps as there are workers run at once", "[step_workers]") {
    std::latch all_running(3);
    std::atomic<int> finished{0};
    {
        step_workers workers(3);
        for (int i = 0; i < 3; ++i)
            workers.run([&] {
                all_running.count_down();
                // Each step returns only once all three run, so one worker
                // would never finish them.
                all_running.wait();
                ++finished;
            });
    }
    CHECK(finished == 3);
}

TEST_CASE("a step waits for a free worker when all are busy", "[step_workers]") {
    std::promise<void> release;
    auto released = release.get_future().share();
    std::atomic<bool> third_started{false};
    step_workers workers(2);
    workers.run([released] { released.wait(); });
    workers.run([released] { released.wait(); });

    auto third =
        std::async(std::launch::async, [&] { workers.run([&] { third_started = true; }); });
    CHECK(third.wait_for(200ms) == std::future_status::timeout);
    CHECK_FALSE(third_started);

    release.set_value();
    third.wait();
}

TEST_CASE("the workers wait for every running step before they go", "[step_workers]") {
    std::atomic<bool> done{false};
    {
        step_workers workers(2);
        workers.run([&] {
            std::this_thread::sleep_for(100ms);
            done = true;
        });
    }
    CHECK(done);
}

TEST_CASE("one worker runs a step on the calling thread", "[step_workers]") {
    step_workers workers(1);
    std::thread::id ran_on;
    workers.run([&] { ran_on = std::this_thread::get_id(); });
    CHECK(ran_on == std::this_thread::get_id());
}
