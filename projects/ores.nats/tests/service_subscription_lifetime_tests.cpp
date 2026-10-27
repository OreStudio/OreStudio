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
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include "test_helpers.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <condition_variable>
#include <cstddef>
#include <mutex>
#include <optional>
#include <string>
#include <thread>
#include <vector>

// What a service trusts when it tears its state down. Destroying a
// subscription must not return while the handler that subscription carries is
// still running, because that handler reaches the owner's state. A service
// destroy-its-subscription-then-destroy-its-state order is safe only if this
// holds, and it did not: ores.marketdata.service dumped core on shutdown when
// one more tick arrived after "Shutdown complete".
//
// The subject is derived from this run, so no stream of the running fleet is
// touched and two runs never share one.
namespace {

using namespace std::chrono_literals;

/**
 * @brief A suffix no other run of this test can use.
 */
/**
 * @brief What the handler and the test thread say to each other.
 */
struct handler_gate {
    std::mutex mu;
    std::condition_variable cv;
    bool entered = false;
    bool release = false;
    bool finished = false;

    void announce_entry() {
        std::lock_guard lock(mu);
        entered = true;
        cv.notify_all();
    }

    bool await_entry(std::chrono::milliseconds timeout) {
        std::unique_lock lock(mu);
        return cv.wait_for(lock, timeout, [this] { return entered; });
    }

    void hold_until_released() {
        std::unique_lock lock(mu);
        cv.wait(lock, [this] { return release; });
        finished = true;
    }

    void release_handler() {
        {
            std::lock_guard lock(mu);
            release = true;
        }
        cv.notify_all();
    }

    bool handler_finished() {
        std::lock_guard lock(mu);
        return finished;
    }
};

}

TEST_CASE("destroying a subscription waits for the handler it is running", "[nats][subscription]") {
    ores::nats::service::client nats(ores::testing::make_nats_options());
    nats.connect();

    const std::string subject = "test.subscription.lifetime." + unique_suffix();
    handler_gate gate;

    std::optional<ores::nats::service::subscription> sub;
    sub.emplace(nats.subscribe(subject, [&gate](ores::nats::message) {
        gate.announce_entry();
        gate.hold_until_released();
    }));

    const std::vector<std::byte> payload{std::byte{1}};

    // A fresh subscription may not have reached the server yet, so publish
    // until the handler is inside the call it will not leave.
    const auto deadline = std::chrono::steady_clock::now() + 10s;
    while (!gate.await_entry(100ms)) {
        REQUIRE(std::chrono::steady_clock::now() < deadline);
        nats.publish(subject, payload);
    }

    std::atomic<bool> destroyed{false};
    std::thread destroyer([&sub, &destroyed] {
        sub.reset();
        destroyed.store(true, std::memory_order_release);
    });

    // The handler is still inside the call. A destroy that returned here would
    // let this test destroy what the handler is writing to, which is the
    // failure the wait exists to prevent.
    std::this_thread::sleep_for(200ms);
    CHECK_FALSE(destroyed.load(std::memory_order_acquire));

    gate.release_handler();
    destroyer.join();

    CHECK(destroyed.load(std::memory_order_acquire));
    CHECK(gate.handler_finished());
}
