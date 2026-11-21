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
#include "ores.service/service/reconciler.hpp"
#include <atomic>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <thread>

namespace {

using ores::service::service::next_reconcile_delay;
using ores::service::service::reconcile_result;
using ores::service::service::reconciler;
using ores::service::service::reconciler_options;
using std::chrono::milliseconds;

/// Polls @p condition until it holds or two seconds pass.
template <typename Condition>
bool eventually(Condition condition) {
    const auto until = std::chrono::steady_clock::now() + std::chrono::seconds(2);
    while (std::chrono::steady_clock::now() < until) {
        if (condition())
            return true;
        std::this_thread::sleep_for(milliseconds(1));
    }
    return condition();
}

reconciler_options fast(std::optional<milliseconds> interval = std::nullopt) {
    return {.name = "test",
            .first_delay = milliseconds(2),
            .max_delay = milliseconds(8),
            .interval = interval};
}

}

TEST_CASE("the wait after a pass that left work doubles to its cap", "[reconciler]") {
    const milliseconds first(1000);
    const milliseconds max(60000);
    auto delay = next_reconcile_delay(std::nullopt, first, max);
    CHECK(delay == milliseconds(1000));
    delay = next_reconcile_delay(delay, first, max);
    CHECK(delay == milliseconds(2000));
    delay = next_reconcile_delay(delay, first, max);
    CHECK(delay == milliseconds(4000));
    for (int i = 0; i < 10; ++i)
        delay = next_reconcile_delay(delay, first, max);
    CHECK(delay == max);
}

TEST_CASE("a pass that leaves work runs again without a kick", "[reconciler]") {
    // The dependency answers from the fourth pass on, as a service that comes
    // up after the one that calls it.
    std::atomic<int> passes{0};
    reconciler r(fast(), [&] {
        return ++passes < 4 ? reconcile_result::pending("no service is handling subject") :
                              reconcile_result::finished();
    });
    r.kick();

    REQUIRE(eventually([&] { return passes.load() == 4; }));
    std::this_thread::sleep_for(milliseconds(30));
    CHECK(passes.load() == 4);
}

TEST_CASE("a reconciler with no interval runs only when kicked", "[reconciler]") {
    std::atomic<int> passes{0};
    reconciler r(fast(), [&] {
        ++passes;
        return reconcile_result::finished();
    });

    std::this_thread::sleep_for(milliseconds(30));
    CHECK(passes.load() == 0);

    r.kick();
    REQUIRE(eventually([&] { return passes.load() == 1; }));
    std::this_thread::sleep_for(milliseconds(30));
    CHECK(passes.load() == 1);
}

TEST_CASE("a reconciler with an interval checks again when it is done", "[reconciler]") {
    std::atomic<int> passes{0};
    reconciler r(fast(milliseconds(5)), [&] {
        ++passes;
        return reconcile_result::finished();
    });

    CHECK(eventually([&] { return passes.load() >= 3; }));
}

TEST_CASE("a pass that throws leaves work", "[reconciler]") {
    std::atomic<int> passes{0};
    reconciler r(fast(), [&]() -> reconcile_result {
        if (++passes < 3)
            throw std::runtime_error("NATS request failed");
        return reconcile_result::finished();
    });
    r.kick();

    REQUIRE(eventually([&] { return passes.load() == 3; }));
}

TEST_CASE("a reconciler stops while it waits", "[reconciler]") {
    std::atomic<int> passes{0};
    reconciler r({.name = "test", .first_delay = std::chrono::hours(1)}, [&] {
        ++passes;
        return reconcile_result::pending("never answers");
    });
    r.kick();
    REQUIRE(eventually([&] { return passes.load() == 1; }));

    const auto started = std::chrono::steady_clock::now();
    r.stop();
    CHECK(std::chrono::steady_clock::now() - started < std::chrono::seconds(1));
    CHECK(passes.load() == 1);
}
