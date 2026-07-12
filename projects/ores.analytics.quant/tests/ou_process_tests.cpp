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
#include "ores.analytics.quant/service/processes/ou_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <numeric>
#include <stdexcept>
#include <vector>

using ores::analytics::quant::service::ou_process;

TEST_CASE("ou_process reports the initial price before any next() call", "[ou_process]") {
    ou_process p(0.1, 100.0, 1.0, 100.0);
    CHECK(p.current() == 100.0);
}

TEST_CASE("ou_process next() updates and returns the same value as current()", "[ou_process]") {
    ou_process p(0.1, 100.0, 1.0, 100.0);
    const double n = p.next();
    CHECK(n == p.current());
}

TEST_CASE("ou_process is deterministic for a fixed seed", "[ou_process]") {
    ou_process a(0.3, 105.0, 2.0, 100.0, 7);
    ou_process b(0.3, 105.0, 2.0, 100.0, 7);

    for (int i = 0; i < 20; ++i)
        CHECK(a.next() == b.next());
}

TEST_CASE("ou_process with zero volatility decays deterministically toward theta", "[ou_process]") {
    ou_process p(0.5, 100.0, 0.0, 200.0);
    double prev = 200.0;
    for (int i = 0; i < 50; ++i) {
        const double next = p.next();
        // Monotonic decay toward theta when starting above it, sigma == 0.
        CHECK(next <= prev);
        CHECK(next >= 100.0);
        prev = next;
    }
    CHECK(prev == Catch::Approx(100.0).margin(1e-6));
}

TEST_CASE("ou_process mean-reverts toward theta over a long run (statistical)", "[ou_process]") {
    ou_process p(0.2, 100.0, 1.0, 100.0, 123);
    std::vector<double> tail;
    for (int i = 0; i < 5000; ++i) {
        const double v = p.next();
        if (i >= 4000)
            tail.push_back(v);
    }
    const double mean = std::accumulate(tail.begin(), tail.end(), 0.0) / tail.size();
    CHECK(mean == Catch::Approx(100.0).margin(1.0));
}

TEST_CASE("ou_process with kappa <= 0 degenerates to a driftless random walk", "[ou_process]") {
    // kappa == 0 must not divide by zero and must not mean-revert.
    ou_process p(0.0, 100.0, 0.0, 100.0);
    // sigma == 0 makes the walk deterministic: price never changes.
    for (int i = 0; i < 10; ++i)
        CHECK(p.next() == Catch::Approx(100.0));
}

TEST_CASE("ou_process rejects negative sigma", "[ou_process]") {
    CHECK_THROWS_AS(ou_process(0.1, 100.0, -1.0, 100.0), std::invalid_argument);
}

TEST_CASE("ou_process rejects a non-positive initial price", "[ou_process]") {
    CHECK_THROWS_AS(ou_process(0.1, 100.0, 1.0, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(ou_process(0.1, 100.0, 1.0, -5.0), std::invalid_argument);
}
