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
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include "ores.analytics.quant/service/processes/vasicek_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <numeric>
#include <vector>

using ores::analytics::quant::service::hull_white_process;
using ores::analytics::quant::service::vasicek_process;

TEST_CASE("vasicek_process reports the initial rate before any next() call",
    "[vasicek_process]") {
    vasicek_process p(0.3, 0.03, 0.01, 0.03);
    CHECK(p.current() == 0.03);
}

TEST_CASE("vasicek_process next() updates and returns the same value as current()",
    "[vasicek_process]") {
    vasicek_process p(0.3, 0.03, 0.01, 0.03);
    const double n = p.next();
    CHECK(n == p.current());
}

TEST_CASE("vasicek_process is deterministic for a fixed seed", "[vasicek_process]") {
    vasicek_process a(0.3, 0.04, 0.02, 0.05, 7);
    vasicek_process b(0.3, 0.04, 0.02, 0.05, 7);

    for (int i = 0; i < 20; ++i)
        CHECK(a.next() == b.next());
}

TEST_CASE("vasicek_process mean-reverts toward theta over a long run (statistical)",
    "[vasicek_process]") {
    vasicek_process p(0.2, 0.03, 0.01, 0.06, 123);
    std::vector<double> tail;
    for (int i = 0; i < 5000; ++i) {
        const double v = p.next();
        if (i >= 4000)
            tail.push_back(v);
    }
    const double mean = std::accumulate(tail.begin(), tail.end(), 0.0) / tail.size();
    CHECK(mean == Catch::Approx(0.03).margin(0.01));
}

TEST_CASE("vasicek_process is exactly hull_white_process composed with a constant theta_path "
    "(next())",
    "[vasicek_process]") {
    // Vasicek is not a separately-derived model here; it must produce
    // bit-identical simulated paths to the single-element-theta_path case
    // of hull_white_process, since that is literally what it delegates to.
    vasicek_process v(0.3, 0.04, 0.015, 0.05, 99);
    hull_white_process h(0.3, {0.04}, 0.015, 0.05, 99);

    for (int i = 0; i < 30; ++i)
        CHECK(v.next() == h.next());
}

TEST_CASE("vasicek_process is exactly hull_white_process composed with a constant theta_path "
    "(discount_factor())",
    "[vasicek_process]") {
    vasicek_process v(0.3, 0.04, 0.015, 0.05);
    hull_white_process h(0.3, {0.04}, 0.015, 0.05);

    for (std::size_t t = 0; t <= 15; ++t)
        CHECK(v.discount_factor(t) == h.discount_factor(t));
}

TEST_CASE("vasicek_process discount_factor(0) is exactly 1", "[vasicek_process]") {
    vasicek_process p(0.3, 0.04, 0.01, 0.04);
    CHECK(p.discount_factor(0) == 1.0);
}

TEST_CASE("vasicek_process one-tick discount_factor is exp(-r_t)", "[vasicek_process]") {
    vasicek_process p(0.3, 0.04, 0.02, 0.05);
    CHECK(p.discount_factor(1) == Catch::Approx(std::exp(-0.05)));
}
