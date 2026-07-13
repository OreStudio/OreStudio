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
#include "ores.analytics.quant/service/processes/cir_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <numeric>
#include <stdexcept>
#include <vector>

using ores::analytics::quant::service::cir_process;

TEST_CASE("cir_process reports the initial rate before any next() call", "[cir_process]") {
    cir_process p(0.3, 0.03, 0.05, 0.03);
    CHECK(p.current() == 0.03);
}

TEST_CASE("cir_process next() updates and returns the same value as current()", "[cir_process]") {
    cir_process p(0.3, 0.03, 0.05, 0.03);
    const double n = p.next();
    CHECK(n == p.current());
}

TEST_CASE("cir_process is deterministic for a fixed seed", "[cir_process]") {
    cir_process a(0.5, 0.04, 0.1, 0.05, 7);
    cir_process b(0.5, 0.04, 0.1, 0.05, 7);

    for (int i = 0; i < 20; ++i)
        CHECK(a.next() == b.next());
}

TEST_CASE("cir_process never produces a negative rate over a long run", "[cir_process]") {
    // Feller condition satisfied (2*kappa*theta = 0.06 >= sigma^2 = 0.04):
    // rate should stay strictly positive.
    cir_process p(0.6, 0.05, 0.2, 0.05, 321);
    for (int i = 0; i < 5000; ++i)
        CHECK(p.next() >= 0.0);
}

TEST_CASE("cir_process never produces a negative rate even when the Feller condition is "
    "violated",
    "[cir_process]") {
    // 2*kappa*theta = 0.02 < sigma^2 = 0.25: the process can touch zero,
    // but the exact simulation must never go negative.
    cir_process p(0.1, 0.1, 0.5, 0.05, 321);
    for (int i = 0; i < 5000; ++i)
        CHECK(p.next() >= 0.0);
}

TEST_CASE("cir_process mean-reverts toward theta over a long run (statistical)",
    "[cir_process]") {
    cir_process p(0.6, 0.05, 0.1, 0.1, 123);
    std::vector<double> tail;
    for (int i = 0; i < 8000; ++i) {
        const double v = p.next();
        if (i >= 7000)
            tail.push_back(v);
    }
    const double mean = std::accumulate(tail.begin(), tail.end(), 0.0) / tail.size();
    CHECK(mean == Catch::Approx(0.05).margin(0.02));
}

TEST_CASE("cir_process with zero volatility follows the deterministic mean-reversion ODE "
    "exactly",
    "[cir_process]") {
    cir_process p(0.5, 0.03, 0.0, 0.08);
    double prev = 0.08;
    for (int i = 0; i < 50; ++i) {
        const double next = p.next();
        CHECK(next <= prev);
        CHECK(next >= 0.03);
        prev = next;
    }
    CHECK(prev == Catch::Approx(0.03).margin(1e-6));
}

TEST_CASE("cir_process handles a zero initial rate without producing NaN", "[cir_process]") {
    // lambda (non-centrality) is exactly 0 when r_t == 0 -- must not divide
    // by zero or otherwise misbehave.
    cir_process p(0.3, 0.03, 0.05, 0.0, 55);
    for (int i = 0; i < 100; ++i) {
        const double v = p.next();
        CHECK(v >= 0.0);
        CHECK_FALSE(std::isnan(v));
    }
}

TEST_CASE("cir_process discount_factor(0) is exactly 1", "[cir_process]") {
    cir_process p(0.3, 0.03, 0.05, 0.03);
    CHECK(p.discount_factor(0) == 1.0);
}

TEST_CASE("cir_process discount_factor decreases monotonically with maturity", "[cir_process]") {
    cir_process p(0.3, 0.03, 0.05, 0.03);
    double prev = 1.0;
    for (std::size_t t = 1; t <= 20; ++t) {
        const double df = p.discount_factor(t);
        CHECK(df < prev);
        CHECK(df > 0.0);
        prev = df;
    }
}

TEST_CASE("cir_process discount_factor with zero volatility matches the deterministic "
    "closed form",
    "[cir_process]") {
    cir_process p(0.5, 0.03, 0.0, 0.05);
    // r(s) = theta + (r0-theta)*e^{-kappa*s}; integral over [0,tau] has a
    // simple closed form -- recomputed independently here as an oracle.
    const double kappa = 0.5, theta = 0.03, r0 = 0.05;
    for (std::size_t t = 1; t <= 10; ++t) {
        const double tau = static_cast<double>(t);
        const double integral = theta * tau + (r0 - theta) * (1.0 - std::exp(-kappa * tau)) / kappa;
        CHECK(p.discount_factor(t) == Catch::Approx(std::exp(-integral)));
    }
}

TEST_CASE("cir_process discount_factor stays finite and in (0, 1] for long maturities",
    "[cir_process]") {
    // Regression test: the un-normalised textbook formula computes
    // exp(gamma*tau) directly, which overflows to +inf once gamma*tau
    // exceeds ~709 -- e.g. gamma around 0.3 with tau in the thousands
    // (a realistic daily-tick 10Y+ tenor) -- collapsing the result to
    // NaN instead of a valid price.
    cir_process p(0.3, 0.03, 0.05, 0.03);
    for (const std::size_t t : {100, 1000, 5000, 10000}) {
        const double df = p.discount_factor(t);
        CHECK(std::isfinite(df));
        CHECK(df > 0.0);
        CHECK(df <= 1.0);
    }
}

TEST_CASE("cir_process rejects non-positive kappa", "[cir_process]") {
    CHECK_THROWS_AS(cir_process(0.0, 0.03, 0.05, 0.03), std::invalid_argument);
    CHECK_THROWS_AS(cir_process(-0.1, 0.03, 0.05, 0.03), std::invalid_argument);
}

TEST_CASE("cir_process rejects non-positive theta", "[cir_process]") {
    CHECK_THROWS_AS(cir_process(0.3, 0.0, 0.05, 0.03), std::invalid_argument);
    CHECK_THROWS_AS(cir_process(0.3, -0.01, 0.05, 0.03), std::invalid_argument);
}

TEST_CASE("cir_process rejects negative sigma", "[cir_process]") {
    CHECK_THROWS_AS(cir_process(0.3, 0.03, -0.01, 0.03), std::invalid_argument);
}

TEST_CASE("cir_process rejects a negative initial rate", "[cir_process]") {
    CHECK_THROWS_AS(cir_process(0.3, 0.03, 0.05, -0.001), std::invalid_argument);
}

TEST_CASE("cir_process accepts a zero initial rate", "[cir_process]") {
    CHECK_NOTHROW(cir_process(0.3, 0.03, 0.05, 0.0));
}
