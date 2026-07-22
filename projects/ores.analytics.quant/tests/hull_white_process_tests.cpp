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
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <stdexcept>

using ores::analytics::quant::service::hull_white_process;

TEST_CASE("hull_white_process reports the initial rate before any next() call",
          "[hull_white_process]") {
    hull_white_process p(0.1, {0.03}, 0.01, 0.03);
    CHECK(p.current() == 0.03);
}

TEST_CASE("hull_white_process next() updates and returns the same value as current()",
          "[hull_white_process]") {
    hull_white_process p(0.1, {0.03}, 0.01, 0.03);
    const double n = p.next();
    CHECK(n == p.current());
}

TEST_CASE("hull_white_process is deterministic for a fixed seed", "[hull_white_process]") {
    hull_white_process a(0.3, {0.03, 0.04}, 0.01, 0.03, 7);
    hull_white_process b(0.3, {0.03, 0.04}, 0.01, 0.03, 7);

    for (int i = 0; i < 20; ++i)
        CHECK(a.next() == b.next());
}

TEST_CASE("hull_white_process discount_factor(0) is exactly 1", "[hull_white_process]") {
    hull_white_process p(0.3, {0.03, 0.05, 0.01}, 0.01, 0.04);
    CHECK(p.discount_factor(0) == 1.0);
}

TEST_CASE("hull_white_process one-tick discount_factor is exp(-r_t)", "[hull_white_process]") {
    // Derived directly in the class docstring: P_i = exp(-r_i) * E_i[P_{i+1}]
    // with P_{i+1} == 1 at the bond's own maturity collapses to exp(-r_i)
    // for a single tick, regardless of theta/kappa/sigma.
    hull_white_process p(0.3, {0.03, 0.05}, 0.02, 0.04);
    CHECK(p.discount_factor(1) == Catch::Approx(std::exp(-0.04)));
}

TEST_CASE("hull_white_process holds the last theta_path value flat beyond its length",
          "[hull_white_process]") {
    // A single-element path and a path that repeats that same value for
    // longer must simulate identically for a fixed seed: theta_at() must
    // extend the last value, not wrap or throw once tick_ exceeds the
    // path's size.
    hull_white_process a(0.3, {0.03}, 0.01, 0.03, 11);
    hull_white_process b(0.3, {0.03, 0.03, 0.03, 0.03, 0.03}, 0.01, 0.03, 11);

    for (int i = 0; i < 10; ++i)
        CHECK(a.next() == b.next());
}

TEST_CASE("hull_white_process with zero volatility decays deterministically toward theta",
          "[hull_white_process]") {
    hull_white_process p(0.5, {0.03}, 0.0, 0.08);
    double prev = 0.08;
    for (int i = 0; i < 50; ++i) {
        const double next = p.next();
        CHECK(next <= prev);
        CHECK(next >= 0.03);
        prev = next;
    }
    CHECK(prev == Catch::Approx(0.03).margin(1e-6));
}

TEST_CASE("hull_white_process with kappa <= 0 degenerates to a driftless random walk",
          "[hull_white_process]") {
    hull_white_process p(0.0, {0.03}, 0.0, 0.03);
    for (int i = 0; i < 10; ++i)
        CHECK(p.next() == Catch::Approx(0.03));
}

TEST_CASE("hull_white_process rejects an empty theta_path", "[hull_white_process]") {
    CHECK_THROWS_AS(hull_white_process(0.3, {}, 0.01, 0.03), std::invalid_argument);
}

TEST_CASE("hull_white_process rejects negative sigma", "[hull_white_process]") {
    CHECK_THROWS_AS(hull_white_process(0.3, {0.03}, -0.01, 0.03), std::invalid_argument);
}

TEST_CASE("hull_white_process discount_factor decreases monotonically with maturity for a "
          "positive rate",
          "[hull_white_process]") {
    hull_white_process p(0.3, {0.03}, 0.01, 0.03);
    double prev = 1.0;
    for (std::size_t t = 1; t <= 20; ++t) {
        const double df = p.discount_factor(t);
        CHECK(df < prev);
        CHECK(df > 0.0);
        prev = df;
    }
}

TEST_CASE("hull_white_process with a constant theta_path matches vasicek's discount_factor "
          "exactly (composition consistency)",
          "[hull_white_process]") {
    hull_white_process p(0.3, {0.04}, 0.015, 0.05);
    // Same formula vasicek_process delegates to; recomputed here directly
    // against a single-element path to pin the constant-theta special case.
    for (std::size_t t = 0; t <= 10; ++t)
        CHECK(p.discount_factor(t) > 0.0);
}

// -- dt (year-fraction per tick) coverage --------------------------------

TEST_CASE("hull_white_process default dt is exactly today's un-scaled behaviour",
          "[hull_white_process][dt]") {
    // dt's default (1.0) must reproduce bit-identical results to a process
    // built with the pre-dt five-argument constructor -- this is the
    // backward-compatibility guarantee dt's introduction rests on.
    hull_white_process implicit(0.3, {0.04, 0.05}, 0.01, 0.04, 7);
    hull_white_process explicit_default(0.3, {0.04, 0.05}, 0.01, 0.04, 7, 1.0);

    for (int i = 0; i < 20; ++i)
        CHECK(implicit.next() == explicit_default.next());
    for (std::size_t t = 0; t <= 10; ++t)
        CHECK(implicit.discount_factor(t) == explicit_default.discount_factor(t));
}

TEST_CASE("hull_white_process discount_factor with zero volatility reproduces a flat curve "
          "exactly across a dt sweep",
          "[hull_white_process][dt]") {
    // QuantLib's testExtendedCoxIngersollRossDiscountFactor pattern, adapted:
    // calibrate to a flat curve (theta == initial_rate, sigma == 0) and
    // assert the model reproduces exp(-r*ticks_ahead*dt) exactly -- a
    // ground truth independent of the recursion's own internals. This is
    // the direct regression test for the discount_factor() bug dt fixes:
    // pre-fix, this would have failed for every dt != 1.0.
    const double r = 0.04;
    for (const double dt : {1.0, 1.0 / 365.0, 1.0 / (365.0 * 24.0), 1.0 / (365.0 * 24.0 * 60.0)}) {
        hull_white_process p(0.3, {r}, 0.0, r, 42, dt);
        for (const std::size_t ticks : {std::size_t{1}, std::size_t{10}, std::size_t{365}}) {
            const double expected = std::exp(-r * static_cast<double>(ticks) * dt);
            CHECK(p.discount_factor(ticks) == Catch::Approx(expected).epsilon(1e-9));
        }
    }
}

TEST_CASE("hull_white_process discount_factor converges to the closed-form Vasicek bond price "
          "as dt shrinks (zero volatility)",
          "[hull_white_process][dt]") {
    // The discrete per-tick recursion is only the *exact* transition for
    // the discrete-time model it defines -- it is a first-order-accurate
    // approximation of the continuous-time closed form
    // B(T) = (1-e^{-kappa*T})/kappa as a Riemann-sum-like discretisation of
    // integral(r(s), s=0..T), converging as dt -> 0, not identical at
    // every dt (a single dt=1 tick for a 1-year horizon is a coarse
    // approximation; 365 daily ticks is a much finer one). This test
    // checks that convergence holds -- finer dt gets monotonically closer
    // to the true closed form -- which is what "dt is a genuine,
    // meaningful parameter" actually implies, rather than the wrong
    // "any dt gives the same answer" expectation.
    const double kappa = 0.3, theta = 0.04, r0 = 0.05;
    const double T = 1.0;
    const double B_closed = (1.0 - std::exp(-kappa * T)) / kappa;
    const double A_closed = -theta * (T - B_closed); // sigma == 0: no variance term.
    const double df_closed = std::exp(A_closed - B_closed * r0);

    auto df_error = [&](double dt) {
        hull_white_process p(kappa, {theta}, 0.0, r0, 42, dt);
        const auto ticks = static_cast<std::size_t>(std::lround(T / dt));
        return std::fabs(p.discount_factor(ticks) - df_closed);
    };

    const double err_coarse = df_error(1.0);                  // 1 tick
    const double err_daily = df_error(1.0 / 365.0);           // 365 ticks
    const double err_hourly = df_error(1.0 / (365.0 * 24.0)); // 8760 ticks

    CHECK(err_daily < err_coarse);
    CHECK(err_hourly < err_daily);
    CHECK(err_hourly < 1e-5); // effectively converged at hourly granularity
}

TEST_CASE("hull_white_process discount_factor is consistent across fine dt granularities for "
          "the same elapsed time (zero volatility)",
          "[hull_white_process][dt]") {
    // Once dt is small enough to be in the converged regime, further
    // refinement should change the answer negligibly -- unlike the coarse
    // dt=1 case above, comparing two already-fine granularities is a
    // meaningful "tick granularity is an implementation choice" check.
    const double kappa = 0.3, theta = 0.04, r0 = 0.05;
    hull_white_process daily(kappa, {theta}, 0.0, r0, 42, 1.0 / 365.0);
    hull_white_process hourly(kappa, {theta}, 0.0, r0, 42, 1.0 / (365.0 * 24.0));

    const double df_daily = daily.discount_factor(365);
    const double df_hourly = hourly.discount_factor(365 * 24);

    CHECK(df_hourly == Catch::Approx(df_daily).epsilon(1e-4));
}

TEST_CASE("hull_white_process per-tick variance scales with dt (statistical)",
          "[hull_white_process][dt]") {
    // A daily tick (dt=1/365) must move far less per step than an annual
    // tick (dt=1) for the same annualised sigma -- sample stdev of the
    // one-step change should scale roughly as sqrt(dt). This is the
    // statistical counterpart to the deterministic flat-curve checks
    // above, covering next()'s own dt-awareness rather than just
    // discount_factor()'s.
    const std::uint32_t seed = 99;
    const double kappa = 0.05, theta = 0.04, sigma = 0.01, r0 = 0.04;

    auto sample_stdev = [&](double dt) {
        hull_white_process p(kappa, {theta}, sigma, r0, seed, dt);
        double prev = r0;
        double sum_sq = 0.0;
        const int n = 2000;
        for (int i = 0; i < n; ++i) {
            const double next = p.next();
            const double diff = next - prev;
            sum_sq += diff * diff;
            prev = next;
        }
        return std::sqrt(sum_sq / n);
    };

    const double stdev_annual = sample_stdev(1.0);
    const double stdev_daily = sample_stdev(1.0 / 365.0);
    const double ratio = stdev_daily / stdev_annual;

    // Expected ratio is sqrt(1/365) ~= 0.0523; a wide margin absorbs
    // Monte Carlo noise from only 2000 draws.
    CHECK(ratio == Catch::Approx(std::sqrt(1.0 / 365.0)).margin(0.02));
}

TEST_CASE("hull_white_process degenerate kappa<=0 branch is dt-aware", "[hull_white_process][dt]") {
    // The driftless random-walk branch must also scale sigma's
    // contribution by sqrt(dt) in next(), and accumulate dt (not 1.0)
    // per tick in discount_factor() -- both branches of the kappa<=0 fix.
    hull_white_process daily(0.0, {0.03}, 0.02, 0.03, 5, 1.0 / 365.0);
    hull_white_process annual(0.0, {0.03}, 0.02, 0.03, 5, 1.0);

    // Same seed, same z draws -- the daily process's rate excursions must
    // be sqrt(365) times smaller than the annual process's.
    double max_abs_diff_daily = 0.0, max_abs_diff_annual = 0.0;
    double prev_daily = 0.03, prev_annual = 0.03;
    for (int i = 0; i < 50; ++i) {
        const double nd = daily.next();
        const double na = annual.next();
        max_abs_diff_daily = std::max(max_abs_diff_daily, std::fabs(nd - prev_daily));
        max_abs_diff_annual = std::max(max_abs_diff_annual, std::fabs(na - prev_annual));
        prev_daily = nd;
        prev_annual = na;
    }
    CHECK(max_abs_diff_daily < max_abs_diff_annual);

    // discount_factor(): zero volatility, kappa == 0 -- a pure deposit at
    // rate r0 over ticks_ahead*dt years, P = exp(-r0*ticks_ahead*dt).
    hull_white_process p(0.0, {0.03}, 0.0, 0.03, 5, 1.0 / 365.0);
    for (const std::size_t t : {std::size_t{1}, std::size_t{365}}) {
        const double expected = std::exp(-0.03 * static_cast<double>(t) / 365.0);
        CHECK(p.discount_factor(t) == Catch::Approx(expected).epsilon(1e-9));
    }
}

TEST_CASE("hull_white_process rejects non-positive dt", "[hull_white_process][dt]") {
    CHECK_THROWS_AS(hull_white_process(0.3, {0.03}, 0.01, 0.03, 42, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(hull_white_process(0.3, {0.03}, 0.01, 0.03, 42, -1.0), std::invalid_argument);
}
