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
#include "ores.analytics.quant/service/processes/heath_jarrow_morton_params.hpp"
#include "ores.analytics.quant/service/processes/heath_jarrow_morton_process.hpp"
#include <Eigen/Dense>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <initializer_list>
#include <limits>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using ores::analytics::quant::service::heath_jarrow_morton_params;
using ores::analytics::quant::service::heath_jarrow_morton_process;
using ores::analytics::quant::service::hjm_no_arbitrage_drift;

namespace {

Eigen::VectorXd vector(std::initializer_list<double> values) {
    Eigen::VectorXd result(values.size());
    std::size_t i = 0;
    for (const double v : values)
        result[i++] = v;
    return result;
}

/**
 * @brief Welford running mean and variance for scalar samples.
 */
struct running_mean_variance {
    double mean = 0.0;
    double m2 = 0.0;
    std::size_t count = 0;

    void add(double x) {
        ++count;
        const double delta = x - mean;
        mean += delta / static_cast<double>(count);
        m2 += delta * (x - mean);
    }

    double sample_variance() const {
        return m2 / static_cast<double>(count - 1);
    }
};

/**
 * @brief Welford running mean and covariance for vector samples.
 */
struct running_moments {
    Eigen::VectorXd mean;
    Eigen::MatrixXd m2;
    std::size_t count = 0;

    explicit running_moments(std::size_t dimension)
        : mean(Eigen::VectorXd::Zero(dimension))
        , m2(Eigen::MatrixXd::Zero(dimension, dimension)) {}

    void add(const Eigen::VectorXd& sample) {
        ++count;
        const Eigen::VectorXd delta = sample - mean;
        mean += delta / static_cast<double>(count);
        m2 += (sample - mean) * delta.transpose();
    }

    Eigen::MatrixXd sample_covariance() const {
        return m2 / static_cast<double>(count - 1);
    }
};

} // namespace

TEST_CASE("hjm zero-volatility curve is frozen", "[heath_jarrow_morton_process]") {
    // sigma = 0 freezes every grid rate: the short rate is constant and
    // the discount factors come from the initial curve alone. The grid
    // is tenors {0, 0.5, 1.5} over rates {0.04, 0.05, 0.045}, so the
    // integral of the initial curve is
    //     [0, 0.5]:     0.5 * (0.04 + 0.05) / 2             = 0.0225
    //     [0.5, 1.0]:   0.5 * (0.05 + 0.0475) / 2           = 0.024375
    //     [1.0, 1.5]:   1.0 * (0.05 + 0.045) / 2            = 0.0475
    //     beyond 1.5:   flat 0.045
    // and the horizon in years is ticks * dt. Only horizons that are
    // exact multiples of dt are representable on the grid: at dt = 1.0
    // the 0.5 and 1.5 horizons fall between ticks. std::round() would
    // silently shift them onto a different horizon (0.5 / 1.0 rounds to
    // 1 tick, horizon 1.0), so the representable mapping is spelled out
    // per dt instead.
    for (const double dt : {0.25, 1.0}) {
        heath_jarrow_morton_process p(
            vector({0.04, 0.05, 0.045}), vector({0.0, 0.0, 0.0}), vector({0.5, 1.0}), 42, dt);
        REQUIRE(p.current() == Catch::Approx(0.04).epsilon(1e-15));
        REQUIRE(p.discount_factor(0) == Catch::Approx(1.0).epsilon(1e-15));

        const std::vector<std::pair<double, double>> expectations =
            dt == 1.0 ? std::vector<std::pair<double, double>>{{1.0, std::exp(-0.046875)},
                                                               {2.0, std::exp(-0.0925)}} :
                        std::vector<std::pair<double, double>>{{0.5, std::exp(-0.0225)},
                                                               {1.0, std::exp(-0.046875)},
                                                               {1.5, std::exp(-0.07)},
                                                               {2.0, std::exp(-0.0925)}};
        for (const auto& [horizon, expected] : expectations)
            REQUIRE(p.discount_factor(static_cast<std::size_t>(horizon / dt)) ==
                    Catch::Approx(expected).epsilon(1e-15));

        // The curve never moves.
        for (std::size_t t = 0; t < 3; ++t)
            REQUIRE(p.next() == Catch::Approx(0.04).epsilon(1e-15));
    }

    // A horizon inside the first segment integrates the partial linear
    // piece: horizon 0.25 cuts [0, 0.5] at the interpolated rate 0.045.
    heath_jarrow_morton_process partial(
        vector({0.04, 0.05, 0.045}), vector({0.0, 0.0, 0.0}), vector({0.5, 1.0}), 42, 0.25);
    REQUIRE(partial.discount_factor(1) == Catch::Approx(std::exp(-0.010625)).epsilon(1e-15));
}

TEST_CASE("hjm single-rate grid is flat forever", "[heath_jarrow_morton_process]") {
    heath_jarrow_morton_process p(vector({0.04}), vector({0.0}), vector({}), 42, 0.25);
    REQUIRE(p.discount_factor(5) == Catch::Approx(std::exp(-0.04 * 5 * 0.25)).epsilon(1e-15));
    for (std::size_t t = 0; t < 3; ++t)
        REQUIRE(p.next() == Catch::Approx(0.04).epsilon(1e-15));
}

TEST_CASE("hjm no-arbitrage drift formula", "[heath_jarrow_morton_process]") {
    // The drift of tenor i is sigma_i times the integral of the vol
    // curve from the front to tenor i; the front rate has zero drift.
    const Eigen::VectorXd drift =
        hjm_no_arbitrage_drift(vector({0.2, 0.3, 0.1}), vector({0.5, 1.0}));
    REQUIRE(drift.size() == 3);
    REQUIRE(drift[0] == Catch::Approx(0.0).epsilon(1e-15));
    REQUIRE(drift[1] == Catch::Approx(0.3 * (0.3 * 0.5)).epsilon(1e-15));
    REQUIRE(drift[2] == Catch::Approx(0.1 * (0.3 * 0.5 + 0.1 * 1.0)).epsilon(1e-15));

    const Eigen::VectorXd drift_big =
        hjm_no_arbitrage_drift(vector({0.2, 0.3, 0.4, 0.5}), vector({0.5, 0.5, 1.0}));
    REQUIRE(drift_big[0] == Catch::Approx(0.0).epsilon(1e-15));
    REQUIRE(drift_big[1] == Catch::Approx(0.3 * 0.15).epsilon(1e-15));
    REQUIRE(drift_big[2] == Catch::Approx(0.4 * 0.35).epsilon(1e-15));
    REQUIRE(drift_big[3] == Catch::Approx(0.5 * 0.85).epsilon(1e-15));
}

TEST_CASE("hjm drift matches the grid integral statistically", "[heath_jarrow_morton_process]") {
    // One tick of the linear transition: E[f(t+dt) - f(t)] = mu * dt
    // exactly, so the sample mean of the increment per unit time must
    // reproduce the hand-computed drift vector, and the front rate's
    // increment must be a zero-mean martingale with variance
    // sigma_0^2 * dt.
    const std::size_t paths = 100000;
    running_moments increments(4);
    running_mean_variance front_increment;

    for (std::size_t i = 0; i < paths; ++i) {
        heath_jarrow_morton_process p(vector({0.04, 0.042, 0.041, 0.045}),
                                      vector({0.2, 0.3, 0.4, 0.5}),
                                      vector({0.5, 0.5, 1.0}),
                                      static_cast<std::uint32_t>(42 + i),
                                      0.25);
        const Eigen::VectorXd before = p.forward_rates();
        p.next();
        const Eigen::VectorXd after = p.forward_rates();
        increments.add((after - before) / 0.25);
        front_increment.add(after[0] - before[0]);
    }

    const Eigen::VectorXd& mean = increments.mean;
    // 1e-2 is >= 3.2 sigma of the noisiest mean (rate 3: sigma = 0.5,
    // standard error ~3.2e-3 at 100000 paths); sub-sigma bounds flake
    // because std::normal_distribution draws differ per standard
    // library, so the same seeds produce different samples on macOS.
    REQUIRE(mean[0] == Catch::Approx(0.0).margin(1e-2));
    REQUIRE(mean[1] == Catch::Approx(0.045).margin(1e-2));
    REQUIRE(mean[2] == Catch::Approx(0.14).margin(1e-2));
    REQUIRE(mean[3] == Catch::Approx(0.425).margin(1e-2));

    REQUIRE(front_increment.mean == Catch::Approx(0.0).margin(1e-2));
    REQUIRE(front_increment.sample_variance() == Catch::Approx(0.2 * 0.2 * 0.25).epsilon(1e-2));
}

TEST_CASE("hjm one-tick discounted bond ratio", "[heath_jarrow_morton_process]") {
    // A bond maturing at the next tick has price 1 then, so the ratio
    // exp(-r*dt) / P(t, t+dt) is the martingale ratio over one tick.
    // With a flat curve it is 1 up to O(dt^2) terms; the tolerance
    // absorbs that residual plus the sampling noise.
    const std::size_t paths = 100000;
    running_mean_variance ratio;

    for (std::size_t i = 0; i < paths; ++i) {
        heath_jarrow_morton_process p(vector({0.05, 0.05, 0.05}),
                                      vector({0.3, 0.3, 0.3}),
                                      vector({0.25, 0.25}),
                                      static_cast<std::uint32_t>(42 + i),
                                      0.25);
        const double price_now = p.discount_factor(1);
        const double r = p.current();
        p.next();
        ratio.add(std::exp(-r * 0.25) / price_now);
    }
    REQUIRE(ratio.mean == Catch::Approx(1.0).margin(3e-3));
}

TEST_CASE("hjm discounted bond prices are martingales", "[heath_jarrow_morton_process]") {
    // The defining HJM property: with the no-arbitrage drift, the
    // money-market-rebased price of a fixed-maturity bond is a
    // martingale. The discretised model satisfies this only up to an
    // O(dt) Euler/trapezoid bias, which for this flat uniform-grid
    // setup is closed form. The shocks from earlier ticks cancel
    // between the numerator and the denominator of each leg's ratio
    // (the trapezoid weights sum to the horizon), so leg k's ratio
    // involves only that tick's shock:
    //     ln R_k = -f0*dt + F(h_k) - F(h_{k+1})
    //              + dt*(k*M(h_k) - (k+1)*M(h_{k+1}))
    //              - sigma*sqrt(dt)*h_{k+1}*Z
    // with h_k = (m-k)*dt the remaining bond life at leg k, F the
    // integral of the initial flat curve and M the trapezoid integral
    // of the drift curve. Taking expectations removes the shock:
    //     E[R_k] = exp(det_k + sigma^2*dt*h_{k+1}^2/2).
    // The bias is exactly zero on the first and last legs (the drift
    // integral and the Ito correction cancel) and peaks at about 3%
    // on the middle legs. The simulated means are checked against
    // this closed form: a wrong drift or a broken bond price would
    // move them by several percent, far outside the tolerance.
    const double f0 = 0.05;
    const double sigma = 0.5;
    const double dt = 0.25;
    const std::size_t paths = 50000;
    const std::size_t ticks = 6;
    Eigen::VectorXd rates(7), vols(7), spacings(6);
    rates.setConstant(f0);
    vols.setConstant(sigma);
    spacings.setConstant(dt);
    const Eigen::VectorXd drift = hjm_no_arbitrage_drift(vols, spacings);

    // Trapezoid integral of a grid curve over [0, h], h on the grid.
    const auto integral = [&](const Eigen::VectorXd& curve, double h) {
        const std::size_t n = static_cast<std::size_t>(h / dt);
        double result = 0.0;
        for (std::size_t i = 0; i <= n; ++i) {
            const double w = (i == 0 || i == n) ? dt / 2.0 : dt;
            result += w * curve[i];
        }
        return result;
    };

    std::vector<running_mean_variance> ratios(ticks - 1);
    for (std::size_t i = 0; i < paths; ++i) {
        heath_jarrow_morton_process p(
            rates, vols, spacings, static_cast<std::uint32_t>(42 + i), dt);
        for (std::size_t k = 0; k + 1 < ticks; ++k) {
            const double r = p.current();
            const double price_now = p.discount_factor(ticks - k);
            p.next();
            const double price_next = p.discount_factor(ticks - k - 1);
            ratios[k].add(std::exp(-r * dt) * price_next / price_now);
        }
    }

    for (std::size_t k = 0; k + 1 < ticks; ++k) {
        const double h_k = (ticks - k) * dt;
        const double h_next = h_k - dt;
        const double det = -f0 * dt + f0 * (h_k - h_next) +
                           dt * (static_cast<double>(k) * integral(drift, h_k) -
                                 static_cast<double>(k + 1) * integral(drift, h_next));
        const double expected = std::exp(det + sigma * sigma * dt * h_next * h_next / 2.0);
        REQUIRE(ratios[k].mean == Catch::Approx(expected).margin(1e-2));
    }
}

TEST_CASE("hjm params struct equals scalar construction", "[heath_jarrow_morton_process]") {
    heath_jarrow_morton_params params;
    params.initial_forward_rates = vector({0.04, 0.042, 0.041});
    params.volatilities = vector({0.2, 0.3, 0.1});
    params.tenor_spacings = vector({0.5, 1.0});

    heath_jarrow_morton_process from_params(params, 99, 0.5);
    heath_jarrow_morton_process from_scalars(
        vector({0.04, 0.042, 0.041}), vector({0.2, 0.3, 0.1}), vector({0.5, 1.0}), 99, 0.5);
    for (std::size_t t = 0; t < 5; ++t) {
        REQUIRE(from_params.next() == from_scalars.next());
        REQUIRE(from_params.discount_factor(3) == from_scalars.discount_factor(3));
    }
}

TEST_CASE("hjm seeds determine the path", "[heath_jarrow_morton_process]") {
    const auto path = [](std::uint32_t seed) {
        heath_jarrow_morton_process p(
            vector({0.04, 0.042, 0.041}), vector({0.2, 0.3, 0.1}), vector({0.5, 1.0}), seed, 0.5);
        std::vector<double> rates;
        for (std::size_t t = 0; t < 5; ++t)
            rates.push_back(p.next());
        return rates;
    };
    REQUIRE(path(42) == path(42));
    REQUIRE(path(42) != path(43));
}

TEST_CASE("hjm rejects invalid parameters", "[heath_jarrow_morton_process]") {
    REQUIRE_NOTHROW(heath_jarrow_morton_process(
        vector({0.04, 0.042, 0.041}), vector({0.2, 0.3, 0.1}), vector({0.5, 1.0}), 42, 0.5));

    REQUIRE_THROWS_AS(heath_jarrow_morton_process(
                          vector({}), vector({0.2, 0.3, 0.1}), vector({0.5, 1.0}), 42, 0.5),
                      std::invalid_argument); // empty initial_forward_rates
    REQUIRE_THROWS_AS(
        heath_jarrow_morton_process(
            vector({0.04, 0.042, 0.041}), vector({0.2, 0.3}), vector({0.5, 1.0}), 42, 0.5),
        std::invalid_argument); // volatilities size mismatch
    REQUIRE_THROWS_AS(heath_jarrow_morton_process(vector({0.04, 0.042, 0.041}),
                                                  vector({0.2, 0.3, 0.1}),
                                                  vector({0.5, 1.0, 0.5}),
                                                  42,
                                                  0.5),
                      std::invalid_argument); // tenor_spacings size mismatch
    REQUIRE_THROWS_AS(
        heath_jarrow_morton_process(
            vector({0.04, 0.042, 0.041}), vector({0.2, -0.3, 0.1}), vector({0.5, 1.0}), 42, 0.5),
        std::invalid_argument); // negative vol
    REQUIRE_THROWS_AS(
        heath_jarrow_morton_process(vector({0.04, 0.042, 0.041}),
                                    vector({0.2, std::numeric_limits<double>::quiet_NaN(), 0.1}),
                                    vector({0.5, 1.0}),
                                    42,
                                    0.5),
        std::invalid_argument); // NaN vol
    REQUIRE_THROWS_AS(
        heath_jarrow_morton_process(
            vector({0.04, 0.042, 0.041}), vector({0.2, 0.3, 0.1}), vector({0.0, 1.0}), 42, 0.5),
        std::invalid_argument); // zero spacing
    REQUIRE_THROWS_AS(
        heath_jarrow_morton_process(
            vector({0.04, 0.042, 0.041}), vector({0.2, 0.3, 0.1}), vector({0.5, -1.0}), 42, 0.5),
        std::invalid_argument); // negative spacing
    REQUIRE_THROWS_AS(
        heath_jarrow_morton_process(vector({0.04, 0.042, 0.041}),
                                    vector({0.2, 0.3, 0.1}),
                                    vector({0.5, std::numeric_limits<double>::quiet_NaN()}),
                                    42,
                                    0.5),
        std::invalid_argument); // NaN spacing
    REQUIRE_THROWS_AS(
        heath_jarrow_morton_process(
            vector({0.04, 0.042, 0.041}), vector({0.2, 0.3, 0.1}), vector({0.5, 1.0}), 42, 0.0),
        std::invalid_argument); // zero dt
    REQUIRE_THROWS_AS(
        heath_jarrow_morton_process(
            vector({0.04, 0.042, 0.041}), vector({0.2, 0.3, 0.1}), vector({0.5, 1.0}), 42, -1.0),
        std::invalid_argument); // negative dt

    // The rejections carry the process name, not a bare message.
    {
        bool caught = false;
        try {
            heath_jarrow_morton_process p(
                vector({0.04, 0.042, 0.041}), vector({0.2, 0.3}), vector({0.5, 1.0}), 42, 0.5);
        } catch (const std::invalid_argument& e) {
            caught = true;
            REQUIRE(std::string(e.what()).find("heath_jarrow_morton_process:") !=
                    std::string::npos);
        }
        REQUIRE(caught);
    }
    {
        bool caught = false;
        try {
            heath_jarrow_morton_process p(vector({0.04, 0.042, 0.041}),
                                          vector({0.2, 0.3, 0.1}),
                                          vector({0.5, -1.0}),
                                          42,
                                          0.5);
        } catch (const std::invalid_argument& e) {
            caught = true;
            REQUIRE(std::string(e.what()).find("heath_jarrow_morton_process:") !=
                    std::string::npos);
        }
        REQUIRE(caught);
    }
}

TEST_CASE("hjm drift rejects invalid inputs", "[heath_jarrow_morton_process]") {
    REQUIRE_THROWS_AS(hjm_no_arbitrage_drift(vector({}), vector({0.5})),
                      std::invalid_argument); // empty volatilities
    REQUIRE_THROWS_AS(hjm_no_arbitrage_drift(vector({0.2, 0.3}), vector({0.5, 0.7})),
                      std::invalid_argument); // spacing count mismatch
    REQUIRE_THROWS_AS(hjm_no_arbitrage_drift(vector({0.2, -0.3}), vector({0.5})),
                      std::invalid_argument); // negative vol
    REQUIRE_THROWS_AS(hjm_no_arbitrage_drift(vector({0.2, 0.3}), vector({0.0})),
                      std::invalid_argument); // zero spacing

    bool caught = false;
    try {
        hjm_no_arbitrage_drift(vector({0.2, -0.3}), vector({0.5}));
    } catch (const std::invalid_argument& e) {
        caught = true;
        REQUIRE(std::string(e.what()).find("hjm_no_arbitrage_drift:") != std::string::npos);
    }
    REQUIRE(caught);
}
