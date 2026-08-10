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
#include "ores.analytics.quant/service/processes/affine_term_structure_process.hpp"
#include "ores.analytics.quant/service/processes/quadratic_gaussian_process.hpp"
#include <Eigen/Dense>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <initializer_list>
#include <limits>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>

using ores::analytics::quant::service::affine_term_structure_process;
using ores::analytics::quant::service::quadratic_gaussian_params;
using ores::analytics::quant::service::quadratic_gaussian_process;

namespace {

Eigen::VectorXd vector(std::initializer_list<double> values) {
    Eigen::VectorXd result(values.size());
    std::size_t i = 0;
    for (const double v : values)
        result[i++] = v;
    return result;
}

Eigen::MatrixXd matrix(std::initializer_list<std::initializer_list<double>> rows) {
    Eigen::MatrixXd result(rows.size(), rows.begin()->size());
    std::size_t i = 0;
    for (const auto& row : rows) {
        std::size_t j = 0;
        for (const double v : row)
            result(i, j++) = v;
        ++i;
    }
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

} // namespace

TEST_CASE("quadratic gaussian with zero gamma reduces to affine", "[quadratic_gaussian_process]") {
    // The model's design anchor: at gamma == 0 the quadratic term
    // vanishes, the C recursion stays at zero and the B and A
    // recursions reduce to the affine ones exactly -- the same seed
    // must produce the same path and the same discounts.
    const Eigen::VectorXd kappas = vector({0.4, 0.2});
    const Eigen::MatrixXd sigma = matrix({{0.09, 0.03}, {0.03, 0.04}});
    const Eigen::VectorXd theta = vector({0.05, 0.03});
    const Eigen::VectorXd deltas = vector({1.0, 0.5});
    const Eigen::VectorXd initial_factors = vector({0.04, 0.02});
    const Eigen::MatrixXd zero_gamma = matrix({{0.0, 0.0}, {0.0, 0.0}});
    for (const double dt : {1.0, 0.25}) {
        affine_term_structure_process affine(
            kappas, sigma, theta, 0.01, deltas, initial_factors, 42, dt);
        quadratic_gaussian_process qg(
            kappas, sigma, theta, 0.01, deltas, zero_gamma, initial_factors, 42, dt);

        const std::size_t ticks = 8;
        for (std::size_t t = 0; t < ticks; ++t) {
            REQUIRE(qg.next() == Catch::Approx(affine.next()).epsilon(1e-12));
            REQUIRE(qg.discount_factor(2) ==
                    Catch::Approx(affine.discount_factor(2)).epsilon(1e-12));
            REQUIRE(qg.discount_factor(ticks - t) ==
                    Catch::Approx(affine.discount_factor(ticks - t)).epsilon(1e-12));
        }
    }
}

TEST_CASE("quadratic gaussian zero-sigma prices the deterministic path",
          "[quadratic_gaussian_process]") {
    // With sigma == 0 the factors move deterministically:
    // X_{i+1} = theta + exp(-kappas*dt) * (X_i - theta), and the bond
    // price is the product of the per-tick discounts exp(-r_i * dt)
    // along that path, computed here directly from the closed form of
    // the rate.
    const Eigen::VectorXd kappas = vector({0.3});
    const Eigen::MatrixXd zero_sigma = matrix({{0.0}});
    const Eigen::VectorXd theta = vector({0.03});
    const double delta_0 = 0.01;
    const Eigen::VectorXd deltas = vector({0.5});
    const Eigen::MatrixXd gamma = matrix({{1.5}});
    const Eigen::VectorXd initial_factors = vector({0.02});
    const double dt = 0.25;

    // The deterministic rate path, computed directly from the closed
    // form of the rate at each state. From state X_t the recursion
    // prices the next n ticks (discounting the current rate first), so
    // the bond price is the product of the per-tick discounts of the
    // rates at states t .. t + n - 1.
    const std::size_t ticks = 5;
    std::vector<double> rates(ticks + 2);
    double x = initial_factors[0];
    const double lambda = std::exp(-kappas[0] * dt);
    for (std::size_t t = 0; t < rates.size(); ++t) {
        rates[t] = delta_0 + deltas[0] * x + gamma(0, 0) * x * x;
        x = theta[0] + lambda * (x - theta[0]);
    }

    quadratic_gaussian_process qg(
        kappas, zero_sigma, theta, delta_0, deltas, gamma, initial_factors, 42, dt);
    for (std::size_t t = 0; t < ticks; ++t) {
        for (std::size_t n = 1; n <= 3; ++n) {
            double direct = 1.0;
            for (std::size_t j = t; j < t + n; ++j)
                direct *= std::exp(-rates[j] * dt);
            REQUIRE(qg.discount_factor(n) == Catch::Approx(direct).epsilon(1e-12));
        }
        qg.next();
    }
}

TEST_CASE("quadratic gaussian bond price matches the brute-force expectation",
          "[quadratic_gaussian_process]") {
    // The recursion's authoritative check: the bond price is the
    // expectation of the pathwise discount over the exact per-tick
    // Gaussian transitions, estimated here by simulation. The recursion
    // is exact for the same discrete model, so the residual against the
    // simulation mean is pure sampling noise, bounded by the margin
    // below.
    const std::size_t paths = 300000;
    const Eigen::VectorXd kappas = vector({0.4});
    const Eigen::MatrixXd sigma = matrix({{0.0625}}); // instantaneous variance sigma^2
    const Eigen::VectorXd theta = vector({0.03});
    const double delta_0 = 0.005;
    const Eigen::VectorXd deltas = vector({1.0});
    const Eigen::MatrixXd gamma = matrix({{2.0}});
    const Eigen::VectorXd initial_factors = vector({0.02});
    const double dt = 0.25;
    const std::size_t ticks = 5;

    quadratic_gaussian_process p(
        kappas, sigma, theta, delta_0, deltas, gamma, initial_factors, 42, dt);

    // The exact one-step transition, sampled directly: x' = theta +
    // exp(-kappas*dt) * (x - theta) + sqrt(V) * z with the exact
    // per-tick variance V = sigma * (1 - exp(-2*kappas*dt)) / (2*kappas).
    const double lambda = std::exp(-kappas[0] * dt);
    const double variance =
        sigma(0, 0) * (1.0 - std::exp(-2.0 * kappas[0] * dt)) / (2.0 * kappas[0]);
    const double sqrt_variance = std::sqrt(variance);

    std::vector<running_mean_variance> discounts(ticks);
    for (std::size_t i = 0; i < paths; ++i) {
        double x = initial_factors[0];
        double pathwise = 1.0;
        std::mt19937 rng(static_cast<std::uint32_t>(42 + i));
        std::normal_distribution<double> normal;
        for (std::size_t t = 0; t < ticks; ++t) {
            const double rate = delta_0 + deltas[0] * x + gamma(0, 0) * x * x;
            pathwise *= std::exp(-rate * dt);
            discounts[t].add(pathwise);
            x = theta[0] + lambda * (x - theta[0]) + sqrt_variance * normal(rng);
        }
    }

    for (std::size_t t = 0; t < ticks; ++t)
        REQUIRE(p.discount_factor(t + 1) == Catch::Approx(discounts[t].mean).margin(1e-3));
}

TEST_CASE("quadratic gaussian first-tick moments match the closed forms",
          "[quadratic_gaussian_process]") {
    // One-tick statistics of the quadratic rate against the closed
    // forms: with X' = theta + exp(-kappas*dt) * (X_0 - theta) + sqrt(V) z,
    //     E[r']  = delta_0 + deltas * mu + gamma * (mu^2 + V)
    //     Var(r') = deltas^2 V + 4 deltas gamma mu V + 2 gamma^2 V^2
    // with mu the one-tick factor mean and V the exact per-tick
    // variance, for kappas = 0.4, sigma = {{0.0625}}, theta = 0.03,
    // X_0 = 0.02, delta_0 = 0.005, deltas = 1, gamma = 2, dt = 0.25.
    const std::size_t paths = 500000;
    const Eigen::VectorXd kappas = vector({0.4});
    const Eigen::MatrixXd sigma = matrix({{0.0625}});
    const Eigen::VectorXd theta = vector({0.03});
    const double delta_0 = 0.005;
    const Eigen::VectorXd deltas = vector({1.0});
    const Eigen::MatrixXd gamma = matrix({{2.0}});
    const Eigen::VectorXd initial_factors = vector({0.02});
    const double dt = 0.25;

    running_mean_variance rate_moments;
    for (std::size_t i = 0; i < paths; ++i) {
        quadratic_gaussian_process p(kappas,
                                     sigma,
                                     theta,
                                     delta_0,
                                     deltas,
                                     gamma,
                                     initial_factors,
                                     static_cast<std::uint32_t>(42 + i),
                                     dt);
        rate_moments.add(p.next());
    }

    const double mu = theta[0] + std::exp(-kappas[0] * dt) * (initial_factors[0] - theta[0]);
    const double v = sigma(0, 0) * (1.0 - std::exp(-2.0 * kappas[0] * dt)) / (2.0 * kappas[0]);
    const double g = gamma(0, 0);
    const double expected_mean = delta_0 + deltas[0] * mu + g * (mu * mu + v);
    const double expected_variance =
        deltas[0] * deltas[0] * v + 4.0 * deltas[0] * g * mu * v + 2.0 * g * g * v * v;

    REQUIRE(rate_moments.mean == Catch::Approx(expected_mean).margin(2e-3));
    REQUIRE(rate_moments.sample_variance() == Catch::Approx(expected_variance).epsilon(1e-2));
}

TEST_CASE("quadratic gaussian rate stays positive under positive-semidefinite gamma",
          "[quadratic_gaussian_process]") {
    // With delta_0 > 0, zero linear sensitivity and a positive-semidefinite
    // gamma the rate is r = delta_0 + X' * gamma * X >= delta_0 > 0
    // pathwise: the quadratic term dominates at large |X|, which is the
    // model's reason for being.
    quadratic_gaussian_process p(vector({0.5, 0.3}),
                                 matrix({{0.04, 0.01}, {0.01, 0.03}}),
                                 vector({0.02, 0.01}),
                                 0.01,
                                 vector({0.0, 0.0}),
                                 matrix({{1.5, 0.0}, {0.0, 0.8}}),
                                 vector({0.0, 0.0}),
                                 42,
                                 0.25);
    for (std::size_t t = 0; t < 1000; ++t)
        REQUIRE(p.next() > 0.0);
}

TEST_CASE("quadratic gaussian discounts are monotone", "[quadratic_gaussian_process]") {
    // Positive rates (delta_0 > 0, zero linear term, PSD gamma) imply
    // strictly decreasing bond prices in the horizon.
    quadratic_gaussian_process p(vector({0.5, 0.3}),
                                 matrix({{0.04, 0.01}, {0.01, 0.03}}),
                                 vector({0.02, 0.01}),
                                 0.01,
                                 vector({0.0, 0.0}),
                                 matrix({{1.5, 0.0}, {0.0, 0.8}}),
                                 vector({0.0, 0.0}),
                                 42,
                                 0.25);
    double previous = 1.0;
    for (std::size_t n = 1; n <= 6; ++n) {
        const double d = p.discount_factor(n);
        REQUIRE(std::isfinite(d));
        REQUIRE(d > 0.0);
        REQUIRE(d < previous);
        previous = d;
    }
}

TEST_CASE("quadratic gaussian params struct equals scalar construction",
          "[quadratic_gaussian_process]") {
    quadratic_gaussian_params params;
    params.kappas = vector({0.4, 0.2});
    params.sigma = matrix({{0.09, 0.03}, {0.03, 0.04}});
    params.theta = vector({0.05, 0.03});
    params.delta_0 = 0.01;
    params.deltas = vector({1.0, 0.5});
    params.gamma = matrix({{0.5, 0.2}, {0.2, 0.3}});
    params.initial_factors = vector({0.04, 0.02});

    quadratic_gaussian_process from_params(params, 99, 0.25);
    quadratic_gaussian_process from_scalars(vector({0.4, 0.2}),
                                            matrix({{0.09, 0.03}, {0.03, 0.04}}),
                                            vector({0.05, 0.03}),
                                            0.01,
                                            vector({1.0, 0.5}),
                                            matrix({{0.5, 0.2}, {0.2, 0.3}}),
                                            vector({0.04, 0.02}),
                                            99,
                                            0.25);
    for (std::size_t t = 0; t < 6; ++t) {
        REQUIRE(from_params.next() == from_scalars.next());
        REQUIRE(from_params.discount_factor(3) == from_scalars.discount_factor(3));
    }
}

TEST_CASE("quadratic gaussian seeds determine the path", "[quadratic_gaussian_process]") {
    const auto path = [](std::uint32_t seed) {
        quadratic_gaussian_process p(vector({0.4, 0.2}),
                                     matrix({{0.09, 0.03}, {0.03, 0.04}}),
                                     vector({0.05, 0.03}),
                                     0.0,
                                     vector({1.0, 1.0}),
                                     matrix({{0.5, 0.2}, {0.2, 0.3}}),
                                     vector({0.04, 0.02}),
                                     seed,
                                     1.0);
        std::vector<double> rates;
        for (std::size_t t = 0; t < 5; ++t)
            rates.push_back(p.next());
        return rates;
    };
    REQUIRE(path(42) == path(42));
    REQUIRE(path(42) != path(43));
}

TEST_CASE("quadratic gaussian rejects invalid parameters", "[quadratic_gaussian_process]") {
    const Eigen::VectorXd two_kappas = vector({0.4, 0.2});
    const Eigen::MatrixXd valid_sigma = matrix({{0.09, 0.03}, {0.03, 0.04}});
    const Eigen::VectorXd two_theta = vector({0.05, 0.03});
    const Eigen::VectorXd two_deltas = vector({1.0, 1.0});
    const Eigen::MatrixXd valid_gamma = matrix({{0.5, 0.2}, {0.2, 0.3}});
    const Eigen::VectorXd two_factors = vector({0.04, 0.02});

    // Valid constructions as the baseline for the single-invalid-input
    // cases below, including the positive-semidefinite boundaries:
    // gamma == 0 and the rank-one gamma matrix.
    REQUIRE_NOTHROW(quadratic_gaussian_process(
        two_kappas, valid_sigma, two_theta, 0.0, two_deltas, valid_gamma, two_factors, 42, 1.0));
    REQUIRE_NOTHROW(quadratic_gaussian_process(two_kappas,
                                               valid_sigma,
                                               two_theta,
                                               0.0,
                                               two_deltas,
                                               matrix({{0.0, 0.0}, {0.0, 0.0}}),
                                               two_factors,
                                               42,
                                               1.0));
    REQUIRE_NOTHROW(quadratic_gaussian_process(two_kappas,
                                               valid_sigma,
                                               two_theta,
                                               0.0,
                                               two_deltas,
                                               matrix({{1.0, 1.0}, {1.0, 1.0}}),
                                               two_factors,
                                               42,
                                               1.0));

    REQUIRE_THROWS_AS(quadratic_gaussian_process(Eigen::VectorXd(),
                                                 valid_sigma,
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 valid_gamma,
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // empty kappas
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 valid_sigma,
                                                 vector({0.05}),
                                                 0.0,
                                                 two_deltas,
                                                 valid_gamma,
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // theta size mismatch
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 valid_sigma,
                                                 two_theta,
                                                 0.0,
                                                 vector({1.0}),
                                                 valid_gamma,
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // deltas size mismatch
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 valid_sigma,
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 valid_gamma,
                                                 vector({0.04}),
                                                 42,
                                                 1.0),
                      std::invalid_argument); // initial_factors size mismatch
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 matrix({{0.09, 0.03, 0.01}, {0.03, 0.04, 0.01}}),
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 valid_gamma,
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // sigma not square
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 matrix({{0.09}}),
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 valid_gamma,
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // sigma wrong size
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 valid_sigma,
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 matrix({{0.5, 0.2, 0.1}, {0.2, 0.3, 0.1}}),
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // gamma not square
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 valid_sigma,
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 matrix({{0.5}}),
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // gamma wrong size
    REQUIRE_THROWS_AS(quadratic_gaussian_process(vector({-0.1, 0.2}),
                                                 valid_sigma,
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 valid_gamma,
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // negative kappa
    REQUIRE_THROWS_AS(
        quadratic_gaussian_process(vector({std::numeric_limits<double>::quiet_NaN(), 0.2}),
                                   valid_sigma,
                                   two_theta,
                                   0.0,
                                   two_deltas,
                                   valid_gamma,
                                   two_factors,
                                   42,
                                   1.0),
        std::invalid_argument); // NaN kappa
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 matrix({{0.09, 0.02}, {0.03, 0.04}}),
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 valid_gamma,
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // asymmetric sigma
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 matrix({{1.0, 2.0}, {2.0, 1.0}}),
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 valid_gamma,
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // indefinite sigma
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 valid_sigma,
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 matrix({{0.5, 0.1}, {0.2, 0.3}}),
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // asymmetric gamma
    REQUIRE_THROWS_AS(quadratic_gaussian_process(two_kappas,
                                                 valid_sigma,
                                                 two_theta,
                                                 0.0,
                                                 two_deltas,
                                                 matrix({{1.0, 1.5}, {1.5, 1.0}}),
                                                 two_factors,
                                                 42,
                                                 1.0),
                      std::invalid_argument); // indefinite gamma
    REQUIRE_THROWS_AS(quadratic_gaussian_process(
                          two_kappas,
                          valid_sigma,
                          two_theta,
                          0.0,
                          two_deltas,
                          matrix({{std::numeric_limits<double>::quiet_NaN(), 0.2}, {0.2, 0.3}}),
                          two_factors,
                          42,
                          1.0),
                      std::invalid_argument); // NaN gamma
    REQUIRE_THROWS_AS(
        quadratic_gaussian_process(
            two_kappas, valid_sigma, two_theta, 0.0, two_deltas, valid_gamma, two_factors, 42, 0.0),
        std::invalid_argument); // dt not strictly positive
}
