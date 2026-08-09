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
#include "ores.analytics.quant/service/processes/affine_term_structure_params.hpp"
#include "ores.analytics.quant/service/processes/affine_term_structure_process.hpp"
#include "ores.analytics.quant/service/processes/hull_white_process.hpp"
#include "ores.analytics.quant/service/processes/two_factor_gaussian_process.hpp"
#include "ores.analytics.quant/service/processes/vasicek_process.hpp"
#include <Eigen/Dense>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <initializer_list>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

using ores::analytics::quant::service::affine_term_structure_params;
using ores::analytics::quant::service::affine_term_structure_process;
using ores::analytics::quant::service::hull_white_process;
using ores::analytics::quant::service::two_factor_gaussian_process;
using ores::analytics::quant::service::vasicek_process;

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

TEST_CASE("affine one-factor reduces to hull-white", "[affine_term_structure_process]") {
    const double kappa = 0.3;
    const double theta = std::log(0.04);
    const double sigma = 0.35;
    const double initial_rate = 0.05;
    for (const double dt : {1.0, 0.25}) {
        // sigma is the instantaneous covariance matrix: its diagonal is
        // the variance sigma^2, not the volatility.
        affine_term_structure_process affine(
            vector({kappa}), matrix({{sigma * sigma}}), vector({theta}), 0.0, vector({1.0}),
            vector({initial_rate}), 42, dt);
        hull_white_process hull_white(kappa, {theta}, sigma, initial_rate, 42, dt);

        const std::size_t ticks = 8;
        for (std::size_t t = 0; t < ticks; ++t) {
            REQUIRE(affine.next() == Catch::Approx(hull_white.next()).epsilon(1e-12));
            REQUIRE(affine.discount_factor(2) ==
                    Catch::Approx(hull_white.discount_factor(2)).epsilon(1e-12));
            REQUIRE(affine.discount_factor(ticks - t) ==
                    Catch::Approx(hull_white.discount_factor(ticks - t)).epsilon(1e-12));
        }
    }
}

TEST_CASE("affine one-factor reduces to vasicek", "[affine_term_structure_process]") {
    const double kappa = 0.5;
    const double theta = 0.04;
    const double sigma = 0.2;
    const double initial_rate = 0.03;
    affine_term_structure_process affine(
        vector({kappa}), matrix({{sigma * sigma}}), vector({theta}), 0.0, vector({1.0}),
        vector({initial_rate}), 7, 0.5);
    vasicek_process vasicek(kappa, theta, sigma, initial_rate, 7, 0.5);
    for (std::size_t t = 0; t < 6; ++t) {
        REQUIRE(affine.next() == Catch::Approx(vasicek.next()).epsilon(1e-12));
        REQUIRE(affine.discount_factor(3) ==
                Catch::Approx(vasicek.discount_factor(3)).epsilon(1e-12));
    }
}

TEST_CASE("affine two-factor reduces to g2", "[affine_term_structure_process]") {
    // Equal kappas: the case where G2's hand-built Cholesky coincides
    // with the general LLT factor, so the two processes must agree.
    const double kappa = 0.3;
    const double sigma_x = 0.3;
    const double sigma_y = 0.2;
    const double rho = 0.5;
    const double initial_rate = 0.05;
    affine_term_structure_process affine(
        vector({kappa, kappa}), matrix({{sigma_x * sigma_x, rho * sigma_x * sigma_y},
                                        {rho * sigma_x * sigma_y, sigma_y * sigma_y}}),
        vector({0.0, 0.0}), 0.0, vector({1.0, 1.0}), vector({initial_rate, 0.0}), 42, 0.25);
    two_factor_gaussian_process g2(kappa, kappa, sigma_x, sigma_y, rho, initial_rate, 42, 0.25);
    const std::size_t ticks = 6;
    for (std::size_t t = 0; t < ticks; ++t) {
        REQUIRE(affine.next() == Catch::Approx(g2.next()).epsilon(1e-12));
        REQUIRE(affine.discount_factor(ticks - t) ==
                Catch::Approx(g2.discount_factor(ticks - t)).epsilon(1e-12));
    }
}

TEST_CASE("affine one-tick discount is exp(-r dt)", "[affine_term_structure_process]") {
    affine_term_structure_process p(
        vector({0.4, 0.2}), matrix({{0.09, 0.03}, {0.03, 0.04}}), vector({0.05, 0.03}),
        0.01, vector({1.0, 0.5}), vector({0.04, 0.02}), 42, 0.5);
    REQUIRE(p.discount_factor(1) ==
            Catch::Approx(std::exp(-p.current() * 0.5)).epsilon(1e-15));
}

TEST_CASE("affine zero-sigma deterministic curve across dt", "[affine_term_structure_process]") {
    // sigma = 0 freezes the diffusion: the recursion must reproduce the
    // hand-rolled deterministic rate path and its discount product at
    // every granularity.
    const double kappa = 0.5;
    const double theta = 0.06;
    const double delta_0 = 0.02;
    const double x0 = 0.03;
    for (const double dt : {1.0, 0.5, 0.25, 0.1}) {
        affine_term_structure_process p(
            vector({kappa}), matrix({{0.0}}), vector({theta}), delta_0, vector({1.0}),
            vector({x0}), 42, dt);
        const std::size_t ticks = 6;
        const auto step = [&](double x) { return theta + std::exp(-kappa * dt) * (x - theta); };

        double expected = 1.0;
        double x = x0;
        for (std::size_t i = 0; i < ticks; ++i) {
            expected *= std::exp(-(delta_0 + x) * dt);
            x = step(x);
        }
        REQUIRE(p.discount_factor(ticks) == Catch::Approx(expected).epsilon(1e-12));

        x = x0;
        for (std::size_t i = 0; i < ticks; ++i) {
            x = step(x);
            REQUIRE(p.next() == Catch::Approx(delta_0 + x).epsilon(1e-12));
        }
    }
}

TEST_CASE("affine zero deltas price a constant rate", "[affine_term_structure_process]") {
    // deltas = 0 makes r = delta_0 regardless of the moving factors, and
    // the discount factor telescopes to exp(-delta_0 * horizon).
    affine_term_structure_process p(
        vector({0.5}), matrix({{0.2}}), vector({0.03}), 0.04, vector({0.0}),
        vector({0.02}), 42, 0.25);
    REQUIRE(p.current() == Catch::Approx(0.04).epsilon(1e-15));
    const std::size_t ticks = 5;
    REQUIRE(p.discount_factor(ticks) ==
            Catch::Approx(std::exp(-0.04 * ticks * 0.25)).epsilon(1e-15));
    for (std::size_t t = 0; t < 3; ++t) {
        REQUIRE(p.next() == Catch::Approx(0.04).epsilon(1e-15));
        REQUIRE(p.discount_factor(2) == Catch::Approx(std::exp(-0.04 * 0.5)).epsilon(1e-15));
    }
}

TEST_CASE("affine correlated moments match the exact transition",
          "[affine_term_structure_process]") {
    // One-tick statistics of the two-factor transition with full
    // covariance, against the closed forms:
    //     E[X_i'] = theta_i + exp(-kappas_i*dt) * (X_0,i - theta_i)
    //     Cov_dt[i][j] = sigma[i][j] * (1-exp(-(kappas_i+kappas_j)*dt))
    //                    / (kappas_i + kappas_j)
    // with kappas = {0.4, 0.2}, sigma = {{0.09, 0.03}, {0.03, 0.04}},
    // theta = {0.05, 0.03}, X_0 = {0.04, 0.02}, dt = 1.
    const std::size_t paths = 500000;
    running_moments moments(2);
    running_mean_variance rate_moments;

    for (std::size_t i = 0; i < paths; ++i) {
        affine_term_structure_process p(
            vector({0.4, 0.2}), matrix({{0.09, 0.03}, {0.03, 0.04}}),
            vector({0.05, 0.03}), 0.0, vector({1.0, 1.0}), vector({0.04, 0.02}),
            static_cast<std::uint32_t>(42 + i), 1.0);
        const double rate = p.next();
        moments.add(p.factors());
        rate_moments.add(rate);
    }

    const Eigen::VectorXd& mean = moments.mean;
    REQUIRE(mean[0] == Catch::Approx(0.04329679953964361).margin(2e-3));
    REQUIRE(mean[1] == Catch::Approx(0.02181269246922018).margin(2e-3));

    const Eigen::MatrixXd covariance = moments.sample_covariance();
    REQUIRE(covariance(0, 0) == Catch::Approx(0.06195049153681257).epsilon(1e-2));
    REQUIRE(covariance(1, 1) == Catch::Approx(0.03296799539640707).epsilon(1e-2));
    REQUIRE(covariance(0, 1) == Catch::Approx(0.02255941819529868).margin(1e-3));
    REQUIRE(covariance(1, 0) == Catch::Approx(covariance(0, 1)).epsilon(1e-12));

    REQUIRE(rate_moments.mean == Catch::Approx(0.06510949200886379).margin(3e-3));
    REQUIRE(rate_moments.sample_variance() ==
            Catch::Approx(0.14003732332381700).epsilon(1e-2));
}

TEST_CASE("affine three-factor sanity", "[affine_term_structure_process]") {
    // sigma = 0 with non-negative factors: rates stay positive and the
    // discounts are strictly decreasing and finite.
    affine_term_structure_process p(
        vector({0.5, 0.3, 0.2}), matrix({{0.0, 0.0, 0.0}, {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0}}),
        vector({0.04, 0.03, 0.02}), 0.0, vector({1.0, 1.0, 1.0}),
        vector({0.03, 0.02, 0.01}), 42, 1.0);
    REQUIRE(p.current() == Catch::Approx(0.06).epsilon(1e-12));
    double previous = 1.0;
    for (std::size_t n = 1; n <= 5; ++n) {
        const double d = p.discount_factor(n);
        REQUIRE(std::isfinite(d));
        REQUIRE(d > 0.0);
        REQUIRE(d < previous);
        previous = d;
    }
}

TEST_CASE("affine params struct equals scalar construction",
          "[affine_term_structure_process]") {
    affine_term_structure_params params;
    params.kappas = vector({0.4, 0.2});
    params.sigma = matrix({{0.09, 0.03}, {0.03, 0.04}});
    params.theta = vector({0.05, 0.03});
    params.delta_0 = 0.01;
    params.deltas = vector({1.0, 0.5});
    params.initial_factors = vector({0.04, 0.02});

    affine_term_structure_process from_params(params, 99, 0.25);
    affine_term_structure_process from_scalars(
        vector({0.4, 0.2}), matrix({{0.09, 0.03}, {0.03, 0.04}}), vector({0.05, 0.03}),
        0.01, vector({1.0, 0.5}), vector({0.04, 0.02}), 99, 0.25);
    for (std::size_t t = 0; t < 6; ++t) {
        REQUIRE(from_params.next() == from_scalars.next());
        REQUIRE(from_params.discount_factor(3) == from_scalars.discount_factor(3));
    }
}

TEST_CASE("affine seeds determine the path", "[affine_term_structure_process]") {
    const auto path = [](std::uint32_t seed) {
        affine_term_structure_process p(
            vector({0.4, 0.2}), matrix({{0.09, 0.03}, {0.03, 0.04}}),
            vector({0.05, 0.03}), 0.0, vector({1.0, 1.0}), vector({0.04, 0.02}), seed, 1.0);
        std::vector<double> rates;
        for (std::size_t t = 0; t < 5; ++t)
            rates.push_back(p.next());
        return rates;
    };
    REQUIRE(path(42) == path(42));
    REQUIRE(path(42) != path(43));
}

TEST_CASE("affine rejects invalid parameters", "[affine_term_structure_process]") {
    const Eigen::VectorXd two_kappas = vector({0.4, 0.2});
    const Eigen::MatrixXd valid_sigma = matrix({{0.09, 0.03}, {0.03, 0.04}});
    const Eigen::VectorXd two_theta = vector({0.05, 0.03});
    const Eigen::VectorXd two_deltas = vector({1.0, 1.0});
    const Eigen::VectorXd two_factors = vector({0.04, 0.02});

    // A valid construction, as the baseline for the single-invalid-input
    // cases below.
    REQUIRE_NOTHROW(affine_term_structure_process(two_kappas, valid_sigma, two_theta, 0.0,
                                                  two_deltas, two_factors, 42, 1.0));

    REQUIRE_THROWS_AS(affine_term_structure_process(Eigen::VectorXd(), valid_sigma, two_theta,
                                                    0.0, two_deltas, two_factors, 42, 1.0),
                      std::invalid_argument); // empty kappas
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, valid_sigma, vector({0.05}),
                                                    0.0, two_deltas, two_factors, 42, 1.0),
                      std::invalid_argument); // theta size mismatch
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, valid_sigma, two_theta, 0.0,
                                                    vector({1.0}), two_factors, 42, 1.0),
                      std::invalid_argument); // deltas size mismatch
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, valid_sigma, two_theta, 0.0,
                                                    two_deltas, vector({0.04}), 42, 1.0),
                      std::invalid_argument); // initial_factors size mismatch
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, matrix({{0.09, 0.03, 0.01},
                                                                        {0.03, 0.04, 0.01}}),
                                                    two_theta, 0.0, two_deltas, two_factors, 42,
                                                    1.0),
                      std::invalid_argument); // sigma not square
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, matrix({{0.09}}), two_theta, 0.0,
                                                    two_deltas, two_factors, 42, 1.0),
                      std::invalid_argument); // sigma wrong size
    REQUIRE_THROWS_AS(affine_term_structure_process(vector({-0.1, 0.2}), valid_sigma, two_theta,
                                                    0.0, two_deltas, two_factors, 42, 1.0),
                      std::invalid_argument); // negative kappa
    REQUIRE_THROWS_AS(affine_term_structure_process(
                          vector({std::numeric_limits<double>::quiet_NaN(), 0.2}), valid_sigma,
                          two_theta, 0.0, two_deltas, two_factors, 42, 1.0),
                      std::invalid_argument); // NaN kappa
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, matrix({{0.09, 0.02},
                                                                        {0.03, 0.04}}),
                                                    two_theta, 0.0, two_deltas, two_factors, 42,
                                                    1.0),
                      std::invalid_argument); // asymmetric sigma
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, matrix({{1.0, 2.0}, {2.0, 1.0}}),
                                                    two_theta, 0.0, two_deltas, two_factors, 42,
                                                    1.0),
                      std::invalid_argument); // indefinite sigma
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, valid_sigma, two_theta, 0.0,
                                                    two_deltas, two_factors, 42, 0.0),
                      std::invalid_argument); // zero dt
    REQUIRE_THROWS_AS(affine_term_structure_process(two_kappas, valid_sigma, two_theta, 0.0,
                                                    two_deltas, two_factors, 42, -1.0),
                      std::invalid_argument); // negative dt

    // The rejections carry the process name, not a bare message.
    {
        bool caught = false;
        try {
            affine_term_structure_process p(two_kappas, valid_sigma, vector({0.05}), 0.0,
                                            two_deltas, two_factors, 42, 1.0);
        } catch (const std::invalid_argument& e) {
            caught = true;
            REQUIRE(std::string(e.what()).find("affine_term_structure_process:") !=
                    std::string::npos);
        }
        REQUIRE(caught);
    }
    {
        bool caught = false;
        try {
            affine_term_structure_process p(two_kappas, matrix({{1.0, 2.0}, {2.0, 1.0}}),
                                            two_theta, 0.0, two_deltas, two_factors, 42, 1.0);
        } catch (const std::invalid_argument& e) {
            caught = true;
            REQUIRE(std::string(e.what()).find("affine_term_structure_process:") !=
                    std::string::npos);
        }
        REQUIRE(caught);
    }
}
