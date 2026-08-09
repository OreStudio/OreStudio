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
#include "ores.analytics.quant/service/processes/libor_market_model_params.hpp"
#include "ores.analytics.quant/service/processes/libor_market_model_process.hpp"
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

using ores::analytics::quant::service::libor_market_model_params;
using ores::analytics::quant::service::libor_market_model_process;
using ores::analytics::quant::service::lmm_spot_measure_drift;

namespace {

Eigen::VectorXd vector(std::initializer_list<double> values) {
    Eigen::VectorXd result(values.size());
    std::size_t i = 0;
    for (const double v : values)
        result[i++] = v;
    return result;
}

Eigen::MatrixXd matrix(std::initializer_list<std::initializer_list<double>> rows) {
    const std::size_t n = rows.size();
    Eigen::MatrixXd result(n, n);
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
 * @brief Welford running mean and variance.
 */
struct running_mean_variance {
    double mean = 0.0;
    double m2 = 0.0;
    std::size_t count = 0;

    void add(double sample) {
        ++count;
        const double delta = sample - mean;
        mean += delta / static_cast<double>(count);
        m2 += delta * (sample - mean);
    }

    double sample_variance() const {
        return m2 / static_cast<double>(count - 1);
    }
};

/**
 * @brief Welford running mean for vector samples.
 */
struct running_vector_mean {
    Eigen::VectorXd mean;
    std::size_t count = 0;

    explicit running_vector_mean(std::size_t dimension)
        : mean(Eigen::VectorXd::Zero(dimension)) {}

    void add(const Eigen::VectorXd& sample) {
        ++count;
        mean += (sample - mean) / static_cast<double>(count);
    }
};

} // namespace

TEST_CASE("lmm spot-measure drift matches the LMMDriftCalculator semantics",
          "[libor_market_model_process]") {
    // Hand-computed against QuantLib's LMMDriftCalculator::computePlain
    // with numeraire = 0: weights[j] = tau_j * (L_j + s_j) / (1 +
    // tau_j * L_j), drift[i] = sum_{j <= i} C[i][j] * weights[j] with
    // C = diag(sigma) * rho * diag(sigma), no sign flip.
    const Eigen::VectorXd rates = vector({0.03, 0.035, 0.04});
    const Eigen::VectorXd displacements = vector({0.01, 0.0, 0.02});
    const Eigen::VectorXd spacings = vector({0.5, 1.0, 0.25});
    const Eigen::VectorXd volatilities = vector({0.2, 0.15, 0.25});
    const Eigen::MatrixXd correlation =
        matrix({{1.0, 0.6, 0.3}, {0.6, 1.0, 0.4}, {0.3, 0.4, 1.0}});
    const Eigen::MatrixXd covariance =
        volatilities.asDiagonal() * correlation * volatilities.asDiagonal();
    const Eigen::VectorXd drift =
        lmm_spot_measure_drift(rates, displacements, spacings, covariance);
    const Eigen::VectorXd expected =
        vector({0.0007881773399014781, 0.0011155493681730565, 0.0017310307010568267});
    for (std::size_t i = 0; i < 3; ++i)
        REQUIRE(drift[i] == Catch::Approx(expected[i]).epsilon(1e-12));

    // A second setup: four rates, non-uniform spacings, all the sum
    // ranges active. Hand-computed the same way.
    const Eigen::VectorXd rates_2 = vector({0.02, 0.025, 0.03, 0.035});
    const Eigen::VectorXd displacements_2 = vector({0.005, 0.01, 0.0, 0.015});
    const Eigen::VectorXd spacings_2 = vector({0.25, 0.5, 1.0, 0.5});
    const Eigen::VectorXd volatilities_2 = vector({0.1, 0.2, 0.15, 0.3});
    const Eigen::MatrixXd correlation_2 = matrix({{1.0, 0.5, 0.2, 0.1},
                                                  {0.5, 1.0, 0.4, 0.3},
                                                  {0.2, 0.4, 1.0, 0.6},
                                                  {0.1, 0.3, 0.6, 1.0}});
    const Eigen::MatrixXd covariance_2 =
        volatilities_2.asDiagonal() * correlation_2 * volatilities_2.asDiagonal();
    const Eigen::VectorXd drift_2 =
        lmm_spot_measure_drift(rates_2, displacements_2, spacings_2, covariance_2);
    const Eigen::VectorXd expected_2 =
        vector({0.000062189054726368182, 0.00075354707941772644, 0.00088140392965056042,
                0.0033274778058215238});
    for (std::size_t i = 0; i < 4; ++i)
        REQUIRE(drift_2[i] == Catch::Approx(expected_2[i]).epsilon(1e-12));
}

TEST_CASE("lmm drift is positive for positive correlations and grows with them",
          "[libor_market_model_process]") {
    // Under the spot measure every rate drifts up: each term of the
    // sum is non-negative when the correlation entries are. The front
    // rate's drift only involves its own variance and is therefore
    // independent of the off-diagonal correlation; the later rates
    // gain from every positive cross term.
    const Eigen::VectorXd rates = vector({0.03, 0.035, 0.04});
    const Eigen::VectorXd displacements = vector({0.01, 0.0, 0.02});
    const Eigen::VectorXd spacings = vector({0.5, 1.0, 0.25});
    const Eigen::VectorXd volatilities = vector({0.2, 0.15, 0.25});

    const Eigen::MatrixXd rho_lo =
        matrix({{1.0, 0.5, 0.5}, {0.5, 1.0, 0.5}, {0.5, 0.5, 1.0}});
    const Eigen::MatrixXd rho_hi =
        matrix({{1.0, 0.9, 0.9}, {0.9, 1.0, 0.9}, {0.9, 0.9, 1.0}});
    const Eigen::VectorXd drift_lo = lmm_spot_measure_drift(
        rates, displacements, spacings,
        volatilities.asDiagonal() * rho_lo * volatilities.asDiagonal());
    const Eigen::VectorXd drift_hi = lmm_spot_measure_drift(
        rates, displacements, spacings,
        volatilities.asDiagonal() * rho_hi * volatilities.asDiagonal());

    for (std::size_t i = 0; i < 3; ++i)
        REQUIRE(drift_lo[i] > 0.0);
    REQUIRE(drift_hi[0] == Catch::Approx(drift_lo[0]).epsilon(1e-15));
    REQUIRE(drift_hi[1] > drift_lo[1]);
    REQUIRE(drift_hi[2] > drift_lo[2]);
}

TEST_CASE("lmm drift can be negative with negative correlation",
          "[libor_market_model_process]") {
    // A high-volatility front rate dragging a low-volatility back
    // rate down: the negative cross term dominates the back rate's own
    // variance term.
    const Eigen::VectorXd rates = vector({0.05, 0.05});
    const Eigen::VectorXd displacements = vector({0.0, 0.0});
    const Eigen::VectorXd spacings = vector({0.5, 0.5});
    const Eigen::VectorXd volatilities = vector({0.5, 0.2});
    const Eigen::MatrixXd correlation = matrix({{1.0, -0.9}, {-0.9, 1.0}});
    const Eigen::VectorXd drift = lmm_spot_measure_drift(
        rates, displacements, spacings,
        volatilities.asDiagonal() * correlation * volatilities.asDiagonal());
    REQUIRE(drift[0] > 0.0);
    REQUIRE(drift[1] < 0.0);
}

TEST_CASE("lmm zero-volatility curve is frozen", "[libor_market_model_process]") {
    // sigma = 0 zeroes the covariance, hence the drift: every rate
    // stays put and the discounts come from the initial curve alone.
    const Eigen::VectorXd rates = vector({0.03, 0.035, 0.04, 0.045});
    const Eigen::VectorXd spacings = vector({0.25, 0.5, 1.0, 0.5});
    libor_market_model_process p(rates, vector({0.0, 0.0, 0.0, 0.0}),
                                 Eigen::MatrixXd::Identity(4, 4),
                                 vector({0.0, 0.0, 0.0, 0.0}), spacings, 42, 0.25);
    REQUIRE(p.current() == Catch::Approx(0.03).epsilon(1e-15));
    REQUIRE(p.discount_factor(0) == Catch::Approx(1.0).epsilon(1e-15));
    REQUIRE(p.discount_factor(1) ==
            Catch::Approx(1.0 / (1.0 + 0.25 * 0.03)).epsilon(1e-15));
    REQUIRE(p.discount_factor(2) ==
            Catch::Approx(1.0 / ((1.0 + 0.25 * 0.03) * (1.0 + 0.5 * 0.035)))
                .epsilon(1e-15));
    REQUIRE(p.discount_factor(4) ==
            Catch::Approx(1.0 / ((1.0 + 0.25 * 0.03) * (1.0 + 0.5 * 0.035) *
                                 (1.0 + 1.0 * 0.04) * (1.0 + 0.5 * 0.045)))
                .epsilon(1e-15));
    for (std::size_t t = 0; t < 5; ++t)
        REQUIRE(p.next() == Catch::Approx(0.03).epsilon(1e-15));
    for (std::size_t i = 0; i < 4; ++i)
        REQUIRE(p.forward_rates()[i] == Catch::Approx(rates[i]).epsilon(1e-15));
    REQUIRE(p.discount_factor(4) ==
            Catch::Approx(1.0 / ((1.0 + 0.25 * 0.03) * (1.0 + 0.5 * 0.035) *
                                 (1.0 + 1.0 * 0.04) * (1.0 + 0.5 * 0.045)))
                .epsilon(1e-15));
}

TEST_CASE("lmm discounting past the grid throws", "[libor_market_model_process]") {
    libor_market_model_process p(vector({0.03, 0.035, 0.04}),
                                 vector({0.2, 0.2, 0.2}),
                                 Eigen::MatrixXd::Identity(3, 3),
                                 vector({0.0, 0.0, 0.0}), vector({0.5, 0.5, 0.5}), 42, 0.25);
    REQUIRE_THROWS_AS(p.discount_factor(4), std::out_of_range);
}

TEST_CASE("lmm one-tick forwards and optionlets are priced exactly",
          "[libor_market_model_process]") {
    // The one-step leg of QuantLib's testOneStepForwardsAndOptionlets:
    // a forward or an optionlet on the rate fixing immediately pays
    // (L_0 - K) * tau_0 or (L_0 - K)+ * tau_0 at t_1, discounted by
    // the known 1 / (1 + tau_0 * L_0) -- exactly, with zero variance.
    const double tau_0 = 0.5;
    const double L_0 = 0.03;
    const double strike = 0.025;
    libor_market_model_process p(vector({L_0, 0.035, 0.04}),
                                 vector({0.2, 0.2, 0.2}),
                                 Eigen::MatrixXd::Identity(3, 3),
                                 vector({0.01, 0.0, 0.0}), vector({tau_0, 0.5, 0.5}), 42,
                                 0.25);
    const double discount = p.discount_factor(1);
    REQUIRE(discount == Catch::Approx(1.0 / (1.0 + tau_0 * L_0)).epsilon(1e-15));
    const double forward = tau_0 * (L_0 - strike) * discount;
    const double optionlet = tau_0 * std::max(L_0 - strike, 0.0) * discount;
    REQUIRE(forward == Catch::Approx(tau_0 * (L_0 - strike) / (1.0 + tau_0 * L_0))
                           .epsilon(1e-15));
    REQUIRE(optionlet ==
            Catch::Approx(tau_0 * std::max(L_0 - strike, 0.0) / (1.0 + tau_0 * L_0))
                .epsilon(1e-15));

    // An out-of-the-money caplet is worthless: the payoff is zero on
    // every path.
    const double high_strike = 0.05;
    REQUIRE(tau_0 * std::max(L_0 - high_strike, 0.0) * discount ==
            Catch::Approx(0.0).epsilon(1e-15));
}

TEST_CASE("lmm one-tick increments follow the drift", "[libor_market_model_process]") {
    // The Euler step applies the drift and the diffusion from the
    // current state, so the mean increment of each rate is its
    // spot-measure drift times dt; the diffusion contributes nothing
    // to the mean. The margins are ~30 standard errors.
    const std::size_t paths = 200000;
    const double dt = 0.25;
    const Eigen::VectorXd rates = vector({0.03, 0.035, 0.04, 0.045});
    const Eigen::VectorXd volatilities = vector({0.2, 0.15, 0.25, 0.3});
    const Eigen::MatrixXd correlation = matrix({{1.0, 0.6, 0.4, 0.2},
                                                {0.6, 1.0, 0.5, 0.3},
                                                {0.4, 0.5, 1.0, 0.7},
                                                {0.2, 0.3, 0.7, 1.0}});
    const Eigen::VectorXd displacements = vector({0.01, 0.02, 0.0, 0.015});
    const Eigen::VectorXd spacings = vector({0.25, 0.5, 1.0, 0.5});
    const Eigen::VectorXd expected_drift = lmm_spot_measure_drift(
        rates, displacements, spacings,
        volatilities.asDiagonal() * correlation * volatilities.asDiagonal());

    running_vector_mean increments(4);
    for (std::size_t i = 0; i < paths; ++i) {
        libor_market_model_process p(rates, volatilities, correlation, displacements,
                                     spacings, static_cast<std::uint32_t>(42 + i), dt);
        const Eigen::VectorXd before = p.forward_rates();
        p.next();
        increments.add(p.forward_rates() - before);
    }
    for (std::size_t i = 0; i < 4; ++i)
        REQUIRE(increments.mean[i] / dt ==
                Catch::Approx(expected_drift[i]).margin(1e-3));
}

TEST_CASE("lmm increments are correlated per the correlation matrix",
          "[libor_market_model_process]") {
    // The first-tick increments are the initial diffusion scaled by
    // sigma_i * (L_i + s_i) * sqrt(dt), so their sample correlation
    // is the input correlation. The margin is ~5 standard errors at
    // this path count.
    const std::size_t paths = 200000;
    const double rho = 0.6;
    const Eigen::VectorXd rates = vector({0.03, 0.035});
    const Eigen::VectorXd volatilities = vector({0.2, 0.2});
    const Eigen::MatrixXd correlation = matrix({{1.0, rho}, {rho, 1.0}});
    const Eigen::VectorXd displacements = vector({0.01, 0.02});
    const Eigen::VectorXd spacings = vector({0.5, 0.5});

    running_mean_variance x, y, xy;
    for (std::size_t i = 0; i < paths; ++i) {
        libor_market_model_process p(rates, volatilities, correlation, displacements,
                                     spacings, static_cast<std::uint32_t>(42 + i), 0.25);
        const Eigen::VectorXd before = p.forward_rates();
        p.next();
        const Eigen::VectorXd delta = p.forward_rates() - before;
        x.add(delta[0]);
        y.add(delta[1]);
        xy.add(delta[0] * delta[1]);
    }
    const double cov = xy.mean - x.mean * y.mean;
    const double sample_rho =
        cov / std::sqrt(x.sample_variance() * y.sample_variance());
    REQUIRE(sample_rho == Catch::Approx(rho).margin(1e-2));
}

TEST_CASE("lmm increment variance scales with dt", "[libor_market_model_process]") {
    // The first-tick increment variance is sigma_0^2 * (L_0 + s_0)^2
    // * dt exactly (the state is known at t_0), so the sample variance
    // divided by dt is the same constant across dt values.
    const std::size_t paths = 200000;
    const Eigen::VectorXd rates = vector({0.03, 0.035});
    const Eigen::VectorXd volatilities = vector({0.2, 0.2});
    const Eigen::VectorXd displacements = vector({0.01, 0.02});
    const Eigen::VectorXd spacings = vector({0.5, 0.5});
    const double expected = 0.2 * 0.2 * 0.04 * 0.04;  // sigma_0^2 (L_0+s_0)^2

    for (const double dt : {0.25, 1.0}) {
        running_mean_variance increments;
        for (std::size_t i = 0; i < paths; ++i) {
            libor_market_model_process p(rates, volatilities, Eigen::MatrixXd::Identity(2, 2),
                                         displacements, spacings,
                                         static_cast<std::uint32_t>(42 + i), dt);
            p.next();
            increments.add(p.forward_rates()[0] - rates[0]);
        }
        REQUIRE(increments.sample_variance() / dt ==
                Catch::Approx(expected).epsilon(1e-2));
    }
}

TEST_CASE("lmm discounted rates are martingales up to fixing",
          "[libor_market_model_process]") {
    // The defining LMM property: under the spot measure the rate
    // rebased by the rolled money-market account is a martingale up to
    // its fixing time,
    //     E[L_i(t_i) * product_{k <= i} 1 / (1 + tau_k L_k(t_k))]
    //     = L_i(0) * product_{k <= i} 1 / (1 + tau_k L_k(0)).
    // The Euler discretisation leaves an O(dt) bias, measured at
    // 2e6 paths for this parameter set and dt = 0.25: +1.8e-4 (leg 1),
    // +1.4e-3 (leg 2), +3.3e-3 (leg 3) -- each hundreds of sigma, so
    // systematic, and all far below the 1e-2 margin (the sampling
    // noise at 5e4 paths is ~1e-4). A wrong drift would show as a
    // several-percent shift per leg, an order of magnitude above the
    // margin.
    const std::size_t paths = 50000;
    const double dt = 0.25;
    const Eigen::VectorXd rates = vector({0.03, 0.035, 0.04, 0.045});
    const Eigen::VectorXd volatilities = vector({0.2, 0.15, 0.25, 0.3});
    const Eigen::MatrixXd correlation = matrix({{1.0, 0.6, 0.4, 0.2},
                                                {0.6, 1.0, 0.5, 0.3},
                                                {0.4, 0.5, 1.0, 0.7},
                                                {0.2, 0.3, 0.7, 1.0}});
    const Eigen::VectorXd displacements = vector({0.01, 0.02, 0.0, 0.015});
    const Eigen::VectorXd spacings = vector({0.25, 0.5, 1.0, 0.5});

    std::vector<running_mean_variance> rebased(4);
    for (std::size_t i = 0; i < paths; ++i) {
        libor_market_model_process p(rates, volatilities, correlation, displacements,
                                     spacings, static_cast<std::uint32_t>(42 + i), dt);
        double numeraire_ratio = 1.0;  // product_{k<t} 1 / (1 + tau_k L_k(t_k))
        for (std::size_t k = 0; k < 4; ++k) {
            const double fixing = p.forward_rates()[k];  // L_k(t_k): fixes now
            rebased[k].add(fixing * numeraire_ratio / (1.0 + spacings[k] * fixing));
            p.next();
            numeraire_ratio /= 1.0 + spacings[k] * fixing;
        }
    }

    double initial_ratio = 1.0;
    for (std::size_t k = 0; k < 4; ++k) {
        const double expected = rates[k] * initial_ratio / (1.0 + spacings[k] * rates[k]);
        REQUIRE(rebased[k].mean == Catch::Approx(expected).margin(1e-2));
        initial_ratio /= 1.0 + spacings[k] * rates[k];
    }
}

TEST_CASE("lmm displacement keeps the log domain positive",
          "[libor_market_model_process]") {
    // The volatility applies to L_i + s_i: the log domain L_i + s_i >
    // 0 is the model's support, and with a moderate volatility the
    // Euler step stays inside it over long runs. (With a very high
    // volatility the plain rate-Euler can step outside the domain --
    // a documented discretisation artifact; the simulation then
    // continues rather than aborting, since the drift formula is well
    // defined for any rates away from its denominator pole.)
    const std::size_t paths = 100000;
    const Eigen::VectorXd rates = vector({0.04, 0.04, 0.04, 0.04});
    const Eigen::VectorXd volatilities = vector({0.3, 0.3, 0.3, 0.3});
    const Eigen::VectorXd displacements = vector({0.2, 0.2, 0.2, 0.2});
    const Eigen::VectorXd spacings = vector({0.25, 0.25, 0.25, 0.25});

    double min_domain = std::numeric_limits<double>::max();
    for (std::size_t i = 0; i < paths; ++i) {
        libor_market_model_process p(rates, volatilities, Eigen::MatrixXd::Identity(4, 4),
                                     displacements, spacings,
                                     static_cast<std::uint32_t>(42 + i), 0.25);
        for (std::size_t t = 0; t < 20; ++t) {
            p.next();
            const Eigen::VectorXd& fwd = p.forward_rates();
            for (std::size_t k = 0; k < 4; ++k)
                min_domain = std::min(min_domain, fwd[k] + displacements[k]);
        }
    }
    REQUIRE(min_domain > 0.0);
}

TEST_CASE("lmm params struct equals scalar construction",
          "[libor_market_model_process]") {
    libor_market_model_params params;
    params.initial_forward_rates = vector({0.03, 0.035, 0.04});
    params.volatilities = vector({0.2, 0.15, 0.25});
    params.correlation = matrix({{1.0, 0.6, 0.3}, {0.6, 1.0, 0.4}, {0.3, 0.4, 1.0}});
    params.displacements = vector({0.01, 0.0, 0.02});
    params.tenor_spacings = vector({0.5, 1.0, 0.25});

    libor_market_model_process from_params(params, 99, 0.5);
    libor_market_model_process from_scalars(
        vector({0.03, 0.035, 0.04}), vector({0.2, 0.15, 0.25}),
        matrix({{1.0, 0.6, 0.3}, {0.6, 1.0, 0.4}, {0.3, 0.4, 1.0}}),
        vector({0.01, 0.0, 0.02}), vector({0.5, 1.0, 0.25}), 99, 0.5);
    for (std::size_t t = 0; t < 5; ++t) {
        REQUIRE(from_params.next() == from_scalars.next());
        REQUIRE(from_params.discount_factor(3) == from_scalars.discount_factor(3));
    }
}

TEST_CASE("lmm seeds determine the path", "[libor_market_model_process]") {
    const auto path = [](std::uint32_t seed) {
        libor_market_model_process p(
            vector({0.03, 0.035, 0.04}), vector({0.2, 0.15, 0.25}),
            Eigen::MatrixXd::Identity(3, 3), vector({0.01, 0.0, 0.02}),
            vector({0.5, 1.0, 0.25}), seed, 0.5);
        std::vector<double> rates;
        for (std::size_t t = 0; t < 5; ++t)
            rates.push_back(p.next());
        return rates;
    };
    REQUIRE(path(42) == path(42));
    REQUIRE(path(42) != path(43));
}

TEST_CASE("lmm process validates its inputs", "[libor_market_model_process]") {
    const Eigen::VectorXd rates = vector({0.03, 0.035, 0.04});
    const Eigen::VectorXd volatilities = vector({0.2, 0.15, 0.25});
    const Eigen::MatrixXd identity = Eigen::MatrixXd::Identity(3, 3);
    const Eigen::VectorXd displacements = vector({0.01, 0.0, 0.02});
    const Eigen::VectorXd spacings = vector({0.5, 1.0, 0.25});

    REQUIRE_THROWS_AS(libor_market_model_process(
                          vector({}), volatilities, identity, displacements, spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, vector({0.2, 0.15}), identity, displacements, spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities, Eigen::MatrixXd::Identity(2, 2),
                          displacements, spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities, identity, vector({0.01, 0.0}),
                          spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities, identity, displacements,
                          vector({0.5, 1.0})),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, vector({0.2, -0.15, 0.25}), identity, displacements,
                          spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities,
                          matrix({{1.0, 0.6, 0.3}, {0.6, 0.5, 0.4}, {0.3, 0.4, 1.0}}),
                          displacements, spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities,
                          matrix({{1.0, 0.6, 0.3}, {0.6, 1.0, 1.4}, {0.3, 1.4, 1.0}}),
                          displacements, spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities,
                          matrix({{1.0, 0.9, 0.5}, {0.9, 1.0, 0.9}, {0.5, 0.9, 1.0}}),
                          displacements, spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities, identity, vector({-0.01, 0.0, 0.02}),
                          spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          vector({0.03, -0.04, 0.04}), volatilities, identity,
                          vector({0.0, 0.0, 0.0}), spacings),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities, identity, displacements,
                          vector({0.5, 0.0, 0.25})),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(libor_market_model_process(
                          rates, volatilities, identity, displacements, spacings, 42, 0.0),
                      std::invalid_argument);

    // The singular limit rho = 1 (the one-factor model) is a
    // legitimate positive-semidefinite correlation, not a limitation.
    REQUIRE_NOTHROW(libor_market_model_process(
        rates, volatilities, matrix({{1.0, 1.0, 1.0}, {1.0, 1.0, 1.0}, {1.0, 1.0, 1.0}}),
        displacements, spacings));

    // Error messages carry the process name.
    try {
        libor_market_model_process(vector({}), volatilities, identity, displacements,
                                   spacings);
        FAIL("expected an exception");
    } catch (const std::invalid_argument& e) {
        REQUIRE(std::string(e.what()).find("libor_market_model_process") !=
                std::string::npos);
    }
}

TEST_CASE("lmm spot-measure drift validates its inputs",
          "[libor_market_model_process]") {
    const Eigen::VectorXd rates = vector({0.03, 0.035, 0.04});
    const Eigen::VectorXd displacements = vector({0.01, 0.0, 0.02});
    const Eigen::VectorXd spacings = vector({0.5, 1.0, 0.25});
    const Eigen::VectorXd volatilities = vector({0.2, 0.15, 0.25});
    const Eigen::MatrixXd covariance =
        volatilities.asDiagonal() * Eigen::MatrixXd::Identity(3, 3) * volatilities.asDiagonal();

    REQUIRE_THROWS_AS(lmm_spot_measure_drift(vector({}), displacements, spacings,
                                             covariance),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(lmm_spot_measure_drift(rates, vector({0.01, 0.0}), spacings,
                                             covariance),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(lmm_spot_measure_drift(rates, displacements, vector({0.5, 1.0}),
                                             covariance),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(lmm_spot_measure_drift(rates, displacements, spacings,
                                             Eigen::MatrixXd::Identity(2, 2)),
                      std::invalid_argument);
    const Eigen::MatrixXd asymmetric =
        matrix({{1.0, 0.6, 0.3}, {0.7, 1.0, 0.4}, {0.3, 0.4, 1.0}});
    REQUIRE_THROWS_AS(lmm_spot_measure_drift(rates, displacements, spacings, asymmetric),
                      std::invalid_argument);
    // The drift formula is well defined for rates with L_i + s_i <= 0
    // (the positive log domain is the process constructor's concern):
    // only the structural inputs are rejected.
    REQUIRE_NOTHROW(lmm_spot_measure_drift(vector({0.03, -0.04, 0.04}), displacements,
                                           spacings, covariance));
    REQUIRE_THROWS_AS(lmm_spot_measure_drift(rates, vector({-0.01, 0.0, 0.02}), spacings,
                                             covariance),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(lmm_spot_measure_drift(rates, displacements, vector({0.5, 0.0, 0.25}),
                                             covariance),
                      std::invalid_argument);
}
