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
#include "ores.analytics.quant/service/processes/libor_market_model_process.hpp"
#include "ores.analytics.quant/math/psd_matrix.hpp"
#include <cmath>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

/**
 * @brief Shared validation of the rate grid.
 *
 * Both lmm_spot_measure_drift and the process constructor validate
 * the same rate-grid inputs; @p prefix keeps the error message honest
 * about which entry point rejected them. NaN values fail every
 * comparison below and are rejected alongside out-of-range ones.
 *
 * The rates themselves are deliberately not required to keep
 * L_i + s_i positive: the drift formula is well defined for any rates
 * with 1 + tau_i * L_i > 0, and the plain rate-Euler discretisation
 * can step outside the log domain under a high volatility -- a
 * documented discretisation artifact, not a reason to abort a
 * simulation. Only the numeraire pole L_i = -1/tau_i itself is a hard
 * error: the drift crosses it only by sign-flipping the weight on a
 * negative growth factor, which the drift function rejects. The
 * positive log domain is the model's support and is enforced on the
 * initial rates by the process constructor only.
 */
void validate_grid(const Eigen::VectorXd& forward_rates,
                   const Eigen::VectorXd& displacements,
                   const Eigen::VectorXd& tenor_spacings,
                   const char* prefix) {
    const std::size_t num_rates = forward_rates.size();
    if (num_rates == 0)
        throw std::invalid_argument(std::string(prefix) + ": forward_rates must not be empty");
    if (displacements.size() != num_rates)
        throw std::invalid_argument(std::string(prefix) + ": displacements must have one entry per "
                                                          "forward_rates entry");
    if (tenor_spacings.size() != num_rates)
        throw std::invalid_argument(std::string(prefix) +
                                    ": tenor_spacings must have one entry per "
                                    "forward_rates entry");
    for (std::size_t i = 0; i < num_rates; ++i) {
        if (!(displacements[i] >= 0.0))
            throw std::invalid_argument(
                std::string(prefix) + ": displacements must be non-negative, got " +
                std::to_string(displacements[i]) + " at index " + std::to_string(i));
        if (!(tenor_spacings[i] > 0.0))
            throw std::invalid_argument(
                std::string(prefix) + ": tenor_spacings must be strictly positive, got " +
                std::to_string(tenor_spacings[i]) + " at index " + std::to_string(i));
    }
}

} // namespace

Eigen::VectorXd lmm_spot_measure_drift(const Eigen::VectorXd& forward_rates,
                                       const Eigen::VectorXd& displacements,
                                       const Eigen::VectorXd& tenor_spacings,
                                       const Eigen::MatrixXd& covariance) {
    validate_grid(forward_rates, displacements, tenor_spacings, "lmm_spot_measure_drift");
    const std::size_t num_rates = forward_rates.size();
    if (covariance.rows() != num_rates || covariance.cols() != num_rates)
        throw std::invalid_argument("lmm_spot_measure_drift: covariance must be square "
                                    "with one row and column per forward_rates entry");
    // The symmetry check is tolerance based: the natural construction
    // C = diag(sigma) * rho * diag(sigma) computes the two off-diagonal
    // entries in different rounding orders, so strict equality would
    // reject a legitimate covariance. A NaN entry fails the comparison
    // below and is rejected alongside a genuinely asymmetric matrix.
    for (std::size_t i = 0; i < num_rates; ++i) {
        for (std::size_t j = i + 1; j < num_rates; ++j) {
            const double difference = std::abs(covariance(i, j) - covariance(j, i));
            const double tolerance = 1e-12 * std::max(1.0, std::abs(covariance(i, j)));
            if (!(difference <= tolerance))
                throw std::invalid_argument("lmm_spot_measure_drift: covariance must be "
                                            "symmetric, got " +
                                            std::to_string(covariance(i, j)) + " at (" +
                                            std::to_string(i) + ", " + std::to_string(j) +
                                            ") and " + std::to_string(covariance(j, i)) + " at (" +
                                            std::to_string(j) + ", " + std::to_string(i) + ")");
        }
    }

    // The weight rate j puts on the money-market numeraire over its
    // accrual period: tau_j * (L_j + s_j) / (1 + tau_j * L_j), exactly
    // the tmp vector of QuantLib's LMMDriftCalculator. The denominator
    // is the numeraire's own growth factor over the period: it must
    // stay positive, so a rate that crossed the pole L_j = -1/tau_j is
    // a hard error, not a silently sign-flipped weight.
    Eigen::VectorXd weights(num_rates);
    for (std::size_t j = 0; j < num_rates; ++j) {
        const double denominator = 1.0 + tenor_spacings[j] * forward_rates[j];
        if (!(denominator > 0.0))
            throw std::invalid_argument("lmm_spot_measure_drift: the numeraire growth "
                                        "factor 1 + tau_j * L_j is not positive at rate " +
                                        std::to_string(j) + ": got " + std::to_string(denominator));
        weights[j] = tenor_spacings[j] * (forward_rates[j] + displacements[j]) / denominator;
    }

    // The spot-measure drift of rate i sums the covariance with each
    // rate j <= i times that rate's numeraire weight (LMMDriftCalculator
    // with numeraire = 0: the sum range is j in [0, i] and there is no
    // sign flip).
    Eigen::VectorXd drift(num_rates);
    for (std::size_t i = 0; i < num_rates; ++i) {
        double sum = 0.0;
        for (std::size_t j = 0; j <= i; ++j)
            sum += covariance(i, j) * weights[j];
        drift[i] = sum;
    }
    return drift;
}

libor_market_model_process::libor_market_model_process(Eigen::VectorXd initial_forward_rates,
                                                       Eigen::VectorXd volatilities,
                                                       Eigen::MatrixXd correlation,
                                                       Eigen::VectorXd displacements,
                                                       Eigen::VectorXd tenor_spacings,
                                                       std::uint32_t seed,
                                                       double dt)
    : dt_(dt)
    , rng_(seed) {

    // The constructor validates everything (with its own prefix),
    // builds the covariance and its Cholesky root, and then delegates
    // the drift core to the free function -- which can then never
    // reject these inputs.
    const std::size_t num_rates = initial_forward_rates.size();
    if (num_rates == 0)
        throw std::invalid_argument("libor_market_model_process: "
                                    "initial_forward_rates must not be empty");
    if (volatilities.size() != num_rates)
        throw std::invalid_argument("libor_market_model_process: volatilities must have "
                                    "one entry per initial_forward_rates entry");
    if (correlation.rows() != num_rates || correlation.cols() != num_rates)
        throw std::invalid_argument("libor_market_model_process: correlation must be "
                                    "square with one row and column per "
                                    "initial_forward_rates entry");
    if (!(dt > 0.0))
        throw std::invalid_argument("libor_market_model_process: dt must be "
                                    "strictly positive");
    for (std::size_t i = 0; i < num_rates; ++i) {
        if (!(volatilities[i] >= 0.0))
            throw std::invalid_argument("libor_market_model_process: volatilities must "
                                        "be non-negative, got " +
                                        std::to_string(volatilities[i]) + " at index " +
                                        std::to_string(i));
        if (!(correlation(i, i) == 1.0))
            throw std::invalid_argument("libor_market_model_process: correlation must "
                                        "have unit diagonal, got " +
                                        std::to_string(correlation(i, i)) + " at index " +
                                        std::to_string(i));
        for (std::size_t j = i + 1; j < num_rates; ++j) {
            if (!(correlation(i, j) >= -1.0 && correlation(i, j) <= 1.0))
                throw std::invalid_argument("libor_market_model_process: correlation "
                                            "entries must be within [-1, 1], got " +
                                            std::to_string(correlation(i, j)) + " at (" +
                                            std::to_string(i) + ", " + std::to_string(j) + ")");
        }
    }
    validate_grid(
        initial_forward_rates, displacements, tenor_spacings, "libor_market_model_process");

    // The model's support: the volatility applies to L_i + s_i, so the
    // simulation starts inside the positive log domain. (The drift
    // core itself does not require this: the Euler step can wander out
    // of the domain mid-run, a documented discretisation artifact that
    // the simulation survives.)
    for (std::size_t i = 0; i < num_rates; ++i) {
        if (!(initial_forward_rates[i] + displacements[i] > 0.0))
            throw std::invalid_argument(
                "libor_market_model_process: "
                "initial_forward_rates[i] + displacements[i] must "
                "be positive, got " +
                std::to_string(initial_forward_rates[i] + displacements[i]) + " at index " +
                std::to_string(i));
    }

    forward_rates_ = std::move(initial_forward_rates);
    displacements_ = std::move(displacements);
    tenor_spacings_ = std::move(tenor_spacings);

    // The instantaneous covariance C = diag(sigma) * rho * diag(sigma)
    // and the root of the correlation for the correlated draws. The
    // correlation must be positive semidefinite; the helper's eigen
    // fallback also accepts the singular limit rho = 1 (the one-factor
    // model) that a Cholesky alone would reject.
    covariance_ = volatilities.asDiagonal() * correlation * volatilities.asDiagonal();
    cholesky_ = math::psd_matrix_square_root(
        correlation, "libor_market_model_process: correlation must be positive semidefinite");

    vol_sqrt_dt_ = volatilities * std::sqrt(dt_);
    z_.resize(num_rates);
    correlated_.resize(num_rates);
}

libor_market_model_process::libor_market_model_process(const libor_market_model_params& params,
                                                       std::uint32_t seed,
                                                       double dt)
    : libor_market_model_process(params.initial_forward_rates,
                                 params.volatilities,
                                 params.correlation,
                                 params.displacements,
                                 params.tenor_spacings,
                                 seed,
                                 dt) {}

double libor_market_model_process::next() {
    // The drift is state dependent: recompute it from the current
    // rates, draw the correlated shocks, then take one Euler step of
    // the displaced-lognormal dynamics.
    const Eigen::VectorXd drift =
        lmm_spot_measure_drift(forward_rates_, displacements_, tenor_spacings_, covariance_);
    for (std::size_t i = 0; i < z_.size(); ++i)
        z_[i] = normal_(rng_);
    correlated_ = cholesky_ * z_;
    for (std::size_t i = 0; i < forward_rates_.size(); ++i)
        forward_rates_[i] += drift[i] * dt_ + vol_sqrt_dt_[i] *
                                                  (forward_rates_[i] + displacements_[i]) *
                                                  correlated_[i];
    ++tick_;
    return current();
}

double libor_market_model_process::current() const {
    return forward_rates_[0];
}

double libor_market_model_process::discount_factor(std::size_t ticks_ahead) const {
    if (ticks_ahead == 0)
        return 1.0;
    if (ticks_ahead > forward_rates_.size())
        throw std::out_of_range("libor_market_model_process: discounting beyond the last "
                                "rate's payment date is not defined: the grid holds " +
                                std::to_string(forward_rates_.size()) + " rates");

    // The rolled money-market account over the ticks ahead, priced
    // from the current forward curve: each rate L_k discounts its
    // period by 1 / (1 + tau_k * L_k). A growth factor that crossed
    // the pole L_k = -1/tau_k would flip the discount negative -- an
    // arbitrage -- so it is rejected rather than priced silently.
    double result = 1.0;
    for (std::size_t k = 0; k < ticks_ahead; ++k) {
        const double denominator = 1.0 + tenor_spacings_[k] * forward_rates_[k];
        if (!(denominator > 0.0))
            throw std::runtime_error("libor_market_model_process: discount_factor: the "
                                     "numeraire growth factor 1 + tau_k * L_k is not "
                                     "positive at rate " +
                                     std::to_string(k) + ": got " + std::to_string(denominator));
        result /= denominator;
    }
    return result;
}

const Eigen::VectorXd& libor_market_model_process::forward_rates() const {
    return forward_rates_;
}

} // namespace ores::analytics::quant::service
