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
#include "ores.analytics.quant/math/psd_matrix.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

/**
 * @brief Below this value a mean-reversion sum behaves as zero and the
 * per-tick variance factor collapses to its dt limit (the hull_white
 * and two_factor_gaussian convention).
 */
const double small_kappa_threshold = std::sqrt(std::numeric_limits<double>::epsilon());

} // namespace

affine_term_structure_process::affine_term_structure_process(Eigen::VectorXd kappas,
                                                             Eigen::MatrixXd sigma,
                                                             Eigen::VectorXd theta,
                                                             double delta_0,
                                                             Eigen::VectorXd deltas,
                                                             Eigen::VectorXd initial_factors,
                                                             std::uint32_t seed,
                                                             double dt)
    : dt_(dt)
    , rng_(seed) {

    // The validation prefix is the process name: the constructor is the
    // public entry point and must say who rejected the input.
    const std::size_t num_factors = kappas.size();
    if (num_factors == 0)
        throw std::invalid_argument("affine_term_structure_process: kappas must not be empty");
    if (theta.size() != num_factors || deltas.size() != num_factors ||
        initial_factors.size() != num_factors)
        throw std::invalid_argument("affine_term_structure_process: theta, deltas and "
                                    "initial_factors must each have one entry per factor in kappas");
    if (sigma.rows() != num_factors || sigma.cols() != num_factors)
        throw std::invalid_argument("affine_term_structure_process: sigma must be square with one "
                                    "row and column per factor in kappas");
    for (std::size_t i = 0; i < num_factors; ++i) {
        if (!(kappas[i] >= 0.0))
            throw std::invalid_argument("affine_term_structure_process: kappas must be "
                                        "non-negative, got " + std::to_string(kappas[i]) +
                                        " at index " + std::to_string(i));
    }
    // The diagonal participates in the symmetry check: a NaN diagonal
    // entry fails the self-comparison and is rejected here.
    for (std::size_t i = 0; i < num_factors; ++i)
        for (std::size_t j = i; j < num_factors; ++j)
            if (!(sigma(i, j) == sigma(j, i)))
                throw std::invalid_argument("affine_term_structure_process: sigma must be "
                                            "symmetric");
    // Validate the raw factor covariance itself, not the derived
    // per-tick product: sigma_dt_ is the Hadamard product of sigma with
    // a kappa-dependent Gram factor, and an indefinite sigma can damp
    // into a positive-definite sigma_dt_ when the kappas differ -- the
    // raw input is the documented contract, so it is what gets checked.
    // The singular limit sigma == 0 (deterministic factors) is a
    // legitimate degenerate input and is accepted by the helper.
    math::psd_matrix_square_root(
        sigma, "affine_term_structure_process: sigma must be positive semidefinite");
    if (!(dt_ > 0.0))
        throw std::invalid_argument("affine_term_structure_process: dt must be strictly positive");

    delta_0_ = delta_0;
    deltas_ = std::move(deltas);
    theta_ = std::move(theta);
    factors_ = std::move(initial_factors);

    // Precompute the per-tick transition pieces: the factor decay
    // lambda = exp(-kappas*dt), the shift (I - Lambda)*theta of the
    // exact one-step mean, the per-tick short-rate sensitivity and the
    // exact per-tick factor covariance.
    lambda_ = (kappas * -dt_).array().exp();
    theta_shift_ = theta_ - lambda_.cwiseProduct(theta_);
    deltas_dt_ = deltas_ * dt_;

    sigma_dt_.resize(num_factors, num_factors);
    for (std::size_t i = 0; i < num_factors; ++i)
        for (std::size_t j = 0; j < num_factors; ++j) {
            const double kappa_sum = kappas[i] + kappas[j];
            const double factor = (kappa_sum > small_kappa_threshold)
                ? (1.0 - std::exp(-kappa_sum * dt_)) / kappa_sum
                : dt_;
            sigma_dt_(i, j) = sigma(i, j) * factor;
        }

    // The root of the per-tick covariance realises the correlated
    // shocks: the Cholesky factor when it is positive definite, the
    // symmetric eigen root for the singular but positive-semidefinite
    // limit. The raw-sigma check above already guarantees the
    // semidefiniteness of the Hadamard product, so this call can only
    // reject a numerical rounding failure.
    cholesky_ = math::psd_matrix_square_root(
        sigma_dt_, "affine_term_structure_process: sigma must be positive semidefinite: "
                   "its per-tick covariance is not");

    // Scratch for the standard-normal shocks: allocated once per
    // construction, reused by every next() call.
    z_.resize(num_factors);
}

affine_term_structure_process::affine_term_structure_process(
    const affine_term_structure_params& params, std::uint32_t seed, double dt)
    : affine_term_structure_process(params.kappas,
                                    params.sigma,
                                    params.theta,
                                    params.delta_0,
                                    params.deltas,
                                    params.initial_factors,
                                    seed,
                                    dt) {}

double affine_term_structure_process::next() {
    // Independent standard normals, one per factor (the same consumption
    // order as two_factor_gaussian_process's u1, u2), then the exact
    // one-step Gaussian transition with Cholesky-correlated shocks.
    for (std::size_t i = 0; i < z_.size(); ++i)
        z_[i] = normal_(rng_);
    factors_ = theta_ + lambda_.cwiseProduct(factors_ - theta_) + cholesky_ * z_;
    ++tick_;
    return current();
}

double affine_term_structure_process::current() const {
    return delta_0_ + deltas_.dot(factors_);
}

double affine_term_structure_process::discount_factor(std::size_t ticks_ahead) const {
    if (ticks_ahead == 0)
        return 1.0;

    // The affine recursion run forward from the bond's maturity: b holds
    // B_{i+1} and a accumulates the A shift, exactly as the scalar
    // hull_white recursion does per tick.
    const std::size_t num_factors = factors_.size();
    Eigen::VectorXd b = Eigen::VectorXd::Zero(num_factors);
    double a = 0.0;
    for (std::size_t i = 0; i < ticks_ahead; ++i) {
        a += -delta_0_ * dt_ - b.dot(theta_shift_) + 0.5 * b.dot(sigma_dt_ * b);
        b = deltas_dt_ + lambda_.cwiseProduct(b);
    }
    return std::exp(a - b.dot(factors_));
}

const Eigen::VectorXd& affine_term_structure_process::factors() const {
    return factors_;
}

} // namespace ores::analytics::quant::service
