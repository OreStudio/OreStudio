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
#include "ores.analytics.quant/service/processes/quadratic_gaussian_process.hpp"
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
 * per-tick variance factor collapses to its dt limit (the affine
 * term-structure process convention).
 */
const double small_kappa_threshold = std::sqrt(std::numeric_limits<double>::epsilon());

} // namespace

quadratic_gaussian_process::quadratic_gaussian_process(Eigen::VectorXd kappas,
                                                       Eigen::MatrixXd sigma,
                                                       Eigen::VectorXd theta,
                                                       double delta_0,
                                                       Eigen::VectorXd deltas,
                                                       Eigen::MatrixXd gamma,
                                                       Eigen::VectorXd initial_factors,
                                                       std::uint32_t seed,
                                                       double dt)
    : dt_(dt)
    , rng_(seed) {

    // The validation prefix is the process name: the constructor is the
    // public entry point and must say who rejected the input. The checks
    // mirror the affine process's, plus the quadratic matrix gamma.
    const std::size_t num_factors = kappas.size();
    if (num_factors == 0)
        throw std::invalid_argument("quadratic_gaussian_process: kappas must not be empty");
    if (theta.size() != num_factors || deltas.size() != num_factors ||
        initial_factors.size() != num_factors)
        throw std::invalid_argument("quadratic_gaussian_process: theta, deltas and "
                                    "initial_factors must each have one entry per factor in kappas");
    if (sigma.rows() != num_factors || sigma.cols() != num_factors)
        throw std::invalid_argument("quadratic_gaussian_process: sigma must be square with one "
                                    "row and column per factor in kappas");
    if (gamma.rows() != num_factors || gamma.cols() != num_factors)
        throw std::invalid_argument("quadratic_gaussian_process: gamma must be square with one "
                                    "row and column per factor in kappas");
    for (std::size_t i = 0; i < num_factors; ++i) {
        if (!(kappas[i] >= 0.0))
            throw std::invalid_argument("quadratic_gaussian_process: kappas must be "
                                        "non-negative, got " + std::to_string(kappas[i]) +
                                        " at index " + std::to_string(i));
    }
    // The diagonal participates in the symmetry check: a NaN diagonal
    // entry fails the self-comparison and is rejected here.
    for (std::size_t i = 0; i < num_factors; ++i)
        for (std::size_t j = i; j < num_factors; ++j)
            if (!(sigma(i, j) == sigma(j, i)))
                throw std::invalid_argument("quadratic_gaussian_process: sigma must be "
                                            "symmetric");
    for (std::size_t i = 0; i < num_factors; ++i)
        for (std::size_t j = i; j < num_factors; ++j)
            if (!(gamma(i, j) == gamma(j, i)))
                throw std::invalid_argument("quadratic_gaussian_process: gamma must be "
                                            "symmetric");
    // The short rate is bounded below exactly when gamma is positive
    // semidefinite; the eigen decomposition on the symmetrised matrix
    // (identical to gamma after the symmetry check) decides.
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> gamma_solver(
        0.5 * (gamma + gamma.transpose()));
    if (gamma_solver.info() != Eigen::Success)
        throw std::invalid_argument("quadratic_gaussian_process: gamma must be positive "
                                    "semidefinite");
    const double largest = gamma_solver.eigenvalues().maxCoeff();
    const double tolerance = -1e-10 * std::max(1.0, std::abs(largest));
    if (!(gamma_solver.eigenvalues().minCoeff() >= tolerance))
        throw std::invalid_argument("quadratic_gaussian_process: gamma must be positive "
                                    "semidefinite");
    // Validate the raw factor covariance itself, not the derived
    // per-tick product (see the affine process constructor for the
    // rationale): sigma_dt_ is the Hadamard product of sigma with a
    // kappa-dependent Gram factor, and an indefinite sigma can damp
    // into a positive-definite sigma_dt_ when the kappas differ. The
    // singular limit sigma == 0 (deterministic factors) is accepted.
    math::psd_matrix_square_root(
        sigma, "quadratic_gaussian_process: sigma must be positive semidefinite");
    if (!(dt_ > 0.0))
        throw std::invalid_argument("quadratic_gaussian_process: dt must be strictly positive");

    delta_0_ = delta_0;
    deltas_ = std::move(deltas);
    theta_ = std::move(theta);
    gamma_ = std::move(gamma);
    factors_ = std::move(initial_factors);

    // Precompute the per-tick transition pieces: the factor decay
    // lambda = exp(-kappas*dt), the shift (I - Lambda)*theta of the
    // exact one-step mean, the per-tick short-rate sensitivities and the
    // exact per-tick factor covariance (all as in the affine process).
    lambda_ = (kappas * -dt_).array().exp();
    theta_shift_ = theta_ - lambda_.cwiseProduct(theta_);
    deltas_dt_ = deltas_ * dt_;
    gamma_dt_ = gamma_ * dt_;

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
    // shocks, exactly as in the affine process: the Cholesky factor for
    // the positive-definite case, the symmetric eigen root for the
    // singular but positive-semidefinite limit. The raw-sigma check
    // above already guarantees the semidefiniteness of the Hadamard
    // product, so this call can only reject a numerical rounding
    // failure.
    cholesky_ = math::psd_matrix_square_root(
        sigma_dt_, "quadratic_gaussian_process: sigma must be positive semidefinite: "
                   "its per-tick covariance is not");

    // Scratch for the standard-normal shocks: allocated once per
    // construction, reused by every next() call.
    z_.resize(num_factors);
}

quadratic_gaussian_process::quadratic_gaussian_process(
    const quadratic_gaussian_params& params, std::uint32_t seed, double dt)
    : quadratic_gaussian_process(params.kappas,
                                 params.sigma,
                                 params.theta,
                                 params.delta_0,
                                 params.deltas,
                                 params.gamma,
                                 params.initial_factors,
                                 seed,
                                 dt) {}

double quadratic_gaussian_process::next() {
    // Independent standard normals, one per factor (the same consumption
    // order as the affine process), then the exact one-step Gaussian
    // transition with Cholesky-correlated shocks.
    for (std::size_t i = 0; i < z_.size(); ++i)
        z_[i] = normal_(rng_);
    factors_ = theta_ + lambda_.cwiseProduct(factors_ - theta_) + cholesky_ * z_;
    ++tick_;
    return current();
}

double quadratic_gaussian_process::current() const {
    return delta_0_ + deltas_.dot(factors_) + factors_.dot(gamma_ * factors_);
}

double quadratic_gaussian_process::discount_factor(std::size_t ticks_ahead) const {
    if (ticks_ahead == 0)
        return 1.0;

    // The exponential-quadratic recursion run forward from the bond's
    // maturity: c and b hold C_{i+1} and B_{i+1}, a accumulates the A
    // shift. Each tick completes the square over the shock
    // eps ~ N(0, V) of the exact transition, V = sigma_dt_: the
    // covariance dressing of the quadratic form is C~ = C (I + 2 V C)^-1
    // and P = V (I + 2 C V)^-1 = V - 2 V C~ V (I is the identity).
    const std::size_t num_factors = factors_.size();
    const Eigen::MatrixXd identity = Eigen::MatrixXd::Identity(num_factors, num_factors);
    Eigen::VectorXd b = Eigen::VectorXd::Zero(num_factors);
    Eigen::MatrixXd c = Eigen::MatrixXd::Zero(num_factors, num_factors);
    double a = 0.0;
    for (std::size_t i = 0; i < ticks_ahead; ++i) {
        // The dressing matrix M = I + 2 V C (V = sigma_dt_): the
        // dressed quadratic form is c_tilde = c M^-1, the solution of
        // M^T c_tilde^T = c^T -- so one LU of M^T serves both the
        // linear solve and the determinant (det M^T == det M). In
        // exact arithmetic M's eigenvalues are 1 + 2 lambda_V lambda_C
        // >= 1 (those of the symmetric positive-definite
        // I + 2 V^(1/2) C V^(1/2)), so a non-positive or non-finite
        // determinant is a numerical blow-up of the Riccati recursion,
        // not a property of the inputs: the price it would produce is
        // nonsense, so it is rejected.
        const Eigen::MatrixXd m = identity + 2.0 * sigma_dt_ * c;
        Eigen::PartialPivLU<Eigen::MatrixXd> lu(m.transpose());
        const double determinant = lu.determinant();
        if (!(std::isfinite(determinant)) || !(determinant > 0.0))
            throw std::runtime_error("quadratic_gaussian_process: discount_factor: the "
                                     "Riccati recursion blew up: I + 2 * sigma_dt * C is "
                                     "not positive definite at tick " +
                                     std::to_string(i));
        const Eigen::MatrixXd c_tilde = lu.solve(c.transpose()).transpose();
        const Eigen::MatrixXd p = sigma_dt_ - 2.0 * sigma_dt_ * c_tilde * sigma_dt_;

        // The A shift: the affine pieces at the shift mean
        // m = theta_shift_, the quadratic Gaussian integral
        // E[exp(-eps' C eps - (2 C m + B)' eps)] = det(I + 2 V C)^-1/2
        //   * exp(1/2 (2 C m + B)' P (2 C m + B)),
        // and the determinant piece (the log of the determinant
        // validated above).
        const Eigen::VectorXd c_theta = c * theta_shift_;
        const Eigen::VectorXd quadratic_arg = 2.0 * c_theta + b;
        const double log_det = 0.5 * std::log(determinant);
        a += -delta_0_ * dt_ - theta_shift_.dot(c_theta) - b.dot(theta_shift_)
             + 0.5 * quadratic_arg.dot(p * quadratic_arg) - log_det;

        // The coefficient recursions: the quadratic rate piece enters C
        // with both factor-decay dressings, and the linear piece enters
        // B through the mean shift and the covariance dressing of the
        // shock.
        c = gamma_dt_ + lambda_.asDiagonal() * c_tilde * lambda_.asDiagonal();
        b = deltas_dt_ + lambda_.cwiseProduct(b + 2.0 * c_tilde *
                                                   (theta_shift_ - sigma_dt_ * b));
    }
    return std::exp(a - b.dot(factors_) - factors_.dot(c * factors_));
}

const Eigen::VectorXd& quadratic_gaussian_process::factors() const {
    return factors_;
}

} // namespace ores::analytics::quant::service
