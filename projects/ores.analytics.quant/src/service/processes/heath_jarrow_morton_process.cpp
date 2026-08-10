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
#include "ores.analytics.quant/service/processes/heath_jarrow_morton_process.hpp"
#include <algorithm>
#include <cmath>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::analytics::quant::service {

namespace {

/**
 * @brief Shared validation of the grid, the volatility vector and dt.
 *
 * Both hjm_no_arbitrage_drift and the process constructor validate the
 * same inputs; @p prefix keeps the error message honest about which
 * entry point rejected them. NaN values fail every comparison below and
 * are rejected alongside out-of-range ones.
 */
void validate_grid(const Eigen::VectorXd& initial_forward_rates,
                   const Eigen::VectorXd& volatilities,
                   const Eigen::VectorXd& tenor_spacings,
                   double dt,
                   const char* prefix) {
    const std::size_t num_rates = initial_forward_rates.size();
    if (num_rates == 0)
        throw std::invalid_argument(std::string(prefix) +
                                    ": initial_forward_rates must not be empty");
    if (volatilities.size() != num_rates)
        throw std::invalid_argument(std::string(prefix) + ": volatilities must have one entry per "
                                                          "initial_forward_rates entry");
    if (tenor_spacings.size() != num_rates - 1)
        throw std::invalid_argument(std::string(prefix) +
                                    ": tenor_spacings must have one entry fewer than "
                                    "initial_forward_rates");
    for (std::size_t i = 0; i < volatilities.size(); ++i) {
        if (!(volatilities[i] >= 0.0))
            throw std::invalid_argument(
                std::string(prefix) + ": volatilities must be non-negative, got " +
                std::to_string(volatilities[i]) + " at index " + std::to_string(i));
    }
    for (std::size_t i = 0; i < tenor_spacings.size(); ++i) {
        if (!(tenor_spacings[i] > 0.0))
            throw std::invalid_argument(
                std::string(prefix) + ": tenor_spacings must be strictly positive, got " +
                std::to_string(tenor_spacings[i]) + " at index " + std::to_string(i));
    }
    if (!(dt > 0.0))
        throw std::invalid_argument(std::string(prefix) + ": dt must be strictly positive");
}

} // namespace

Eigen::VectorXd hjm_no_arbitrage_drift(const Eigen::VectorXd& volatilities,
                                       const Eigen::VectorXd& tenor_spacings) {
    const std::size_t num_rates = volatilities.size();
    if (num_rates == 0)
        throw std::invalid_argument("hjm_no_arbitrage_drift: volatilities must not be empty");
    if (tenor_spacings.size() != num_rates - 1)
        throw std::invalid_argument("hjm_no_arbitrage_drift: tenor_spacings must have one entry "
                                    "fewer than volatilities");
    for (std::size_t i = 0; i < num_rates; ++i) {
        if (!(volatilities[i] >= 0.0))
            throw std::invalid_argument("hjm_no_arbitrage_drift: volatilities must be "
                                        "non-negative, got " +
                                        std::to_string(volatilities[i]) + " at index " +
                                        std::to_string(i));
    }
    for (std::size_t i = 0; i < tenor_spacings.size(); ++i) {
        if (!(tenor_spacings[i] > 0.0))
            throw std::invalid_argument("hjm_no_arbitrage_drift: tenor_spacings must be "
                                        "strictly positive, got " +
                                        std::to_string(tenor_spacings[i]) + " at index " +
                                        std::to_string(i));
    }

    // The drift of the rate at tenor tau_i is sigma_i times the integral
    // of the volatility curve from the front to tau_i; with the
    // piecewise-constant convention the integral over [tau_{j-1}, tau_j]
    // is sigma_j * (tau_j - tau_{j-1}). The front rate therefore has
    // zero drift: the short rate is a martingale under the risk-neutral
    // measure.
    Eigen::VectorXd drift(volatilities.size());
    double vol_integral = 0.0;
    for (std::size_t i = 0; i < volatilities.size(); ++i) {
        drift[i] = volatilities[i] * vol_integral;
        if (i + 1 < volatilities.size())
            vol_integral += volatilities[i + 1] * tenor_spacings[i];
    }
    return drift;
}

heath_jarrow_morton_process::heath_jarrow_morton_process(Eigen::VectorXd initial_forward_rates,
                                                         Eigen::VectorXd volatilities,
                                                         Eigen::VectorXd tenor_spacings,
                                                         std::uint32_t seed,
                                                         double dt)
    : dt_(dt)
    , rng_(seed) {

    // The constructor rejects invalid input with its own prefix before
    // the drift core runs (its own checks can then never fire).
    validate_grid(
        initial_forward_rates, volatilities, tenor_spacings, dt_, "heath_jarrow_morton_process");
    forward_rates_ = std::move(initial_forward_rates);

    // The drift and the volatility scale are time-homogeneous: both are
    // precomputed once, and next() is a single fused vector update.
    const Eigen::VectorXd drift = hjm_no_arbitrage_drift(volatilities, tenor_spacings);
    drift_dt_ = drift * dt_;
    vol_sqrt_dt_ = volatilities * std::sqrt(dt_);

    tenors_.resize(forward_rates_.size());
    tenors_[0] = 0.0;
    for (std::size_t i = 0; i < tenor_spacings.size(); ++i)
        tenors_[i + 1] = tenors_[i] + tenor_spacings[i];
}

heath_jarrow_morton_process::heath_jarrow_morton_process(const heath_jarrow_morton_params& params,
                                                         std::uint32_t seed,
                                                         double dt)
    : heath_jarrow_morton_process(
          params.initial_forward_rates, params.volatilities, params.tenor_spacings, seed, dt) {}

double heath_jarrow_morton_process::next() {
    // One shared shock moves every grid rate, exactly the single-factor
    // HJM dynamics on the grid.
    const double z = normal_(rng_);
    forward_rates_ = forward_rates_ + drift_dt_ + vol_sqrt_dt_ * z;
    ++tick_;
    return current();
}

double heath_jarrow_morton_process::current() const {
    return forward_rates_[0];
}

double heath_jarrow_morton_process::discount_factor(std::size_t ticks_ahead) const {
    if (ticks_ahead == 0)
        return 1.0;

    // The integral of the current curve over [0, horizon]: piecewise
    // linear between the tenors (trapezoid rule), flat at the last
    // tenor beyond the grid. A horizon inside a segment integrates the
    // partial linear piece.
    const double horizon = ticks_ahead * dt_;
    const std::size_t num_rates = forward_rates_.size();
    double integral = 0.0;
    for (std::size_t i = 0; i + 1 < num_rates && horizon > tenors_[i]; ++i) {
        const double segment_end = tenors_[i + 1];
        const double end = std::min(segment_end, horizon);
        const double length = end - tenors_[i];
        const double spacing = segment_end - tenors_[i];
        const double f_end =
            forward_rates_[i] + (forward_rates_[i + 1] - forward_rates_[i]) * (length / spacing);
        integral += 0.5 * (forward_rates_[i] + f_end) * length;
    }
    const double last_tenor = tenors_[num_rates - 1];
    if (horizon > last_tenor)
        integral += (horizon - last_tenor) * forward_rates_[num_rates - 1];
    return std::exp(-integral);
}

const Eigen::VectorXd& heath_jarrow_morton_process::forward_rates() const {
    return forward_rates_;
}

} // namespace ores::analytics::quant::service
