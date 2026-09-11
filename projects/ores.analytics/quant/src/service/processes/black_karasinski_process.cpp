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
#include "ores.analytics.quant/service/processes/black_karasinski_process.hpp"
#include <cmath>
#include <limits>
#include <stdexcept>

namespace ores::analytics::quant::service {
namespace math = ores::analytics::quant::math;

namespace {
// Mirrors ornstein_uhlenbeck_process's own threshold and QuantLib's
// OrnsteinUhlenbeckProcess::variance() branch point: kappa positive but
// too small to divide by safely still needs the algebraic small-kappa
// limit, not just kappa == 0 exactly.
const double small_kappa_threshold = std::sqrt(std::numeric_limits<double>::epsilon());
}

math::rate_tree build_black_karasinski_tree(
    double log_rate, double kappa, double theta, double sigma, double dt, std::size_t steps) {
    if (sigma < 0.0)
        throw std::invalid_argument("build_black_karasinski_tree: sigma must be non-negative");
    if (dt <= 0.0)
        throw std::invalid_argument("build_black_karasinski_tree: dt must be strictly positive");

    // The driftless branch (kappa <= 0) mirrors next()'s degenerate
    // transition: no mean reversion (decay 1), per-tick variance sigma^2*dt.
    // The level-center update below then reads theta + (center - theta) * 1,
    // i.e. the center stays put -- the same formula in both branches.
    const double decay = kappa > small_kappa_threshold ? std::exp(-kappa * dt) : 1.0;
    const double variance = kappa > small_kappa_threshold ?
                                sigma * sigma * (1.0 - decay * decay) / (2.0 * kappa) :
                                sigma * sigma * dt;
    const double spacing = std::sqrt(3.0 * variance);

    math::rate_tree tree;
    tree.levels.resize(steps + 1);
    double center = log_rate;
    for (std::size_t i = 0; i <= steps; ++i) {
        auto& level = tree.levels[i];
        level.resize(2 * i + 1);
        for (std::size_t j = 0; j < level.size(); ++j) {
            const double offset = static_cast<double>(j) - static_cast<double>(i);
            math::rate_tree_node& node = level[j];
            node.log_rate = center + offset * spacing;
            if (i < steps) {
                // Middle branch nearest the conditional mean offset o*f;
                // |o*f| <= i (decay <= 1) so temp never leaves [-i, i] and
                // the children temp-1..temp+1 land inside the next level's
                // range [-(i+1), i+1] -- no clamping, no range tracking.
                const long temp = std::lround(offset * decay);
                const double e = offset * decay - static_cast<double>(temp);
                const double e2 = e * e;
                const std::size_t base = static_cast<std::size_t>(static_cast<long>(i + 1) + temp);
                node.child_indices = {base - 1, base, base + 1};
                node.child_probabilities = {(1.0 + 3.0 * e2 - 3.0 * e) / 6.0,
                                            (2.0 - 3.0 * e2) / 3.0,
                                            (1.0 + 3.0 * e2 + 3.0 * e) / 6.0};
            }
        }
        center = theta + (center - theta) * decay;
    }
    return tree;
}

black_karasinski_process::black_karasinski_process(
    double kappa, double theta, double sigma, double initial_rate, std::uint32_t seed, double dt)
    : kappa_(kappa)
    , theta_(theta)
    , sigma_(sigma)
    , log_rate_(std::log(initial_rate))
    , dt_(dt)
    , rng_(seed)
    , normal_(0.0, 1.0) {

    if (sigma_ < 0.0)
        throw std::invalid_argument("black_karasinski_process: sigma must be non-negative");
    if (initial_rate <= 0.0)
        throw std::invalid_argument("black_karasinski_process: initial_rate must be positive");
    if (dt_ <= 0.0)
        throw std::invalid_argument("black_karasinski_process: dt must be strictly positive");
}

black_karasinski_process::black_karasinski_process(const black_karasinski_params& params,
                                                   std::uint32_t seed,
                                                   double dt)
    : black_karasinski_process(
          params.kappa, params.theta, params.sigma, params.initial_rate, seed, dt) {}

double black_karasinski_process::next() {
    const double z = normal_(rng_);
    if (kappa_ > small_kappa_threshold) {
        const double decay = std::exp(-kappa_ * dt_);
        const double var = (1.0 - decay * decay) / (2.0 * kappa_);
        log_rate_ = theta_ + (log_rate_ - theta_) * decay + sigma_ * std::sqrt(var) * z;
    } else {
        // kappa <= 0 (or too small to divide by safely): no mean reversion --
        // the same degenerate treatment as ornstein_uhlenbeck_process, and
        // QuantLib's own small-speed algebraic limit (variance = sigma^2*dt).
        log_rate_ += sigma_ * std::sqrt(dt_) * z;
    }
    ++tick_;
    return std::exp(log_rate_);
}

double black_karasinski_process::current() const {
    return std::exp(log_rate_);
}

double black_karasinski_process::discount_factor(std::size_t ticks_ahead) const {
    if (ticks_ahead == 0)
        return 1.0;

    if (sigma_ == 0.0) {
        // Deterministic closed form along the exact mean-reversion path
        // m_i = theta + (ln r - theta) * e^{-kappa*i*dt}: the bond price is
        // exp(-sum_i exp(m_i) * dt). O(ticks_ahead) instead of a degenerate
        // zero-spacing lattice; identical result (see the tests).
        const double decay = kappa_ > small_kappa_threshold ? std::exp(-kappa_ * dt_) : 1.0;
        double sum = 0.0;
        double center = log_rate_;
        for (std::size_t i = 0; i < ticks_ahead; ++i) {
            sum += std::exp(center);
            center = theta_ + (center - theta_) * decay;
        }
        return std::exp(-sum * dt_);
    }

    const math::rate_tree tree =
        build_black_karasinski_tree(log_rate_, kappa_, theta_, sigma_, dt_, ticks_ahead);
    const auto prices = math::propagate_state_prices(tree, math::tree_node{0, 0}, ticks_ahead, dt_);
    double price = 0.0;
    for (const double p : prices)
        price += p;
    return price;
}

} // namespace ores::analytics::quant::service
