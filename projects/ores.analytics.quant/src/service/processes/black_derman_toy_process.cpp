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
#include "ores.analytics.quant/service/processes/black_derman_toy_process.hpp"
#include <cmath>
#include <numeric>
#include <stdexcept>
#include <string>

namespace ores::analytics::quant::service {
namespace math = ores::analytics::quant::math;

namespace {

/**
 * @brief Shared validation of the curve, the volatility path and dt.
 *
 * Both build_black_derman_toy_tree and the process constructor validate
 * the same inputs; @p prefix keeps the error message honest about which
 * entry point rejected them. NaN values fail every comparison below and
 * are rejected alongside out-of-range ones.
 */
void validate_curve_inputs(const std::vector<double>& discount_curve,
                           const std::vector<double>& sigma_path,
                           double dt,
                           const char* prefix) {
    if (discount_curve.empty())
        throw std::invalid_argument(std::string(prefix) + ": discount_curve must not be empty");
    for (std::size_t i = 0; i < discount_curve.size(); ++i) {
        const double d = discount_curve[i];
        if (!(d > 0.0 && d < 1.0))
            throw std::invalid_argument(std::string(prefix) + ": discount factor at index " +
                                        std::to_string(i) + " must lie in (0, 1), got " +
                                        std::to_string(d));
        if (i > 0 && !(d < discount_curve[i - 1]))
            throw std::invalid_argument(std::string(prefix) +
                                        ": discount curve must be strictly "
                                        "decreasing: the factor at index " +
                                        std::to_string(i) + " (" + std::to_string(d) +
                                        ") is not below its predecessor (" +
                                        std::to_string(discount_curve[i - 1]) + ")");
    }
    if (sigma_path.empty())
        throw std::invalid_argument(std::string(prefix) + ": sigma_path must not be empty");
    for (std::size_t i = 0; i < sigma_path.size(); ++i) {
        if (!(sigma_path[i] >= 0.0))
            throw std::invalid_argument(std::string(prefix) + ": sigma_path value at index " +
                                        std::to_string(i) + " must be non-negative, got " +
                                        std::to_string(sigma_path[i]));
    }
    if (!(dt > 0.0))
        throw std::invalid_argument(std::string(prefix) + ": dt must be strictly positive");
}

/**
 * @brief The time-varying volatility at a level, held flat past the end
 * of the input path (the theta_path convention).
 */
double sigma_at(const std::vector<double>& sigma_path, std::size_t level) {
    return level < sigma_path.size() ? sigma_path[level] : sigma_path.back();
}

/**
 * @brief Forward one tick of the state-price recursion.
 *
 * A state price q_j at the current level moves to the next level's nodes
 * j and j+1 with probability 1/2 each, discounted by one tick of node
 * j's rate:
 *
 *     q_{i+1,j} = 1/2 * q_{i,j} * e^{-r_{i,j}*dt} + 1/2 * q_{i,j-1} * e^{-r_{i,j-1}*dt}
 *
 * The returned vector has one entry more than the input: every node of a
 * binomial level has exactly two children, so the level sizes grow by
 * one per tick.
 */
std::vector<double> propagate_state_prices_one_tick(const std::vector<double>& state_prices,
                                                    const std::vector<math::rate_tree_node>& level,
                                                    double dt) {
    std::vector<double> next(state_prices.size() + 1, 0.0);
    for (std::size_t j = 0; j < state_prices.size(); ++j) {
        const double discounted = state_prices[j] * std::exp(-std::exp(level[j].log_rate) * dt);
        next[j] += 0.5 * discounted;
        next[j + 1] += 0.5 * discounted;
    }
    return next;
}

/**
 * @brief Find the shift a such that the level prices the next bond.
 *
 * Solves f(a) = sum_j q_j * exp(-exp(a + j*spacing)*dt) = target by
 * monotone bisection. f is continuous and strictly decreasing in a; the
 * bracket is guaranteed to exist: f(-inf) = sum_j q_j (the level's total
 * state price, i.e. the discount factor the previous level was
 * calibrated against -- strictly above target for a strictly decreasing
 * curve) and f(+inf) = 0 (strictly below target, which is positive).
 * The guard below rejects the only input that breaks this bracket: a
 * curve that is flat (or inverted) in floating point.
 */
double calibrate_level(const std::vector<double>& state_prices,
                       double spacing,
                       double target,
                       double dt,
                       double guess) {
    const double price_sum = std::accumulate(state_prices.begin(), state_prices.end(), 0.0);
    if (!(price_sum > target))
        throw std::invalid_argument("black_derman_toy_process: cannot calibrate a tree level: the "
                                    "level's state prices sum (" +
                                    std::to_string(price_sum) +
                                    ") does not exceed the target discount factor (" +
                                    std::to_string(target) +
                                    "); the input curve is not strictly "
                                    "decreasing in floating point");

    const auto f = [&](double a) {
        double sum = 0.0;
        for (std::size_t j = 0; j < state_prices.size(); ++j)
            sum += state_prices[j] * std::exp(-std::exp(a + j * spacing) * dt);
        return sum;
    };

    // Expand a bracket around the guess until it straddles the target;
    // the asymptotes above guarantee the expansion terminates. The
    // doubling keeps the walk logarithmic even in the pathological case
    // of a root orders of magnitude away from the guess.
    double lo, hi;
    if (f(guess) > target) {
        lo = guess;
        double step = 1.0;
        hi = guess + step;
        while (f(hi) > target) {
            step *= 2.0;
            hi = guess + step;
        }
    } else {
        hi = guess;
        double step = 1.0;
        lo = guess - step;
        while (!(f(lo) > target)) {
            step *= 2.0;
            lo = guess - step;
        }
    }

    // Monotone bisection; 80 iterations drive any reachable bracket far
    // below ulp scale (2^-80 of the bracket width).
    for (int iteration = 0; iteration < 80; ++iteration) {
        const double mid = 0.5 * (lo + hi);
        if (f(mid) > target)
            lo = mid;
        else
            hi = mid;
    }
    return 0.5 * (lo + hi);
}

} // namespace

math::rate_tree build_black_derman_toy_tree(const std::vector<double>& discount_curve,
                                            const std::vector<double>& sigma_path,
                                            double dt) {
    validate_curve_inputs(discount_curve, sigma_path, dt, "build_black_derman_toy_tree");

    const std::size_t num_levels = discount_curve.size();
    math::rate_tree tree;
    tree.levels.resize(num_levels);

    // State prices at the current level; level 0 holds a single unit at
    // the root. The root's shift is known in closed form (one node):
    // exp(-exp(a_0)*dt) = D(t_1).
    std::vector<double> state_prices{1.0};
    double guess = std::log(-std::log(discount_curve[0]) / dt);

    for (std::size_t i = 0; i < num_levels; ++i) {
        const double spacing = sigma_at(sigma_path, i) * std::sqrt(dt);
        const double shift = calibrate_level(state_prices, spacing, discount_curve[i], dt, guess);
        guess = shift;

        auto& level = tree.levels[i];
        level.resize(i + 1);
        for (std::size_t j = 0; j <= i; ++j) {
            math::rate_tree_node& node = level[j];
            node.log_rate = shift + j * spacing;
            if (i + 1 < num_levels) {
                node.child_indices = {j, j + 1};
                node.child_probabilities = {0.5, 0.5};
            }
        }
        if (i + 1 < num_levels)
            state_prices = propagate_state_prices_one_tick(state_prices, level, dt);
    }
    return tree;
}

black_derman_toy_process::black_derman_toy_process(std::vector<double> discount_curve,
                                                   std::vector<double> sigma_path,
                                                   std::uint32_t seed,
                                                   double dt)
    : dt_(dt)
    , rng_(seed) {

    // The constructor rejects invalid input with its own prefix before
    // the builder runs (the builder's own checks can then never fire).
    validate_curve_inputs(discount_curve, sigma_path, dt_, "black_derman_toy_process");
    tree_ = build_black_derman_toy_tree(discount_curve, sigma_path, dt_);

    // The flat-extension ladder: the last fitted level's shift and
    // spacing. The spacing is sigma*sqrt(dt) by construction -- read off
    // the tree when the last level has two nodes to compare against, and
    // taken from the path directly for a single-level tree.
    const auto& last_level = tree_.levels.back();
    a_last_ = last_level[0].log_rate;
    b_last_ = (last_level.size() > 1) ? last_level[1].log_rate - last_level[0].log_rate :
                                        sigma_at(sigma_path, 0) * std::sqrt(dt_);
}

black_derman_toy_process::black_derman_toy_process(const black_derman_toy_params& params,
                                                   std::uint32_t seed,
                                                   double dt)
    : black_derman_toy_process(params.discount_curve, params.sigma_path, seed, dt) {}

double black_derman_toy_process::next() {
    ++tick_;
    if (node_.level + 1 < tree_.step_count()) {
        // Fitted region: walk along the stored branches.
        node_ = math::next_tree_node(tree_, node_, rng_);
    } else {
        // Flat region: the last fitted ladder (a_last, b_last) continues
        // with the same 1/2, 1/2 branching -- the seam is seamless.
        node_.level = node_.level + 1;
        if (uniform_(rng_) > 0.5)
            ++node_.index;
    }
    return current();
}

double black_derman_toy_process::current() const {
    return std::exp(log_rate_at(node_));
}

double black_derman_toy_process::log_rate_at(const math::tree_node& node) const {
    if (node.level < tree_.levels.size())
        return tree_.levels[node.level][node.index].log_rate;
    return a_last_ + node.index * b_last_;
}

double black_derman_toy_process::discount_factor(std::size_t ticks_ahead) const {
    if (ticks_ahead == 0)
        return 1.0;

    // The stored tree prices any horizon that stays inside the fitted
    // region (the last fitted level is a landing point, not a departure
    // point -- the generic rate_tree contract).
    const std::size_t last_fitted = tree_.levels.size() - 1;
    if (node_.level + ticks_ahead <= last_fitted) {
        const auto prices = math::propagate_state_prices(tree_, node_, ticks_ahead, dt_);
        double price = 0.0;
        for (const double p : prices)
            price += p;
        return price;
    }

    // A horizon past the last fitted level: materialise a local extended
    // tree covering exactly the requested ticks -- the fitted levels the
    // walk still passes, then the flat ladder (a_last, b_last). Node
    // (k, j) of the extension is the absolute node (node_.level + k,
    // node_.index + j): the binomial branching keeps the walk node's
    // index as the offset of every later level, and the start node is
    // the extension's level 0. The branching is the same 1/2, 1/2
    // everywhere, so the extension is seamless and unbounded.
    math::rate_tree extended;
    extended.levels.resize(ticks_ahead + 1);
    for (std::size_t k = 0; k <= ticks_ahead; ++k) {
        const std::size_t absolute_level = node_.level + k;
        auto& level = extended.levels[k];
        level.resize(k + 1);
        for (std::size_t j = 0; j <= k; ++j) {
            math::rate_tree_node& n = level[j];
            n.log_rate = (absolute_level < last_fitted) ?
                             tree_.levels[absolute_level][node_.index + j].log_rate :
                             a_last_ + (node_.index + j) * b_last_;
            if (k < ticks_ahead) {
                n.child_indices = {j, j + 1};
                n.child_probabilities = {0.5, 0.5};
            }
        }
    }
    const auto prices =
        math::propagate_state_prices(extended, math::tree_node{0, 0}, ticks_ahead, dt_);
    double price = 0.0;
    for (const double p : prices)
        price += p;
    return price;
}

} // namespace ores::analytics::quant::service
