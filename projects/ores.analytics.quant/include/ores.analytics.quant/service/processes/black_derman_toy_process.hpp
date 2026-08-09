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
#ifndef ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_BLACK_DERMAN_TOY_PROCESS_HPP
#define ORES_ANALYTICS_QUANT_SERVICE_PROCESSES_BLACK_DERMAN_TOY_PROCESS_HPP

#include "ores.analytics.quant/domain/i_yield_curve_process.hpp"
#include "ores.analytics.quant/export.hpp"
#include "ores.analytics.quant/math/rate_tree.hpp"
#include "ores.analytics.quant/service/processes/black_derman_toy_params.hpp"
#include <cstddef>
#include <cstdint>
#include <random>
#include <vector>

namespace ores::analytics::quant::service {

/**
 * @brief Build the curve-fitted Black-Derman-Toy binomial lattice.
 *
 * The lattice models ln r on a recombining binomial tree: level i has
 * i+1 nodes and node (i, j), j in [0, i], carries
 *
 *     ln r = a_i + j * b_i
 *
 * with the spacing b_i = sigma_i * sqrt(dt) set by the time-varying
 * volatility (sigma_i is sigma_path[i], held flat once the path runs
 * out) and each shift a_i calibrated so the tree reproduces the input
 * discount curve exactly. The branching is the standard 1/2, 1/2 pair
 * (i, j) -> (i+1, j), (i+1, j+1).
 *
 * Calibration is by forward state-price induction, Hull's construction
 * ("Options, Futures and Other Derivatives", ch. 30). A state price
 * q_{i,j} is the value today of one unit of currency delivered at node
 * (i, j); q_{0,0} = 1 and the recursion
 *
 *     q_{i+1,j} = 1/2 * q_{i,j} * e^{-r_{i,j}*dt} + 1/2 * q_{i,j-1} * e^{-r_{i,j-1}*dt}
 *
 * shows the state prices at level i+1 depend only on levels 0..i -- no
 * backward induction is ever needed. The bond maturing at t_{i+1}
 * prices through the level-i rates,
 *
 *     D(t_{i+1}) = sum_j q_{i,j} * e^{-exp(a_i + j*b_i)*dt},
 *
 * and the left-hand side is the input curve, so a_i is the unique root
 * of a strictly monotone one-dimensional equation; the root always
 * exists (the sum at a -> -inf is D(t_i) > D(t_{i+1}), at a -> +inf it
 * is 0) and is found by bisection with no bracketing bound. Each level
 * is calibrated in turn, then its state prices propagate one tick.
 *
 * The last stored level (level discount_curve.size() - 1) is terminal:
 * its rates are fitted but nothing branches from it -- the tree's
 * contract in the generic rate_tree.
 *
 * @param discount_curve Input discount factors D(t_i), i = 1..M: one
 *                       factor per tick, strictly decreasing, each in
 *                       (0, 1). The fitted tree has M levels.
 * @param sigma_path     Time-varying volatility of ln r, one value per
 *                       tick, held flat past the end of the path; must
 *                       be non-empty and non-negative.
 * @param dt             Year fraction per tick; must be strictly
 *                       positive.
 * @return The fitted lattice, ready for the rate_tree walk and
 *         state-price propagation.
 */
ORES_ANALYTICS_QUANT_EXPORT ores::analytics::quant::math::rate_tree
build_black_derman_toy_tree(const std::vector<double>& discount_curve,
                            const std::vector<double>& sigma_path,
                            double dt);

/**
 * @brief Black-Derman-Toy short-rate process: a log-normal lattice
 * with time-varying volatility, calibrated to an input discount curve.
 *
 * The model lives on a recombining binomial tree of ln r -- node (i, j)
 * carries ln r = a_i + j*b_i with b_i = sigma_i*sqrt(dt) -- whose shifts
 * a_i are fitted so the tree reproduces the input discount curve exactly
 * (see build_black_derman_toy_tree for the calibration). Unlike
 * Black-Karasinski or Hull-White there is no stochastic differential
 * formulation beyond the lattice: the tree is the model, built once at
 * construction and shared by every call. This is also how the original
 * BDT model is defined and calibrated in the literature (Hull, ch. 30);
 * it was removed from modern QuantLib, so there is no reference
 * implementation to cross-check against.
 *
 * next() walks the tree with the risk-neutral probabilities 1/2: inside
 * the fitted region through the generic rate_tree walk, and beyond the
 * last fitted level along the *flat continuation* of the last fitted
 * ladder (a, b) -- the same convention as hull_white_process's
 * theta_path, which is held flat once it runs out. The flat region
 * keeps the identical branching and spacing, so the walk is seamless
 * across the boundary and the rate ladder never stops.
 *
 * discount_factor() prices a zero-coupon bond by propagating state
 * prices forward from the current node through the lattice and summing
 * them (the rate_tree utility). Within the fitted region the stored
 * tree is used directly; a horizon past the last fitted level
 * materialises a local extended tree -- the fitted levels followed by
 * the flat ladder -- covering exactly the requested number of ticks.
 * There is no bound on ticks_ahead: the extension is unlimited, and the
 * flat ladder extends the input curve's own asymptotic discounting.
 *
 * dt is the year-fraction one tick represents (default 1.0 -- one tick
 * per year, i.e. the class's unscaled behaviour); sigma stays in its
 * natural annualised units always -- callers never pre-scale it for a
 * finer tick granularity, they pass the real dt instead.
 */
class ORES_ANALYTICS_QUANT_EXPORT black_derman_toy_process final
    : public ores::analytics::quant::domain::IYieldCurveProcess {
public:
    black_derman_toy_process(std::vector<double> discount_curve,
                             std::vector<double> sigma_path,
                             std::uint32_t seed = 42,
                             double dt = 1.0);

    /**
     * @brief Construct from the strongly-typed parameter struct.
     *
     * The row-based parameter architecture (ores.synthetic) stores
     * parameters as {name, value} pairs; the mapping layer materialises
     * this struct from those rows and constructs the process through this
     * overload.
     */
    explicit black_derman_toy_process(const black_derman_toy_params& params,
                                      std::uint32_t seed = 42,
                                      double dt = 1.0);

    double next() override;
    double current() const override;
    double discount_factor(std::size_t ticks_ahead) const override;

private:
    /// The log rate carried by a walk position: a fitted node's stored
    /// log rate inside the tree, the flat ladder's value beyond it.
    double log_rate_at(const ores::analytics::quant::math::tree_node& node) const;

    double dt_;
    std::size_t tick_ = 0;
    ores::analytics::quant::math::tree_node node_{0, 0};
    ores::analytics::quant::math::rate_tree tree_;
    double a_last_;  ///< Flat-extension ladder: shift of the last fitted level.
    double b_last_;  ///< Flat-extension ladder: spacing of the last fitted level.
    std::mt19937 rng_;
    std::uniform_real_distribution<double> uniform_{0.0, 1.0};
};

} // namespace ores::analytics::quant::service

#endif
