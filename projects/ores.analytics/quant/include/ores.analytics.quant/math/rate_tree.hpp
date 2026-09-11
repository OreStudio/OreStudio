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
#ifndef ORES_ANALYTICS_QUANT_MATH_RATE_TREE_HPP
#define ORES_ANALYTICS_QUANT_MATH_RATE_TREE_HPP

#include "ores.analytics.quant/export.hpp"
#include <cstddef>
#include <random>
#include <vector>

namespace ores::analytics::quant::math {

/**
 * @brief A position within a rate_tree: the level (one per simulation
 * tick) and the node index within that level.
 */
struct tree_node {
    std::size_t level = 0;
    std::size_t index = 0;
};

/**
 * @brief One node of a rate_tree: the log short rate it carries and the
 * branches leaving it.
 *
 * A branch is a (target index, probability) pair: child_indices[c] is the
 * node index at the next level the process moves to with probability
 * child_probabilities[c]. A binomial node has two branches, a trinomial
 * node three; the tree itself never needs to know which.
 */
struct rate_tree_node {
    double log_rate = 0.0;
    std::vector<std::size_t> child_indices;
    std::vector<double> child_probabilities;
};

/**
 * @brief A generic risk-neutral lattice of short rates.
 *
 * levels[level] holds the nodes of one simulation tick, in a recombining
 * tree: each node's branches target nodes of the next level, so any path
 * the walk can take lands on a stored node. The structure is deliberately
 * model-free -- the processes build it (a curve-calibrated binomial for
 * black_derman_toy_process, an OU-consistent trinomial for
 * black_karasinski_process) and the operations below only read it. The
 * values are log short rates, not rates: the exponential is applied where
 * discounting needs the rate itself.
 *
 * There is deliberately no flat-extension, clamping, or boundary logic in
 * this type. The operations validate against the stored levels and leave
 * "what happens beyond the last level" to the process that built the
 * tree. A future backward-induction pass (derivative pricing on the
 * lattice) would slot in here alongside the forward pass below.
 */
struct rate_tree {
    /// levels[level][index] -- level i has as many nodes as the builder
    /// chose for it; the walk only ever moves along stored branches.
    std::vector<std::vector<rate_tree_node>> levels;

    /// Number of stored levels (each one simulation tick).
    std::size_t step_count() const {
        return levels.size();
    }
};

/**
 * @brief Simulate one step of the random walk on the tree.
 *
 * Draws a uniform deviate and moves along the current node's branches
 * according to their probabilities (a cumulative-distribution draw, so
 * the branching works for any number of branches).
 *
 * @param tree The lattice to walk.
 * @param node The current position; must be a stored node.
 * @param rng  The caller's random engine; the tree itself owns no state.
 * @return The node one level down the chosen branch.
 */
ORES_ANALYTICS_QUANT_EXPORT tree_node next_tree_node(const rate_tree& tree,
                                                     const tree_node& node,
                                                     std::mt19937& rng);

/**
 * @brief Forward risk-neutral state-price propagation.
 *
 * Starts with a unit state price at @p start and propagates it through
 * @p steps levels, discounting by one tick of the short rate at every
 * node it passes: a state price at node j of the next level accumulates
 * prices[j] * p(j -> j') * exp(-r(j) * dt). The returned vector holds the
 * state prices at the level reached after @p steps ticks; the
 * zero-coupon bond price from @p start over that horizon is their sum
 * (a state price at maturity is the price of one unit of currency
 * delivered there).
 *
 * @param tree  The lattice; must store enough levels for the whole walk,
 *              i.e. start.level + steps < step_count() (the last stored
 *              level is a terminal landing point, never a departure
 *              point). The processes build trees covering exactly the
 *              horizon they price, so this bound is never an artificial
 *              restriction.
 * @param start The node the propagation begins at.
 * @param steps Number of ticks to propagate; 0 returns the unit state
 *              price at @p start.
 * @param dt    Year fraction per tick, used to discount exp(-r*dt) at
 *              each node; must be non-negative (0 discounts nothing,
 *              which reduces the state prices to plain probabilities).
 * @return The state-price vector at level start.level + steps.
 */
ORES_ANALYTICS_QUANT_EXPORT std::vector<double>
propagate_state_prices(const rate_tree& tree, const tree_node& start, std::size_t steps, double dt);

} // namespace ores::analytics::quant::math

#endif
