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
#include "ores.analytics.quant/math/rate_tree.hpp"
#include <cmath>
#include <stdexcept>
#include <string>
#include <utility>

namespace ores::analytics::quant::math {

namespace {

/**
 * @brief Reject nodes that are not stored in the tree.
 *
 * The walk and the propagation both begin from a caller-supplied
 * position; an out-of-range position is a construction or caller error,
 * so it fails loudly rather than reading past the stored levels.
 */
void validate_node(const rate_tree& tree, const tree_node& node) {
    if (tree.levels.empty())
        throw std::invalid_argument("rate_tree: cannot operate on an empty tree");
    if (node.level >= tree.levels.size())
        throw std::invalid_argument("rate_tree: node level out of range");
    if (node.index >= tree.levels[node.level].size())
        throw std::invalid_argument("rate_tree: node index out of range");
}

/**
 * @brief Reject malformed branches: a departing node must have at least
 * one branch, the branch lists must agree in length, every target must
 * exist at the next level and the probabilities must form a
 * distribution. Both the walk and the propagation depart from nodes, so
 * both reject a malformed node the same way.
 */
void validate_branching(const rate_tree_node& node, std::size_t next_level_size) {
    if (node.child_indices.empty())
        throw std::invalid_argument("rate_tree: node has no child branches");
    if (node.child_indices.size() != node.child_probabilities.size())
        throw std::invalid_argument("rate_tree: child index/probability count mismatch");
    double probability_sum = 0.0;
    for (std::size_t c = 0; c < node.child_indices.size(); ++c) {
        if (node.child_indices[c] >= next_level_size)
            throw std::invalid_argument("rate_tree: child index out of range");
        if (!(node.child_probabilities[c] >= 0.0))
            throw std::invalid_argument("rate_tree: child probability must be non-negative");
        probability_sum += node.child_probabilities[c];
    }
    // The probabilities must sum to one; the tolerance absorbs the
    // rounding of the builders' moment-matching formulas (a trinomial
    // like (1+e^2/v-3e/b)/6 + (2-e^2/v)/3 + (1+e^2/v+3e/b)/6 is 1 only
    // up to machine precision).
    if (!(std::abs(probability_sum - 1.0) <= 1e-10 * std::max(1.0, probability_sum)))
        throw std::invalid_argument("rate_tree: child probabilities must sum to one, got " +
                                    std::to_string(probability_sum));
}

} // anonymous namespace

tree_node next_tree_node(const rate_tree& tree, const tree_node& node, std::mt19937& rng) {
    validate_node(tree, node);
    if (node.level + 1 >= tree.step_count())
        throw std::invalid_argument("rate_tree: cannot walk from the last level of the tree");
    const auto& current = tree.levels[node.level][node.index];
    validate_branching(current, tree.levels[node.level + 1].size());

    std::uniform_real_distribution<double> uniform(0.0, 1.0);
    const double u = uniform(rng);

    // Cumulative-distribution draw: pick the branch whose cumulative
    // probability first reaches u. The last branch is the default so a
    // u within a whisker of 1 can never fall through the loop.
    std::size_t chosen = current.child_probabilities.size() - 1;
    double cumulative = 0.0;
    for (std::size_t c = 0; c < current.child_probabilities.size(); ++c) {
        cumulative += current.child_probabilities[c];
        if (u <= cumulative) {
            chosen = c;
            break;
        }
    }
    return tree_node{node.level + 1, current.child_indices[chosen]};
}

std::vector<double> propagate_state_prices(const rate_tree& tree,
                                           const tree_node& start,
                                           std::size_t steps,
                                           double dt) {
    validate_node(tree, start);
    // After `steps` ticks the propagation sits on level start.level +
    // steps, which must be a stored level; the last stored level is
    // terminal (nothing can walk from it) but is a valid landing point.
    if (start.level + steps >= tree.step_count())
        throw std::invalid_argument(
            "rate_tree: propagation exceeds the tree's levels; build a tree covering the "
            "full horizon");
    if (dt < 0.0)
        throw std::invalid_argument("rate_tree: dt must be non-negative");

    std::vector<double> prices(tree.levels[start.level].size(), 0.0);
    prices[start.index] = 1.0;

    for (std::size_t step = 0; step < steps; ++step) {
        const auto& level = tree.levels[start.level + step];
        const auto& next_level = tree.levels[start.level + step + 1];
        std::vector<double> next(next_level.size(), 0.0);
        for (std::size_t j = 0; j < level.size(); ++j) {
            if (prices[j] == 0.0)
                continue;
            validate_branching(level[j], next_level.size());
            const double discount = std::exp(-std::exp(level[j].log_rate) * dt);
            const auto& children = level[j].child_indices;
            const auto& probabilities = level[j].child_probabilities;
            for (std::size_t c = 0; c < children.size(); ++c)
                next[children[c]] += prices[j] * probabilities[c] * discount;
        }
        prices = std::move(next);
    }
    return prices;
}

} // namespace ores::analytics::quant::math
