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
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <random>
#include <stdexcept>

using ores::analytics::quant::math::next_tree_node;
using ores::analytics::quant::math::propagate_state_prices;
using ores::analytics::quant::math::rate_tree;
using ores::analytics::quant::math::rate_tree_node;
using ores::analytics::quant::math::tree_node;

namespace {

/**
 * @brief A hand-built recombining binomial tree: level i has i+1 nodes,
 * node j branches to (j, j+1) with probability 1/2 each, and node (i,j)
 * carries the log-rate a_i + j*b_i.
 */
rate_tree build_binomial_tree(std::size_t levels, double a, double b) {
    rate_tree tree;
    tree.levels.resize(levels);
    for (std::size_t i = 0; i < levels; ++i) {
        tree.levels[i].resize(i + 1);
        for (std::size_t j = 0; j <= i; ++j) {
            tree.levels[i][j].log_rate = a + static_cast<double>(j) * b;
            tree.levels[i][j].child_indices = {j, j + 1};
            tree.levels[i][j].child_probabilities = {0.5, 0.5};
        }
    }
    return tree;
}

/**
 * @brief A hand-built recombining trinomial tree: level i has 2i+1 nodes,
 * node j branches to (j, j+1, j+2) with probabilities (1/6, 2/3, 1/6).
 */
rate_tree build_trinomial_tree(std::size_t levels) {
    rate_tree tree;
    tree.levels.resize(levels);
    for (std::size_t i = 0; i < levels; ++i) {
        tree.levels[i].resize(2 * i + 1);
        for (std::size_t j = 0; j < tree.levels[i].size(); ++j) {
            tree.levels[i][j].log_rate = 0.0;
            tree.levels[i][j].child_indices = {j, j + 1, j + 2};
            tree.levels[i][j].child_probabilities = {1.0 / 6.0, 2.0 / 3.0, 1.0 / 6.0};
        }
    }
    return tree;
}

/**
 * @brief True if one of the node's branches targets `index` at the next
 * level.
 */
bool branches_to(const rate_tree_node& node, std::size_t index) {
    for (const std::size_t child : node.child_indices)
        if (child == index)
            return true;
    return false;
}

} // anonymous namespace

TEST_CASE("rate_tree next_tree_node walks only along stored branches", "[rate_tree]") {
    // Every step must land on a stored node of the next level and move
    // along one of the current node's branches -- the recombining
    // property, exercised statistically over many draws.
    const rate_tree tree = build_binomial_tree(6, std::log(0.04), 0.05);
    std::mt19937 rng(7);
    tree_node node{0, 0};
    for (int i = 0; i < 2000; ++i) {
        if (node.level + 1 >= tree.step_count()) {
            node = {0, 0}; // restart for the next walk
            continue;
        }
        const auto& previous = tree.levels[node.level][node.index];
        node = next_tree_node(tree, node, rng);
        CHECK(node.index < tree.levels[node.level].size());
        CHECK(branches_to(previous, node.index));
    }
}

TEST_CASE("rate_tree next_tree_node walks a trinomial the same way", "[rate_tree]") {
    const rate_tree tree = build_trinomial_tree(4);
    std::mt19937 rng(11);
    tree_node node{0, 0};
    for (int i = 0; i < 2000; ++i) {
        if (node.level + 1 >= tree.step_count()) {
            node = {0, 0};
            continue;
        }
        const auto& previous = tree.levels[node.level][node.index];
        node = next_tree_node(tree, node, rng);
        CHECK(node.index < tree.levels[node.level].size());
        CHECK(branches_to(previous, node.index));
    }
}

TEST_CASE("rate_tree propagation without discounting conserves probability",
          "[rate_tree]") {
    // With dt == 0 the state prices are plain path probabilities: their
    // sum at any level must be exactly 1. Five ticks from the root land
    // on the last of the six stored levels.
    const rate_tree tree = build_binomial_tree(6, std::log(0.04), 0.05);
    const auto prices = propagate_state_prices(tree, {0, 0}, 5, 0.0);
    double sum = 0.0;
    for (const double p : prices)
        sum += p;
    CHECK(sum == Catch::Approx(1.0).epsilon(1e-12));
}

TEST_CASE("rate_tree propagation from a mid-tree node conserves probability",
          "[rate_tree]") {
    // The propagation must work from any stored node, not just the root:
    // a unit state price at a mid-tree node spreads to the nodes
    // reachable from it.
    const rate_tree tree = build_binomial_tree(6, std::log(0.04), 0.05);
    const auto prices = propagate_state_prices(tree, {2, 1}, 3, 0.0);
    double sum = 0.0;
    for (const double p : prices)
        sum += p;
    CHECK(sum == Catch::Approx(1.0).epsilon(1e-12));
}

TEST_CASE("rate_tree one-tick bond price is exp(-r*dt) from any node",
          "[rate_tree]") {
    // A single propagation step discounts the unit state price by the
    // node's own rate regardless of the branching probabilities -- the
    // tree's bond price over one tick is exactly the deposit factor.
    const rate_tree tree = build_binomial_tree(4, std::log(0.05), 0.04);
    for (const auto& node : {tree_node{1, 1}, tree_node{2, 1}}) {
        const auto prices = propagate_state_prices(tree, node, 1, 1.0);
        double price = 0.0;
        for (const double p : prices)
            price += p;
        const double r = std::exp(tree.levels[node.level][node.index].log_rate);
        CHECK(price == Catch::Approx(std::exp(-r)).epsilon(1e-12));
    }
}

TEST_CASE("rate_tree multi-step propagation matches hand-computed state prices",
          "[rate_tree]") {
    // Two-tick binomial: from the root, the state price at each level-2
    // node is the path probability times the path discount factors.
    const double r0 = 0.05, r1_down = 0.04, r1_up = 0.06;
    rate_tree tree;
    tree.levels.resize(3);
    tree.levels[0].resize(1);
    tree.levels[0][0] = {std::log(r0), {0, 1}, {0.5, 0.5}};
    tree.levels[1].resize(2);
    tree.levels[1][0] = {std::log(r1_down), {0, 1}, {0.5, 0.5}};
    tree.levels[1][1] = {std::log(r1_up), {1, 2}, {0.5, 0.5}};
    tree.levels[2].resize(3);

    const auto prices = propagate_state_prices(tree, {0, 0}, 2, 1.0);
    const double d0 = std::exp(-r0);
    const double dd = std::exp(-r1_down);
    const double du = std::exp(-r1_up);
    REQUIRE(prices.size() == 3);
    CHECK(prices[0] == Catch::Approx(0.25 * d0 * dd).epsilon(1e-12));
    CHECK(prices[1] == Catch::Approx(0.25 * d0 * (dd + du)).epsilon(1e-12));
    CHECK(prices[2] == Catch::Approx(0.25 * d0 * du).epsilon(1e-12));
    CHECK(prices[0] + prices[1] + prices[2] ==
          Catch::Approx(d0 * (0.5 * dd + 0.5 * du)).epsilon(1e-12));
}

TEST_CASE("rate_tree propagation with zero steps returns the unit state price",
          "[rate_tree]") {
    const rate_tree tree = build_binomial_tree(3, std::log(0.04), 0.05);
    const auto prices = propagate_state_prices(tree, {2, 1}, 0, 1.0);
    REQUIRE(prices.size() == 3);
    CHECK(prices[1] == Catch::Approx(1.0).epsilon(1e-12));
    CHECK(prices[0] == 0.0);
    CHECK(prices[2] == 0.0);
}

TEST_CASE("rate_tree rejects operations on an empty tree", "[rate_tree]") {
    const rate_tree tree;
    std::mt19937 rng(1);
    CHECK_THROWS_AS(next_tree_node(tree, {0, 0}, rng), std::invalid_argument);
    CHECK_THROWS_AS(propagate_state_prices(tree, {0, 0}, 1, 1.0), std::invalid_argument);
}

TEST_CASE("rate_tree rejects out-of-range nodes", "[rate_tree]") {
    const rate_tree tree = build_binomial_tree(3, std::log(0.04), 0.05);
    std::mt19937 rng(1);
    CHECK_THROWS_AS(next_tree_node(tree, {3, 0}, rng), std::invalid_argument);
    CHECK_THROWS_AS(next_tree_node(tree, {1, 2}, rng), std::invalid_argument);
    CHECK_THROWS_AS(propagate_state_prices(tree, {3, 0}, 1, 1.0), std::invalid_argument);
}

TEST_CASE("rate_tree rejects walking from the last level of the tree",
          "[rate_tree]") {
    const rate_tree tree = build_binomial_tree(2, std::log(0.04), 0.05);
    std::mt19937 rng(1);
    CHECK_THROWS_AS(next_tree_node(tree, {1, 0}, rng), std::invalid_argument);
}

TEST_CASE("rate_tree rejects propagation beyond the stored levels",
          "[rate_tree]") {
    const rate_tree tree = build_binomial_tree(3, std::log(0.04), 0.05);
    // From the root, 2 ticks reach the last stored level (valid); 3 ticks
    // would need a level that is not stored.
    CHECK(propagate_state_prices(tree, {0, 0}, 2, 1.0).size() == 3);
    CHECK_THROWS_AS(propagate_state_prices(tree, {0, 0}, 3, 1.0), std::invalid_argument);
}

TEST_CASE("rate_tree rejects negative dt and malformed branches", "[rate_tree]") {
    const rate_tree tree = build_binomial_tree(3, std::log(0.04), 0.05);
    CHECK_THROWS_AS(propagate_state_prices(tree, {0, 0}, 1, -1.0), std::invalid_argument);

    rate_tree broken = build_binomial_tree(2, std::log(0.04), 0.05);
    broken.levels[0][0].child_probabilities = {0.5};
    std::mt19937 rng(1);
    CHECK_THROWS_AS(next_tree_node(broken, {0, 0}, rng), std::invalid_argument);

    rate_tree out_of_range = build_binomial_tree(2, std::log(0.04), 0.05);
    out_of_range.levels[0][0].child_indices = {1, 7};
    CHECK_THROWS_AS(next_tree_node(out_of_range, {0, 0}, rng), std::invalid_argument);
    CHECK_THROWS_AS(propagate_state_prices(out_of_range, {0, 0}, 1, 1.0), std::invalid_argument);
}
