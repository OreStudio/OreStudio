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
#include "ores.analytics.quant/service/processes/black_derman_toy_params.hpp"
#include "ores.analytics.quant/service/processes/black_derman_toy_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <limits>
#include <numeric>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>

using ores::analytics::quant::math::next_tree_node;
using ores::analytics::quant::math::propagate_state_prices;
using ores::analytics::quant::math::rate_tree;
using ores::analytics::quant::math::tree_node;
using ores::analytics::quant::service::black_derman_toy_params;
using ores::analytics::quant::service::black_derman_toy_process;
using ores::analytics::quant::service::build_black_derman_toy_tree;

TEST_CASE("black_derman_toy_process: zero sigma reproduces the input curve exactly",
          "[black_derman_toy_process]") {
    // D(t_i) = exp(-0.04 * i): one level per tick.
    const std::vector<double> curve = {0.9607894391523232,
                                       0.9231163463866358,
                                       0.8869204367151574,
                                       0.8521437889662113,
                                       0.8187307530779818,
                                       0.7866278610665535,
                                       0.7557837414687275,
                                       0.7261490370736908};
    black_derman_toy_process p(curve, {0.0}, 42, 1.0);
    for (std::size_t n = 1; n <= curve.size(); ++n)
        REQUIRE(p.discount_factor(n) == Catch::Approx(curve[n - 1]).epsilon(1e-9));
}

TEST_CASE("black_derman_toy_process: zero sigma deterministic path", "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.96, 0.92, 0.88, 0.84, 0.80};
    black_derman_toy_process p(curve, {0.0}, 42, 1.0);
    const auto tree = build_black_derman_toy_tree(curve, {0.0}, 1.0);

    // With sigma = 0 every node of a level carries the same rate: the
    // per-tick discount is D(t_{k+1}) / D(t_k), so
    // r_k = ln(D(t_k) / D(t_{k+1})) with D(t_0) = 1.
    for (std::size_t k = 0; k < curve.size(); ++k) {
        const double expected_rate =
            (k == 0) ? -std::log(curve[0]) : std::log(curve[k - 1] / curve[k]);
        const double expected_log_rate = std::log(expected_rate);
        for (std::size_t j = 0; j <= k; ++j)
            REQUIRE(tree.levels[k][j].log_rate == Catch::Approx(expected_log_rate).epsilon(1e-10));
        REQUIRE(p.current() == Catch::Approx(expected_rate).epsilon(1e-10));
        if (k + 1 < curve.size())
            p.next();
    }
}

TEST_CASE("black_derman_toy_process: positive sigma reproduces the input curve exactly",
          "[black_derman_toy_process]") {
    // dt = 0.5 ticks: the curve values are D at 0.5-year spacings and
    // the reproduction must hold regardless of the tick size.
    const std::vector<double> curve = {0.9801986733,
                                       0.9607894392,
                                       0.9417645336,
                                       0.9231163464,
                                       0.9048374180,
                                       0.8869204367,
                                       0.8693582354,
                                       0.8521437890};
    const std::vector<double> sigma_path = {0.1, 0.2, 0.3, 0.15, 0.25, 0.2, 0.1, 0.3};
    black_derman_toy_process p(curve, sigma_path, 42, 0.5);
    for (std::size_t n = 1; n <= curve.size(); ++n)
        REQUIRE(p.discount_factor(n) == Catch::Approx(curve[n - 1]).epsilon(1e-9));
}

TEST_CASE("black_derman_toy_process: one-tick discount from a walked state",
          "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.98, 0.95, 0.91, 0.86, 0.80, 0.74};
    black_derman_toy_process p(curve, {0.2, 0.3, 0.1, 0.25, 0.15, 0.2}, 7, 1.0);
    for (std::size_t i = 0; i < 3; ++i)
        p.next();
    // A one-tick bond prices exactly through the current node's rate:
    // the two half-probability branches sum to one.
    REQUIRE(p.discount_factor(1) == Catch::Approx(std::exp(-p.current())).epsilon(1e-12));

    // The same identity must hold once the walk passes the last fitted
    // level (level 5): the local extended tree starts at the walked
    // node, wherever the walk left it.
    for (std::size_t i = 0; i < 5; ++i)
        p.next();
    REQUIRE(p.discount_factor(1) == Catch::Approx(std::exp(-p.current())).epsilon(1e-12));
}

TEST_CASE("black_derman_toy_process: discounts are monotone across the fitted and flat regions",
          "[black_derman_toy_process]") {
    // Six fitted levels, then the flat ladder; every rate is positive so
    // every per-tick discount is below one and the product decreases.
    const std::vector<double> curve = {0.98, 0.95, 0.91, 0.86, 0.80, 0.74};
    black_derman_toy_process p(curve, {0.2}, 42, 1.0);
    double previous = 1.0;
    for (std::size_t n = 1; n <= 12; ++n) {
        const double d = p.discount_factor(n);
        REQUIRE(d > 0.0);
        REQUIRE(d < previous);
        previous = d;
    }
}

TEST_CASE("black_derman_toy_process: level spacing is sigma times sqrt of dt",
          "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.98, 0.95, 0.91, 0.86, 0.80};
    const auto tree_one = build_black_derman_toy_tree(curve, {0.2, 0.3}, 1.0);
    REQUIRE(tree_one.levels[0].size() == 1);
    REQUIRE(tree_one.levels[1].size() == 2);
    // Level 1's spacing is set by the time-varying path's second value.
    REQUIRE(tree_one.levels[1][1].log_rate - tree_one.levels[1][0].log_rate ==
            Catch::Approx(0.3).epsilon(1e-15));
    const auto tree_quarter = build_black_derman_toy_tree(curve, {0.2}, 0.25);
    REQUIRE(tree_quarter.levels[1][1].log_rate - tree_quarter.levels[1][0].log_rate ==
            Catch::Approx(0.2 * std::sqrt(0.25)).epsilon(1e-15));
}

TEST_CASE("black_derman_toy_process: flat extension reproduces the extended curve",
          "[black_derman_toy_process]") {
    // curve[2] / curve[1] = 0.9, so the last fitted ladder discounts
    // exactly 0.9 per tick when extended flat (sigma = 0).
    const std::vector<double> curve = {0.99, 0.95, 0.855};
    black_derman_toy_process p(curve, {0.0}, 42, 1.0);
    REQUIRE(p.discount_factor(3) == Catch::Approx(0.855).epsilon(1e-9));
    REQUIRE(p.discount_factor(4) == Catch::Approx(0.855 * 0.9).epsilon(1e-9));
    // Walk to the last fitted level and extend flat from there.
    p.next();
    p.next();
    REQUIRE(p.discount_factor(5) == Catch::Approx(std::pow(0.9, 5)).epsilon(1e-9));
}

TEST_CASE("black_derman_toy_process: local extended tree matches a hand-built extension",
          "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.97, 0.93, 0.88, 0.82, 0.75, 0.67};
    const double dt = 1.0;
    black_derman_toy_process p(curve, {0.2}, 42, dt);
    const auto tree = build_black_derman_toy_tree(curve, {0.2}, dt);

    // Rebuild the extension by hand: the flat ladder (a_last, b_last)
    // read off the last fitted level of the builder's tree.
    const auto& last = tree.levels.back();
    const double a_last = last[0].log_rate;
    const double b_last = last[1].log_rate - last[0].log_rate;
    const std::size_t last_fitted = tree.levels.size() - 1;
    const std::size_t ticks = 10;

    rate_tree extended;
    extended.levels.resize(ticks + 1);
    for (std::size_t k = 0; k <= ticks; ++k) {
        auto& level = extended.levels[k];
        level.resize(k + 1);
        for (std::size_t j = 0; j <= k; ++j) {
            auto& n = level[j];
            n.log_rate = (k < last_fitted) ? tree.levels[k][j].log_rate : a_last + j * b_last;
            if (k < ticks) {
                n.child_indices = {j, j + 1};
                n.child_probabilities = {0.5, 0.5};
            }
        }
    }
    const auto prices = propagate_state_prices(extended, tree_node{0, 0}, ticks, dt);
    const double hand = std::accumulate(prices.begin(), prices.end(), 0.0);
    REQUIRE(p.discount_factor(ticks) == Catch::Approx(hand).epsilon(1e-12));
}

TEST_CASE("black_derman_toy_process: extended tree honours the walked node",
          "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.97, 0.93, 0.88, 0.82, 0.75, 0.67};
    const std::vector<double> sigma_path = {0.2, 0.3, 0.1, 0.25, 0.15, 0.2};
    const double dt = 1.0;
    const auto tree = build_black_derman_toy_tree(curve, sigma_path, dt);
    const auto& last = tree.levels.back();
    const double a_last = last[0].log_rate;
    const double b_last = last[1].log_rate - last[0].log_rate;

    black_derman_toy_process p(curve, sigma_path, 42, dt);
    // Replicate the process's own walk with an independently seeded
    // engine: same seed and same draws, so the walked node's index is
    // known to the test. A wrong extension that ignored the index would
    // price a different node and fail the comparison below.
    std::mt19937 rng(42);
    tree_node node{0, 0};
    for (std::size_t i = 0; i < 4; ++i) {
        p.next();
        node = next_tree_node(tree, node, rng);
    }
    REQUIRE(node.level == 4);

    // Three fitted ticks, then four flat ones.
    const std::size_t ticks = 7;
    const std::size_t last_fitted = tree.levels.size() - 1;
    rate_tree extended;
    extended.levels.resize(ticks + 1);
    for (std::size_t k = 0; k <= ticks; ++k) {
        const std::size_t absolute_level = node.level + k;
        auto& level = extended.levels[k];
        level.resize(k + 1);
        for (std::size_t j = 0; j <= k; ++j) {
            auto& n = level[j];
            n.log_rate = (absolute_level < last_fitted) ?
                             tree.levels[absolute_level][node.index + j].log_rate :
                             a_last + (node.index + j) * b_last;
            if (k < ticks) {
                n.child_indices = {j, j + 1};
                n.child_probabilities = {0.5, 0.5};
            }
        }
    }
    const auto prices = propagate_state_prices(extended, tree_node{0, 0}, ticks, dt);
    const double hand = std::accumulate(prices.begin(), prices.end(), 0.0);
    REQUIRE(p.discount_factor(ticks) == Catch::Approx(hand).epsilon(1e-12));
}

TEST_CASE("black_derman_toy_process: deep flat extension stays finite and monotone",
          "[black_derman_toy_process]") {
    // A gentle curve and small sigma keep the flat-ladder rates small,
    // so five hundred flat ticks still leave a representable price.
    const std::vector<double> curve = {0.99501, 0.99005, 0.98511, 0.98020, 0.97531, 0.97045};
    black_derman_toy_process p(curve, {0.01}, 42, 1.0);
    const double d100 = p.discount_factor(100);
    const double d300 = p.discount_factor(300);
    const double d500 = p.discount_factor(500);
    REQUIRE(std::isfinite(d500));
    REQUIRE(d500 > 0.0);
    REQUIRE(d500 < d300);
    REQUIRE(d300 < d100);
    REQUIRE(d100 < 1.0);
}

TEST_CASE("black_derman_toy_process: seed determinism and distinct seeds differ",
          "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.98, 0.95, 0.91, 0.86, 0.80, 0.74};
    const std::vector<double> sigma_path = {0.2, 0.3, 0.1, 0.25, 0.15, 0.2};
    black_derman_toy_process a(curve, sigma_path, 42, 1.0);
    black_derman_toy_process b(curve, sigma_path, 42, 1.0);
    for (std::size_t i = 0; i < 4; ++i) {
        REQUIRE(a.next() == b.next());
        REQUIRE(a.discount_factor(3) == b.discount_factor(3));
    }
    black_derman_toy_process c(curve, sigma_path, 43, 1.0);
    bool differs = false;
    for (std::size_t i = 0; i < 4; ++i)
        differs = differs || (a.next() != c.next());
    REQUIRE(differs);
}

TEST_CASE("black_derman_toy_process: params struct equivalence", "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.98, 0.95, 0.91, 0.86, 0.80, 0.74};
    const std::vector<double> sigma_path = {0.2, 0.3, 0.1, 0.25, 0.15, 0.2};
    black_derman_toy_process a(curve, sigma_path, 42, 0.5);
    black_derman_toy_process b(black_derman_toy_params{curve, sigma_path}, 42, 0.5);
    for (std::size_t i = 0; i < 4; ++i) {
        REQUIRE(a.next() == b.next());
        REQUIRE(a.discount_factor(4) == b.discount_factor(4));
    }
}

TEST_CASE("black_derman_toy_process: zero-tick discount is one", "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.98, 0.95, 0.91};
    black_derman_toy_process p(curve, {0.2}, 42, 1.0);
    REQUIRE(p.discount_factor(0) == 1.0);
    p.next();
    REQUIRE(p.discount_factor(0) == 1.0);
}

TEST_CASE("black_derman_toy_process: branching is the standard 1/2, 1/2 binomial",
          "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.98, 0.95, 0.91, 0.86, 0.80};
    const auto tree = build_black_derman_toy_tree(curve, {0.2, 0.3, 0.1, 0.25, 0.15}, 0.5);
    for (std::size_t i = 0; i < tree.levels.size(); ++i) {
        REQUIRE(tree.levels[i].size() == i + 1);
        for (std::size_t j = 0; j < tree.levels[i].size(); ++j) {
            const auto& node = tree.levels[i][j];
            if (i + 1 < tree.levels.size()) {
                REQUIRE(node.child_indices == std::vector<std::size_t>{j, j + 1});
                REQUIRE(node.child_probabilities.size() == 2);
                REQUIRE(node.child_probabilities[0] == Catch::Approx(0.5).epsilon(1e-15));
                REQUIRE(node.child_probabilities[1] == Catch::Approx(0.5).epsilon(1e-15));
            } else {
                // The last stored level is terminal: it is a landing
                // point, never a departure point.
                REQUIRE(node.child_indices.empty());
                REQUIRE(node.child_probabilities.empty());
            }
        }
    }
}

TEST_CASE("black_derman_toy_process: tree prices match Monte-Carlo path discounts",
          "[black_derman_toy_process]") {
    const std::vector<double> curve = {0.97, 0.93, 0.88, 0.82, 0.75, 0.67};
    const std::vector<double> sigma_path = {0.25, 0.2, 0.3, 0.15, 0.25, 0.2};
    const std::size_t ticks = 8; // six fitted ticks, then two flat ones
    black_derman_toy_process p(curve, sigma_path, 42, 1.0);
    const double tree_price = p.discount_factor(ticks);

    const std::size_t paths = 100000;
    double sum = 0.0;
    for (std::size_t path = 0; path < paths; ++path) {
        black_derman_toy_process q(curve, sigma_path, static_cast<std::uint32_t>(42 + path), 1.0);
        // The tree's propagation discounts the start level first, so the
        // path must too: current() at the walk node, then advance and
        // discount -- levels 0..ticks-1 on both sides.
        double path_discount = std::exp(-q.current());
        for (std::size_t t = 1; t < ticks; ++t) {
            q.next();
            path_discount *= std::exp(-q.current());
        }
        sum += path_discount;
    }
    const double mc_price = sum / static_cast<double>(paths);
    REQUIRE(mc_price == Catch::Approx(tree_price).epsilon(3e-3));
}

TEST_CASE("black_derman_toy_process: builder validation rejections", "[black_derman_toy_process]") {
    const std::vector<double> good_curve = {0.98, 0.95, 0.91};
    const std::vector<double> good_sigma = {0.2};
    REQUIRE_NOTHROW(build_black_derman_toy_tree(good_curve, good_sigma, 1.0));

    // Curve: empty, outside (0, 1), flat, increasing, NaN.
    REQUIRE_THROWS_AS(build_black_derman_toy_tree({}, good_sigma, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree({1.0}, good_sigma, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree({0.0}, good_sigma, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree({-0.5}, good_sigma, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree({0.98, 0.98}, good_sigma, 1.0),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree({0.95, 0.98}, good_sigma, 1.0),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree(
                          {0.98, std::numeric_limits<double>::quiet_NaN()}, good_sigma, 1.0),
                      std::invalid_argument);
    // Sigma path: empty, negative, NaN.
    REQUIRE_THROWS_AS(build_black_derman_toy_tree(good_curve, {}, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree(good_curve, {-0.1}, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(
        build_black_derman_toy_tree(good_curve, {std::numeric_limits<double>::quiet_NaN()}, 1.0),
        std::invalid_argument);
    // dt: zero, negative, NaN.
    REQUIRE_THROWS_AS(build_black_derman_toy_tree(good_curve, good_sigma, 0.0),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree(good_curve, good_sigma, -1.0),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(build_black_derman_toy_tree(
                          good_curve, good_sigma, std::numeric_limits<double>::quiet_NaN()),
                      std::invalid_argument);

    // The message names the entry point that rejected the input.
    try {
        build_black_derman_toy_tree({}, good_sigma, 1.0);
        FAIL("expected an exception");
    } catch (const std::invalid_argument& e) {
        REQUIRE(std::string(e.what()).find("build_black_derman_toy_tree") != std::string::npos);
    }
}

TEST_CASE("black_derman_toy_process: constructor validation rejections",
          "[black_derman_toy_process]") {
    const std::vector<double> good_curve = {0.98, 0.95, 0.91};
    const std::vector<double> good_sigma = {0.2};
    REQUIRE_NOTHROW(black_derman_toy_process(good_curve, good_sigma, 42, 1.0));

    REQUIRE_THROWS_AS(black_derman_toy_process({}, good_sigma, 42, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(black_derman_toy_process({0.5, 0.5}, good_sigma, 42, 1.0),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(black_derman_toy_process({0.95, 0.98}, good_sigma, 42, 1.0),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(black_derman_toy_process(good_curve, {}, 42, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(black_derman_toy_process(good_curve, {-0.1}, 42, 1.0), std::invalid_argument);
    REQUIRE_THROWS_AS(black_derman_toy_process(good_curve, good_sigma, 42, 0.0),
                      std::invalid_argument);
    REQUIRE_THROWS_AS(black_derman_toy_process(good_curve, good_sigma, 42, -1.0),
                      std::invalid_argument);

    // The constructor rejects with its own prefix before the builder runs.
    try {
        black_derman_toy_process({}, good_sigma, 42, 1.0);
        FAIL("expected an exception");
    } catch (const std::invalid_argument& e) {
        REQUIRE(std::string(e.what()).find("black_derman_toy_process") != std::string::npos);
    }
}
