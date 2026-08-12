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
#include "ores.analytics.quant/service/processes/black_karasinski_params.hpp"
#include "ores.analytics.quant/service/processes/black_karasinski_process.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <random>
#include <stdexcept>
#include <utility>
#include <vector>

using ores::analytics::quant::math::next_tree_node;
using ores::analytics::quant::math::propagate_state_prices;
using ores::analytics::quant::math::tree_node;
using ores::analytics::quant::service::black_karasinski_params;
using ores::analytics::quant::service::black_karasinski_process;
using ores::analytics::quant::service::build_black_karasinski_tree;

namespace {

/**
 * @brief Welford's one-pass running mean and variance.
 */
struct running_moments {
    double mean = 0.0;
    double m2 = 0.0;
    std::size_t count = 0;

    void add(double x) {
        ++count;
        const double delta = x - mean;
        mean += delta / static_cast<double>(count);
        const double delta2 = x - mean;
        m2 += delta * delta2;
    }

    double sample_variance() const {
        return m2 / static_cast<double>(count - 1);
    }
};

} // anonymous namespace

TEST_CASE("black_karasinski_process reports the initial rate before any next() call",
          "[black_karasinski_process]") {
    // The state is ln r, so current() = exp(ln 0.05) is 0.05 to within one
    // ulp of the exp/log round trip, not bitwise.
    black_karasinski_process p(0.3, std::log(0.04), 0.2, 0.05);
    CHECK(p.current() == Catch::Approx(0.05).epsilon(1e-12));
}

TEST_CASE("black_karasinski_process next() updates and returns the same value as current()",
          "[black_karasinski_process]") {
    black_karasinski_process p(0.3, std::log(0.04), 0.2, 0.05);
    const double n = p.next();
    CHECK(n == p.current());
    CHECK(n > 0.0);
}

TEST_CASE("black_karasinski_process is deterministic for a fixed seed",
          "[black_karasinski_process]") {
    black_karasinski_process a(0.3, std::log(0.04), 0.2, 0.05, 7);
    black_karasinski_process b(0.3, std::log(0.04), 0.2, 0.05, 7);

    for (int i = 0; i < 20; ++i)
        CHECK(a.next() == b.next());
    CHECK(a.discount_factor(4) == b.discount_factor(4));
}

TEST_CASE("black_karasinski_process different seeds produce different paths",
          "[black_karasinski_process]") {
    black_karasinski_process a(0.3, std::log(0.04), 0.2, 0.05, 42);
    black_karasinski_process b(0.3, std::log(0.04), 0.2, 0.05, 43);
    CHECK(a.next() != b.next());
}

TEST_CASE("black_karasinski_process discount_factor(0) is exactly 1",
          "[black_karasinski_process]") {
    black_karasinski_process p(0.3, std::log(0.04), 0.2, 0.05);
    CHECK(p.discount_factor(0) == 1.0);
}

TEST_CASE("black_karasinski_process one-tick discount_factor is exp(-r*dt)",
          "[black_karasinski_process]") {
    // The lattice's one-tick bond price from the root depends only on the
    // root's own rate, regardless of branching or probabilities.
    black_karasinski_process p(0.3, std::log(0.04), 0.25, 0.04, 42, 0.5);
    CHECK(p.discount_factor(1) == Catch::Approx(std::exp(-0.04 * 0.5)).epsilon(1e-12));
}

TEST_CASE("black_karasinski_process one-tick discount_factor is exact in the driftless branch",
          "[black_karasinski_process]") {
    black_karasinski_process p(0.0, std::log(0.04), 0.25, 0.04, 42, 0.5);
    CHECK(p.discount_factor(1) == Catch::Approx(std::exp(-0.04 * 0.5)).epsilon(1e-12));
}

TEST_CASE("black_karasinski_process with zero volatility follows the deterministic path exactly",
          "[black_karasinski_process]") {
    const double kappa = 0.4, theta = std::log(0.04), r0 = 0.06, dt = 0.5;
    const double decay = std::exp(-kappa * dt);
    black_karasinski_process p(kappa, theta, 0.0, r0, 42, dt);

    // The bond price from the initial state is exp(-sum_i exp(m_i) * dt)
    // along the mean-reversion path.
    double x = std::log(r0);
    double sum = 0.0;
    for (std::size_t i = 0; i < 5; ++i) {
        sum += std::exp(x) * dt;
        x = theta + (x - theta) * decay;
    }
    CHECK(p.discount_factor(5) == Catch::Approx(std::exp(-sum)).epsilon(1e-12));

    // Each next() lands exactly on the next point of that path: advance
    // the reference before comparing, since the first next() already
    // leaves the initial rate behind.
    x = std::log(r0);
    for (int i = 0; i < 8; ++i) {
        x = theta + (x - theta) * decay;
        CHECK(p.next() == Catch::Approx(std::exp(x)).epsilon(1e-15));
    }

    // And the one-tick price from the advanced state is exp(-r * dt) with
    // the deterministic rate now reached.
    CHECK(p.discount_factor(1) == Catch::Approx(std::exp(-std::exp(x) * dt)).epsilon(1e-12));
}

TEST_CASE("black_karasinski_process zero-volatility closed form equals the zero-spacing lattice",
          "[black_karasinski_process]") {
    // build_black_karasinski_tree with sigma == 0 produces a degenerate
    // zero-spacing lattice (every node of a level at the deterministic
    // center); its state-price propagation must reproduce the closed-form
    // shortcut in discount_factor().
    const double kappa = 0.4, theta = std::log(0.04), r0 = 0.06, dt = 0.5;
    const auto tree = build_black_karasinski_tree(std::log(r0), kappa, theta, 0.0, dt, 5);
    const auto prices = propagate_state_prices(tree, tree_node{0, 0}, 5, dt);
    double lattice_price = 0.0;
    for (const double p : prices)
        lattice_price += p;

    black_karasinski_process p(kappa, theta, 0.0, r0, 42, dt);
    CHECK(p.discount_factor(5) == Catch::Approx(lattice_price).epsilon(1e-12));
}

TEST_CASE("black_karasinski_process ln r is a Gaussian OU with the exact moments",
          "[black_karasinski_process]") {
    // Statistical: after 1 and 2 ticks, the sample mean and variance of
    // ln r must match the exact OU formulas, E[x_i] = theta + (x0 - theta)
    // * e^{-kappa*i*dt} and Var(x_i) = sigma^2 (1 - e^{-2*kappa*i*dt}) /
    // (2*kappa), to well within the Monte Carlo error.
    const double kappa = 0.5, theta = std::log(0.04), sigma = 0.4, r0 = 0.05, dt = 0.25;
    const std::size_t paths = 200000;

    running_moments one_tick, two_ticks;
    for (std::size_t path = 0; path < paths; ++path) {
        black_karasinski_process p(
            kappa, theta, sigma, r0, 42 + static_cast<std::uint32_t>(path), dt);
        const double x1 = std::log(p.next());
        const double x2 = std::log(p.next());
        one_tick.add(x1);
        two_ticks.add(x2);
    }

    const double x0 = std::log(r0);
    const double e1 = std::exp(-kappa * dt);
    const double e2 = e1 * e1;
    const double expect_mean1 = theta + (x0 - theta) * e1;
    const double expect_var1 = sigma * sigma * (1.0 - e2) / (2.0 * kappa);
    const double expect_mean2 = theta + (x0 - theta) * e2;
    const double expect_var2 = sigma * sigma * (1.0 - e2 * e2) / (2.0 * kappa);

    CHECK(one_tick.mean == Catch::Approx(expect_mean1).epsilon(4e-3));
    CHECK(one_tick.sample_variance() == Catch::Approx(expect_var1).epsilon(1.5e-2));
    CHECK(two_ticks.mean == Catch::Approx(expect_mean2).epsilon(4e-3));
    CHECK(two_ticks.sample_variance() == Catch::Approx(expect_var2).epsilon(1.5e-2));
}

TEST_CASE("black_karasinski_process tree price agrees with Monte Carlo",
          "[black_karasinski_process]") {
    // The lattice matches the per-tick conditional mean and variance of
    // next()'s exact transition, so the state-price sum and the average
    // pathwise product of deposit factors must agree; third-moment
    // differences enter only at order (r*dt)^3 and are far below the
    // statistical noise here.
    const double kappa = 0.3, theta = std::log(0.04), sigma = 0.35, r0 = 0.05, dt = 0.1;
    const std::size_t ticks = 20;
    black_karasinski_process p(kappa, theta, sigma, r0, 42, dt);
    const double tree_price = p.discount_factor(ticks);

    const std::size_t paths = 100000;
    double sum = 0.0;
    for (std::size_t path = 0; path < paths; ++path) {
        black_karasinski_process m(
            kappa, theta, sigma, r0, 42 + static_cast<std::uint32_t>(path), dt);
        double product = 1.0;
        for (std::size_t i = 0; i < ticks; ++i)
            product *= std::exp(-m.next() * dt);
        sum += product;
    }
    const double mc_price = sum / static_cast<double>(paths);
    CHECK(mc_price == Catch::Approx(tree_price).epsilon(3e-3));
}

TEST_CASE("black_karasinski_process tree price converges as dt shrinks",
          "[black_karasinski_process]") {
    // Halving dt at a fixed horizon must move the lattice price toward
    // the continuous-time model; the gap between successive refinements
    // shrinks as dt -> 0.
    const double kappa = 0.3, theta = std::log(0.04), sigma = 0.35, r0 = 0.05;
    black_karasinski_process p20(kappa, theta, sigma, r0, 42, 0.1);
    black_karasinski_process p40(kappa, theta, sigma, r0, 42, 0.05);
    black_karasinski_process p80(kappa, theta, sigma, r0, 42, 0.025);
    const double price20 = p20.discount_factor(20);
    const double price40 = p40.discount_factor(40);
    const double price80 = p80.discount_factor(80);
    CHECK(std::abs(price40 - price20) < 1e-3);
    CHECK(std::abs(price80 - price40) < std::abs(price40 - price20));
    CHECK(price80 > 0.0);
}

TEST_CASE("black_karasinski_process with kappa <= 0 degenerates to a driftless random walk",
          "[black_karasinski_process]") {
    // kappa == 0: one tick of ln r has variance sigma^2 * dt (statistical),
    // and kappa < 0 behaves identically -- no anti-mean-reversion pull.
    const double sigma = 0.3, r0 = 0.05, dt = 0.5;
    const std::size_t paths = 200000;
    running_moments zero_kappa, negative_kappa;
    for (std::size_t path = 0; path < paths; ++path) {
        black_karasinski_process a(
            0.0, std::log(0.04), sigma, r0, 42 + static_cast<std::uint32_t>(path), dt);
        black_karasinski_process b(
            -0.2, std::log(0.04), sigma, r0, 42 + static_cast<std::uint32_t>(path), dt);
        zero_kappa.add(std::log(a.next()));
        negative_kappa.add(std::log(b.next()));
    }
    // 1.5e-2 is ~4.7 sigma of the estimator at 200000 paths (the same
    // headroom the Gaussian-OU moments test uses above); sub-sigma bounds
    // flake because std::normal_distribution draws differ per standard
    // library, so the same seeds produce different samples on macOS.
    CHECK(zero_kappa.sample_variance() == Catch::Approx(sigma * sigma * dt).epsilon(1.5e-2));
    CHECK(negative_kappa.sample_variance() == Catch::Approx(sigma * sigma * dt).epsilon(1.5e-2));

    black_karasinski_process p(-0.2, std::log(0.04), 0.3, 0.05, 42, dt);
    for (int i = 0; i < 100; ++i) {
        const double r = p.next();
        CHECK(r > 0.0);
        CHECK(std::isfinite(r));
    }
    const double df = p.discount_factor(10);
    CHECK(df > 0.0);
    CHECK(df < 1.0);
}

TEST_CASE("black_karasinski_process is dt-aware: variance scales with dt",
          "[black_karasinski_process]") {
    // Four ticks at dt = 0.25 must produce the same one-unit-of-time
    // variance as one tick at dt = 1.0: sigma^2.
    const double sigma = 0.3, r0 = 0.05;
    const std::size_t paths = 200000;
    running_moments fine, coarse;
    for (std::size_t path = 0; path < paths; ++path) {
        const std::uint32_t seed = 42 + static_cast<std::uint32_t>(path);
        black_karasinski_process a(0.0, std::log(0.04), sigma, r0, seed, 0.25);
        double x = std::log(r0);
        for (int i = 0; i < 4; ++i)
            x = std::log(a.next());
        fine.add(x);

        black_karasinski_process b(0.0, std::log(0.04), sigma, r0, seed, 1.0);
        coarse.add(std::log(b.next()));
    }
    CHECK(fine.sample_variance() == Catch::Approx(sigma * sigma).epsilon(1e-2));
    CHECK(coarse.sample_variance() == Catch::Approx(sigma * sigma).epsilon(1e-2));
}

TEST_CASE("black_karasinski_process keeps the rate positive over long runs",
          "[black_karasinski_process]") {
    black_karasinski_process p(0.3, std::log(0.05), 0.35, 0.04, 42, 0.1);
    for (int i = 0; i < 50000; ++i) {
        const double r = p.next();
        CHECK(r > 0.0);
        CHECK(std::isfinite(r));
    }
}

TEST_CASE("black_karasinski_process discount_factor decreases monotonically with maturity",
          "[black_karasinski_process]") {
    // With positive rates every extra tick multiplies every state price
    // by a deposit factor strictly below 1, so the price must fall.
    black_karasinski_process p(0.4, std::log(0.04), 0.2, 0.05, 42, 0.25);
    double prev = 1.0;
    for (std::size_t t = 1; t <= 15; ++t) {
        const double df = p.discount_factor(t);
        CHECK(df < prev);
        CHECK(df > 0.0);
        prev = df;
    }
}

TEST_CASE("black_karasinski_process rejects invalid parameters", "[black_karasinski_process]") {
    CHECK_THROWS_AS(black_karasinski_process(0.3, 0.0, -0.1, 0.05), std::invalid_argument);
    CHECK_THROWS_AS(black_karasinski_process(0.3, 0.0, 0.2, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(black_karasinski_process(0.3, 0.0, 0.2, -0.02), std::invalid_argument);
    CHECK_THROWS_AS(black_karasinski_process(0.3, 0.0, 0.2, 0.05, 42, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(black_karasinski_process(0.3, 0.0, 0.2, 0.05, 42, -0.5), std::invalid_argument);
}

TEST_CASE("black_karasinski_process params-struct constructor is equivalent to the scalar one",
          "[black_karasinski_process]") {
    black_karasinski_params params{0.3, std::log(0.04), 0.2, 0.05};
    black_karasinski_process a(params, 9, 0.5);
    black_karasinski_process b(0.3, std::log(0.04), 0.2, 0.05, 9, 0.5);
    for (int i = 0; i < 5; ++i)
        CHECK(a.next() == b.next());
    CHECK(a.discount_factor(4) == b.discount_factor(4));
}

TEST_CASE("build_black_karasinski_tree centres levels on the deterministic mean path",
          "[black_karasinski_process]") {
    const double kappa = 0.5, theta = std::log(0.04), sigma = 0.4, x0 = std::log(0.05), dt = 0.25;
    const auto tree = build_black_karasinski_tree(x0, kappa, theta, sigma, dt, 3);

    const double decay = std::exp(-kappa * dt);
    const double variance = sigma * sigma * (1.0 - decay * decay) / (2.0 * kappa);
    const double spacing = std::sqrt(3.0 * variance);
    const double m1 = theta + (x0 - theta) * decay;
    const double m2 = theta + (x0 - theta) * decay * decay;

    REQUIRE(tree.levels.size() == 4);
    REQUIRE(tree.levels[1].size() == 3);
    REQUIRE(tree.levels[2].size() == 5);
    // Node (i, j) carries m_i + (j - i) * b.
    CHECK(tree.levels[1][0].log_rate == Catch::Approx(m1 - spacing).epsilon(1e-12));
    CHECK(tree.levels[1][1].log_rate == Catch::Approx(m1).epsilon(1e-12));
    CHECK(tree.levels[1][2].log_rate == Catch::Approx(m1 + spacing).epsilon(1e-12));
    CHECK(tree.levels[2][2].log_rate == Catch::Approx(m2).epsilon(1e-12));
    CHECK(tree.levels[2][4].log_rate == Catch::Approx(m2 + 2.0 * spacing).epsilon(1e-12));
}

TEST_CASE("build_black_karasinski_tree root branches symmetrically with the exact probabilities",
          "[black_karasinski_process]") {
    // kappa*dt = ln(2) makes the decay exactly 1/2: the root's conditional
    // mean offset is 0, so temp = 0, e = 0 and the probabilities are the
    // classic (1/6, 2/3, 1/6).
    const double kappa = std::log(2.0), dt = 1.0;
    const auto tree =
        build_black_karasinski_tree(std::log(0.05), kappa, std::log(0.04), 0.3, dt, 2);
    REQUIRE(tree.levels[0][0].child_indices.size() == 3);
    CHECK(tree.levels[0][0].child_indices[0] == 0);
    CHECK(tree.levels[0][0].child_indices[1] == 1);
    CHECK(tree.levels[0][0].child_indices[2] == 2);
    CHECK(tree.levels[0][0].child_probabilities[0] == 1.0 / 6.0);
    CHECK(tree.levels[0][0].child_probabilities[1] == 2.0 / 3.0);
    CHECK(tree.levels[0][0].child_probabilities[2] == 1.0 / 6.0);
}

TEST_CASE("build_black_karasinski_tree matches mean and variance at a shifted node",
          "[black_karasinski_process]") {
    // Node (1, 2) has offset 1; with decay 1/2 the conditional mean offset
    // is 0.5, lround(0.5) = 1 (half away from zero), so e = -0.5 exactly
    // and the probabilities are the closed values below.
    const double kappa = std::log(2.0), dt = 1.0;
    const auto tree =
        build_black_karasinski_tree(std::log(0.05), kappa, std::log(0.04), 0.3, dt, 5);
    const auto& node = tree.levels[1][2];
    REQUIRE(node.child_indices.size() == 3);
    CHECK(node.child_indices[0] == 2);
    CHECK(node.child_indices[1] == 3);
    CHECK(node.child_indices[2] == 4);
    CHECK(node.child_probabilities[0] == Catch::Approx(13.0 / 24.0).epsilon(1e-12));
    CHECK(node.child_probabilities[1] == Catch::Approx(5.0 / 12.0).epsilon(1e-12));
    CHECK(node.child_probabilities[2] == Catch::Approx(1.0 / 24.0).epsilon(1e-12));

    // A node whose conditional mean lands on a grid point (offset -2 with
    // decay 1/2) is symmetric again; the decay is only 1/2 to within an
    // ulp, so the probabilities are 1/6, 2/3, 1/6 to within machine
    // precision.
    const auto& aligned = tree.levels[4][2];
    CHECK(aligned.child_probabilities[0] == Catch::Approx(1.0 / 6.0).epsilon(1e-12));
    CHECK(aligned.child_probabilities[1] == Catch::Approx(2.0 / 3.0).epsilon(1e-12));
    CHECK(aligned.child_probabilities[2] == Catch::Approx(1.0 / 6.0).epsilon(1e-12));
}

TEST_CASE("build_black_karasinski_tree is valid for any horizon", "[black_karasinski_process]") {
    // The failure mode of a drift-matched binomial: horizons with
    // kappa*T >> 1 push the probabilities outside [0, 1]. The centered
    // adaptive trinomial must keep every probability valid, every branch
    // target inside the next level, and every probability vector summing
    // to 1, for arbitrarily deep trees and for the driftless branch.
    const std::vector<std::pair<double, double>> cases = {
        {2.0, 0.8}, // kappa*dt = 2 -- far beyond a binomial's validity
        {0.0, 1.0}, // driftless branch
    };
    for (const auto& [kappa, sigma] : cases) {
        const auto tree =
            build_black_karasinski_tree(std::log(0.1), kappa, std::log(0.04), sigma, 1.0, 300);
        REQUIRE(tree.levels.size() == 301);
        for (std::size_t i = 0; i < 300; ++i) {
            const auto& next_level = tree.levels[i + 1];
            for (const auto& node : tree.levels[i]) {
                REQUIRE(node.child_indices.size() == 3);
                for (const std::size_t child : node.child_indices) {
                    CHECK(child < next_level.size());
                }
                const double p1 = node.child_probabilities[0];
                const double p2 = node.child_probabilities[1];
                const double p3 = node.child_probabilities[2];
                CHECK(p1 >= 0.0);
                CHECK(p2 >= 0.0);
                CHECK(p3 >= 0.0);
                CHECK(p1 <= 1.0);
                CHECK(p2 <= 1.0);
                CHECK(p3 <= 1.0);
                CHECK(p1 + p2 + p3 == Catch::Approx(1.0).epsilon(1e-12));
            }
        }
        CHECK(tree.levels[300][0].child_indices.empty()); // terminal level
    }
}

TEST_CASE("black_karasinski_tree walk stays on stored nodes", "[black_karasinski_process]") {
    const auto tree =
        build_black_karasinski_tree(std::log(0.05), 0.6, std::log(0.04), 0.3, 0.25, 20);
    std::mt19937 rng(3);
    tree_node node{0, 0};
    for (int i = 0; i < 2000; ++i) {
        if (node.level + 1 >= tree.step_count()) {
            node = {0, 0};
            continue;
        }
        node = next_tree_node(tree, node, rng);
        CHECK(node.level < tree.step_count());
        CHECK(node.index < tree.levels[node.level].size());
    }
}

TEST_CASE("build_black_karasinski_tree rejects invalid parameters", "[black_karasinski_process]") {
    CHECK_THROWS_AS(build_black_karasinski_tree(std::log(0.05), 0.3, 0.0, -0.1, 1.0, 2),
                    std::invalid_argument);
    CHECK_THROWS_AS(build_black_karasinski_tree(std::log(0.05), 0.3, 0.0, 0.2, 0.0, 2),
                    std::invalid_argument);
}
