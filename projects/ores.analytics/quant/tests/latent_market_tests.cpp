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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.analytics.quant/domain/latent_market.hpp"
#include "ores.analytics.quant/math/correlation_loadings.hpp"
#include <Eigen/Core>
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <numeric>
#include <stdexcept>
#include <vector>

using ores::analytics::quant::domain::factor_weight;
using ores::analytics::quant::domain::latent_add_factor;
using ores::analytics::quant::domain::latent_add_series;
using ores::analytics::quant::domain::latent_advance;
using ores::analytics::quant::domain::latent_driver_value;
using ores::analytics::quant::domain::latent_market;
using ores::analytics::quant::domain::latent_set_loadings;
using ores::analytics::quant::domain::latent_tick;
using ores::analytics::quant::domain::make_latent_market;
using ores::analytics::quant::math::keyed_path_finest_step;
using ores::analytics::quant::math::keyed_path_value_at;
using ores::analytics::quant::math::make_keyed_path;
using ores::analytics::quant::math::correlation_loadings;

namespace {

constexpr double latent_hour = 3600.0;
constexpr unsigned latent_depth = 10;

/// A block of an anchor and one member, correlated by rho, as the design builds
/// it: the anchor is the first factor, the member adds the second.
latent_market make_latent_block(std::uint64_t seed, double rho) {
    latent_market market = make_latent_market(seed, latent_hour, latent_depth, 0.0);
    latent_add_factor(market, 1);
    latent_add_factor(market, 2);

    latent_add_series(market);
    const std::vector<factor_weight> anchor_loadings{{.factor = 0, .weight = 1.0}};
    latent_set_loadings(market, 0, anchor_loadings);

    latent_add_series(market);
    const std::vector<factor_weight> member_loadings{
        {.factor = 0, .weight = rho}, {.factor = 1, .weight = std::sqrt(1.0 - rho * rho)}};
    latent_set_loadings(market, 1, member_loadings);
    return market;
}

struct latent_interval {
    double start;
    double end;
    double delta;
};

double latent_covariance_of(const std::vector<double>& a, const std::vector<double>& b) {
    const double mean_a = std::accumulate(a.begin(), a.end(), 0.0) / static_cast<double>(a.size());
    const double mean_b = std::accumulate(b.begin(), b.end(), 0.0) / static_cast<double>(b.size());
    double sum = 0.0;
    for (std::size_t i = 0; i < a.size(); ++i)
        sum += (a[i] - mean_a) * (b[i] - mean_b);
    return sum / static_cast<double>(a.size() - 1);
}

/// Hayashi-Yoshida covariance: the sum of products over the pairs of intervals
/// that overlap. The estimator the design note names, because tick-to-tick
/// returns understate a correlation that holds exactly.
double latent_hy_covariance(const std::vector<latent_interval>& a,
                            const std::vector<latent_interval>& b) {
    double sum = 0.0;
    for (const latent_interval& x : a) {
        for (const latent_interval& y : b) {
            if (x.start < y.end && y.start < x.end)
                sum += x.delta * y.delta;
        }
    }
    return sum;
}

}

TEST_CASE("the loadings of a correlation put the anchor first", "[latent_market]") {
    Eigen::MatrixXd correlation(2, 2);
    correlation << 1.0, 0.6, 0.6, 1.0;
    const Eigen::MatrixXd loadings = correlation_loadings(correlation, "test");
    CHECK(loadings(0, 0) == Catch::Approx(1.0));
    CHECK(loadings(0, 1) == Catch::Approx(0.0));
    CHECK(loadings(1, 0) == Catch::Approx(0.6));
    CHECK(loadings(1, 1) == Catch::Approx(0.8));

    // L * L^T is the stated matrix, and each row has unit norm.
    const Eigen::MatrixXd realised = loadings * loadings.transpose();
    for (Eigen::Index i = 0; i < 2; ++i) {
        CHECK(realised(i, i) == Catch::Approx(1.0));
        for (Eigen::Index j = 0; j < 2; ++j)
            CHECK(realised(i, j) == Catch::Approx(correlation(i, j)).margin(1e-12));
    }
}

TEST_CASE("a set of numbers that is not a correlation is refused", "[latent_market]") {
    Eigen::MatrixXd diagonal(2, 2);
    diagonal << 1.0, 0.6, 0.6, 1.5;
    CHECK_THROWS_AS(correlation_loadings(diagonal, "test"), std::invalid_argument);

    Eigen::MatrixXd asymmetric(2, 2);
    asymmetric << 1.0, 0.6, 0.5, 1.0;
    CHECK_THROWS_AS(correlation_loadings(asymmetric, "test"), std::invalid_argument);

    Eigen::MatrixXd outside(2, 2);
    outside << 1.0, 1.2, 1.2, 1.0;
    CHECK_THROWS_AS(correlation_loadings(outside, "test"), std::invalid_argument);

    // A symmetric matrix with a unit diagonal that is still not positive
    // definite: it is refused rather than projected onto the nearest one.
    Eigen::MatrixXd indefinite(3, 3);
    indefinite << 1.0, 0.9, 0.9, 0.9, 1.0, -0.9, 0.9, -0.9, 1.0;
    CHECK_THROWS_AS(correlation_loadings(indefinite, "test"), std::invalid_argument);
}

TEST_CASE("two drivers on coincident intervals correlate by their loadings", "[latent_market]") {
    constexpr double rho = 0.6;
    constexpr int runs = 400;
    constexpr int ticks = 50;
    const double dt = latent_hour / static_cast<double>(ticks);

    std::vector<double> anchor_increments;
    std::vector<double> member_increments;
    anchor_increments.reserve(runs * ticks);
    member_increments.reserve(runs * ticks);
    for (int r = 0; r < runs; ++r) {
        latent_market market = make_latent_block(static_cast<std::uint64_t>(100 + r), rho);
        for (int i = 1; i <= ticks; ++i) {
            const double t = static_cast<double>(i) * dt;
            anchor_increments.push_back(latent_tick(market, 0, t).shared);
            member_increments.push_back(latent_tick(market, 1, t).shared);
        }
    }

    const double anchor_variance = latent_covariance_of(anchor_increments, anchor_increments);
    const double member_variance = latent_covariance_of(member_increments, member_increments);
    const double correlation = latent_covariance_of(anchor_increments, member_increments) /
                               std::sqrt(anchor_variance * member_variance);

    const double tolerance =
        4.0 * (1.0 - rho * rho) / std::sqrt(static_cast<double>(anchor_increments.size()));
    CHECK(std::abs(correlation - rho) < std::max(0.02, tolerance));
}

TEST_CASE("the Hayashi-Yoshida estimator recovers it under irregular sampling", "[latent_market]") {
    constexpr double rho = 0.6;
    constexpr int runs = 200;
    constexpr int ticks = 60;
    const double dt = latent_hour / static_cast<double>(ticks);

    double covariance = 0.0;
    double anchor_variation = 0.0;
    double member_variation = 0.0;
    for (int r = 0; r < runs; ++r) {
        latent_market market = make_latent_block(static_cast<std::uint64_t>(9000 + r), rho);
        // Independent, irregular tick times, merged in time order as a scheduler
        // fires them, so no feed is ever read in the past.
        std::vector<double> anchor_times;
        std::vector<double> member_times;
        anchor_times.reserve(ticks);
        member_times.reserve(ticks);
        double at = 0.0;
        double mt = 0.0;
        for (int i = 0; i < ticks; ++i) {
            at += dt * (0.5 + 1.5 * static_cast<double>((r * 7 + i * 3) % 10) / 10.0);
            mt += dt * (0.5 + 1.5 * static_cast<double>((r * 5 + i * 7) % 10) / 10.0);
            anchor_times.push_back(at);
            member_times.push_back(mt);
        }

        std::vector<latent_interval> anchor;
        std::vector<latent_interval> member;
        anchor.reserve(ticks);
        member.reserve(ticks);
        double anchor_time = 0.0;
        double member_time = 0.0;
        std::size_t ia = 0;
        std::size_t im = 0;
        while (ia < anchor_times.size() || im < member_times.size()) {
            const bool anchor_due =
                ia < anchor_times.size() &&
                (im >= member_times.size() || anchor_times[ia] <= member_times[im]);
            if (anchor_due) {
                const double t = anchor_times[ia++];
                const auto increment = latent_tick(market, 0, t);
                anchor.push_back({.start = anchor_time, .end = t, .delta = increment.shared});
                anchor_time = t;
            } else {
                const double t = member_times[im++];
                const auto increment = latent_tick(market, 1, t);
                member.push_back({.start = member_time, .end = t, .delta = increment.shared});
                member_time = t;
            }
        }
        covariance += latent_hy_covariance(anchor, member);
        anchor_variation += latent_hy_covariance(anchor, anchor);
        member_variation += latent_hy_covariance(member, member);
    }

    const double correlation = covariance / std::sqrt(anchor_variation * member_variation);
    CHECK(std::abs(correlation - rho) < 0.08);
}

TEST_CASE("a series is untouched by another series' ticks", "[latent_market]") {
    constexpr double rho = 0.6;
    const double dt = latent_hour / 40.0;

    std::vector<double> alone;
    {
        latent_market market = make_latent_block(555, rho);
        for (int i = 1; i <= 40; ++i)
            alone.push_back(latent_tick(market, 0, static_cast<double>(i) * dt).shared);
    }

    // A third series ticks between every tick of the anchor, a burst of its own.
    std::vector<double> shared;
    {
        latent_market market = make_latent_block(555, rho);
        const std::uint32_t burst = latent_add_series(market);
        const std::vector<factor_weight> burst_loadings{{.factor = 1, .weight = 1.0}};
        latent_set_loadings(market, burst, burst_loadings);
        for (int i = 1; i <= 40; ++i) {
            const double t = static_cast<double>(i) * dt;
            static_cast<void>(latent_tick(market, burst, t - 0.5 * dt));
            shared.push_back(latent_tick(market, 0, t).shared);
            static_cast<void>(latent_tick(market, burst, t));
        }
    }

    CHECK(shared == alone);
}

TEST_CASE("a factor no series loads on changes nothing", "[latent_market]") {
    const double dt = latent_hour / 20.0;
    std::vector<double> without;
    std::vector<double> with;
    for (int pass = 0; pass < 2; ++pass) {
        latent_market market = make_latent_block(313, 0.6);
        if (pass == 1) {
            for (std::uint32_t k = 0; k < 200; ++k)
                latent_add_factor(market, 10'000 + k);
        }
        for (int i = 1; i <= 20; ++i) {
            const double value = latent_tick(market, 0, static_cast<double>(i) * dt).shared;
            (pass == 0 ? without : with).push_back(value);
        }
    }
    CHECK(with == without);
}

TEST_CASE("an event costs its own factors, not every factor in the market",
          "[.benchmark][latent_market]") {
    constexpr int events = 20000;
    for (const std::uint32_t factor_count : {2u, 2000u}) {
        latent_market market = make_latent_market(4242, latent_hour, latent_depth, 0.0);
        for (std::uint32_t k = 0; k < factor_count; ++k)
            latent_add_factor(market, static_cast<std::uint64_t>(k) + 1);
        latent_add_series(market);
        const std::vector<factor_weight> loadings{{.factor = 0, .weight = 0.6},
                                                  {.factor = 1, .weight = 0.8}};
        latent_set_loadings(market, 0, loadings);

        // A day a millisecond of market, so every event lands in a new finest
        // step and the bridge is walked in full: the honest cold-tick cost.
        const double dt = 100.0 * latent_hour / static_cast<double>(events);
        double sink = 0.0;
        const auto started = std::chrono::steady_clock::now();
        for (int i = 1; i <= events; ++i)
            sink += latent_tick(market, 0, static_cast<double>(i) * dt).shared;
        const auto ns = std::chrono::duration_cast<std::chrono::nanoseconds>(
                            std::chrono::steady_clock::now() - started)
                            .count() /
                        static_cast<long long>(events);
        std::printf("factors %u: %lld ns an event (sink %g)\n",
                    factor_count,
                    static_cast<long long>(ns),
                    sink);
    }
}

TEST_CASE("a market is reproducible by seed", "[latent_market]") {
    const double dt = latent_hour / 20.0;
    std::vector<double> first;
    std::vector<double> second;
    for (int pass = 0; pass < 2; ++pass) {
        latent_market market = make_latent_block(777, 0.4);
        for (int i = 1; i <= 20; ++i) {
            const double value = latent_tick(market, 1, static_cast<double>(i) * dt).shared;
            (pass == 0 ? first : second).push_back(value);
        }
    }
    CHECK(first == second);
}

TEST_CASE("a driver refuses loadings that are not its own", "[latent_market]") {
    latent_market market = make_latent_block(888, 0.5);

    // The squared weights must fit inside one of variance.
    const std::uint32_t greedy = latent_add_series(market);
    const std::vector<factor_weight> too_much{{.factor = 0, .weight = 0.9},
                                              {.factor = 1, .weight = 0.9}};
    CHECK_THROWS_AS(latent_set_loadings(market, greedy, too_much), std::invalid_argument);

    const std::uint32_t repeated = latent_add_series(market);
    const std::vector<factor_weight> twice{{.factor = 0, .weight = 0.5},
                                           {.factor = 0, .weight = 0.5}};
    CHECK_THROWS_AS(latent_set_loadings(market, repeated, twice), std::invalid_argument);

    const std::uint32_t out_of_range = latent_add_series(market);
    const std::vector<factor_weight> missing{{.factor = 9, .weight = 0.5}};
    CHECK_THROWS_AS(latent_set_loadings(market, out_of_range, missing), std::invalid_argument);

    // Loadings belong to the series most recently added, so the flat array is
    // built in one pass and never moved.
    const std::vector<factor_weight> late{{.factor = 0, .weight = 0.5}};
    CHECK_THROWS_AS(latent_set_loadings(market, 0, late), std::invalid_argument);
}

TEST_CASE("a tick with no time behind it carries no increment", "[latent_market]") {
    // The scheduler clamps a late tick to a clock that has not moved, so two ticks
    // can share an instant. No time means no noise: the increment is zero and the
    // caller skips the tick rather than dividing by the square root of nothing.
    latent_market market = make_latent_block(2024, 0.5);
    static_cast<void>(latent_tick(market, 0, 0.1 * latent_hour));
    const auto again = latent_tick(market, 0, 0.1 * latent_hour);
    CHECK(again.elapsed == 0.0);
    CHECK(again.shared == 0.0);
}

TEST_CASE("a burst inside one finest step interpolates between its two nodes", "[latent_market]") {
    // A Hawkes burst can fire many ticks inside one finest step. The path is then
    // linear there: the increments add up to the step's own increment, and their
    // squares are not the elapsed time. This pins the bound the design note states,
    // rather than pretending the burst carries noise the resolution cannot hold.
    const math::keyed_path_params path = make_keyed_path(
        math::stream_key{.run_seed = 7, .series = 1, .purpose = math::draw_purpose::market},
        latent_hour,
        latent_depth,
        0.0);
    const double step = math::keyed_path_finest_step(path);
    const double node = 0.5 * latent_hour;
    const double full =
        math::keyed_path_value_at(path, node + step) - math::keyed_path_value_at(path, node);

    double total = 0.0;
    double squares = 0.0;
    double previous = math::keyed_path_value_at(path, node);
    constexpr int ticks = 8;
    for (int i = 1; i <= ticks; ++i) {
        const double t = node + step * static_cast<double>(i) / static_cast<double>(ticks);
        const double value = math::keyed_path_value_at(path, t);
        const double increment = value - previous;
        total += increment;
        squares += increment * increment;
        previous = value;
    }

    CHECK(total == Catch::Approx(full).margin(1e-12));
    // The step's quadratic variation is lost: eight equal parts carry an eighth
    // of it, so the burst reads as smooth rather than Brownian. This is the
    // resolution bound the design note states, and the reason the depth has to
    // sit below the shortest gap an arrival law can produce.
    CHECK(squares < 0.2 * full * full);
    CHECK(squares > 0.0);
}

TEST_CASE("a first tick measures from the run's start", "[latent_market]") {
    latent_market market = make_latent_block(999, 0.5);
    latent_advance(market, 0.25 * latent_hour);
    CHECK(latent_driver_value(market, 0) != 0.0);

    const auto increment = latent_tick(market, 0, 0.25 * latent_hour);
    CHECK(increment.elapsed == Catch::Approx(0.25 * latent_hour));

    // The driver value the tick records is the value the market already held.
    CHECK(latent_driver_value(market, 0) == increment.shared);

    // Time never runs backwards.
    CHECK_THROWS_AS(latent_tick(market, 0, 0.1 * latent_hour), std::invalid_argument);
    CHECK_THROWS_AS(latent_advance(market, 0.1 * latent_hour), std::invalid_argument);
}
