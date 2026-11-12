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
#include "ores.analytics.quant/math/keyed_brownian_path.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <stdexcept>
#include <vector>

using ores::analytics::quant::math::draw_purpose;
using ores::analytics::quant::math::keyed_path_boundary_at;
using ores::analytics::quant::math::keyed_path_cell_increment;
using ores::analytics::quant::math::keyed_path_finest_step;
using ores::analytics::quant::math::keyed_path_params;
using ores::analytics::quant::math::keyed_path_span;
using ores::analytics::quant::math::keyed_path_value_at;
using ores::analytics::quant::math::keyed_walk;
using ores::analytics::quant::math::keyed_walk_value;
using ores::analytics::quant::math::make_keyed_path;
using ores::analytics::quant::math::make_keyed_walk;
using ores::analytics::quant::math::standard_normal_at;
using ores::analytics::quant::math::stream_key;

namespace {

constexpr double one_hour = 3600.0;
constexpr std::uint64_t run_seed = 20261112;

stream_key factor_key(std::uint64_t id) {
    return {.run_seed = run_seed, .series = id, .purpose = draw_purpose::market};
}

keyed_path_params make_path(std::uint64_t id, unsigned depth, double origin = 0.0) {
    return make_keyed_path(factor_key(id), one_hour, depth, origin);
}

std::vector<double> sample(unsigned depth, int count, double t) {
    std::vector<double> out;
    out.reserve(static_cast<std::size_t>(count));
    for (int i = 0; i < count; ++i)
        out.push_back(keyed_path_value_at(make_path(static_cast<std::uint64_t>(i), depth), t));
    return out;
}

double mean_of(const std::vector<double>& xs) {
    double sum = 0.0;
    for (const double x : xs)
        sum += x;
    return sum / static_cast<double>(xs.size());
}

double variance_of(const std::vector<double>& xs) {
    const double mean = mean_of(xs);
    double sum = 0.0;
    for (const double x : xs) {
        const double d = x - mean;
        sum += d * d;
    }
    return sum / static_cast<double>(xs.size() - 1);
}

}

TEST_CASE("a keyed path returns the same value whoever asks and in any order",
          "[keyed_brownian_path]") {
    const std::vector<double> times{
        0.1 * one_hour, 0.25 * one_hour, 0.9 * one_hour, 3.3 * one_hour, 7.7 * one_hour};
    const keyed_path_params first = make_path(17, 12);
    std::vector<double> expected;
    for (const double t : times)
        expected.push_back(keyed_path_value_at(first, t));

    const keyed_path_params second = make_path(17, 12);
    std::vector<double> actual(times.size());
    for (std::size_t i = times.size(); i-- > 0;)
        actual[i] = keyed_path_value_at(second, times[i]);

    CHECK(actual == expected);
}

TEST_CASE("a bridge node has the law of Brownian motion at its time", "[keyed_brownian_path]") {
    constexpr unsigned depth = 10;
    const std::vector midpoints = sample(depth, 8192, 0.5 * one_hour);
    const double mean = mean_of(midpoints);
    const double variance = variance_of(midpoints);

    // The midpoint is normal with the variance of its time, half a cell; the
    // quarter cell is the bridge's conditional variance, not this one.
    const double expected_variance = one_hour / 2.0;
    const double expected_sd = std::sqrt(expected_variance / static_cast<double>(midpoints.size()));
    CHECK(std::abs(mean) < 5.0 * expected_sd);
    CHECK(variance > 0.95 * expected_variance);
    CHECK(variance < 1.05 * expected_variance);
}

TEST_CASE("an increment between nodes has the variance of its duration", "[keyed_brownian_path]") {
    constexpr unsigned depth = 10;
    constexpr int count = 8192;

    std::vector<double> near;
    std::vector<double> whole;
    near.reserve(count);
    whole.reserve(count);
    for (int i = 0; i < count; ++i) {
        const keyed_path_params path = make_path(static_cast<std::uint64_t>(i), depth);
        near.push_back(keyed_path_value_at(path, 0.5 * one_hour) -
                       keyed_path_value_at(path, 0.25 * one_hour));
        whole.push_back(keyed_path_value_at(path, one_hour) - keyed_path_value_at(path, 0.0));
    }

    const double near_variance = variance_of(near);
    const double whole_variance = variance_of(whole);
    CHECK(near_variance > 0.95 * (one_hour / 4.0));
    CHECK(near_variance < 1.05 * (one_hour / 4.0));
    CHECK(whole_variance > 0.95 * one_hour);
    CHECK(whole_variance < 1.05 * one_hour);
}

TEST_CASE("inside the finest step the path interpolates linearly", "[keyed_brownian_path]") {
    const keyed_path_params path = make_path(41, 8);
    const double step = keyed_path_finest_step(path);
    const double node = 0.5 * one_hour;
    const double from = keyed_path_value_at(path, node);
    const double to = keyed_path_value_at(path, node + step);

    const double quarter = keyed_path_value_at(path, node + 0.25 * step);
    const double three_quarters = keyed_path_value_at(path, node + 0.75 * step);

    CHECK(std::abs(quarter - (from + 0.25 * (to - from))) < 1e-9 * std::abs(to - from));
    CHECK(std::abs(three_quarters - (from + 0.75 * (to - from))) < 1e-9 * std::abs(to - from));
}

TEST_CASE("the path at several times has the covariance of Brownian motion",
          "[keyed_brownian_path]") {
    // QuantLib's brownianbridge testVariates checks the joint law, not just each
    // margin: the variates must be standard and uncorrelated across the grid.
    constexpr int count = 4096;
    const std::vector<double> times{0.25 * one_hour, 0.5 * one_hour, one_hour, 2.0 * one_hour};
    const std::size_t n = times.size();

    std::vector<std::vector<double>> values(count, std::vector<double>(n, 0.0));
    for (int i = 0; i < count; ++i) {
        const keyed_path_params path = make_path(static_cast<std::uint64_t>(1000 + i), 10);
        for (std::size_t j = 0; j < n; ++j)
            values[i][j] = keyed_path_value_at(path, times[j]);
    }

    std::vector<double> mean(n, 0.0);
    for (int i = 0; i < count; ++i)
        for (std::size_t j = 0; j < n; ++j)
            mean[j] += values[i][j];
    for (double& m : mean)
        m /= static_cast<double>(count);

    for (std::size_t a = 0; a < n; ++a) {
        for (std::size_t b = 0; b < n; ++b) {
            double covariance = 0.0;
            for (int i = 0; i < count; ++i)
                covariance += (values[i][a] - mean[a]) * (values[i][b] - mean[b]);
            covariance /= static_cast<double>(count - 1);

            const double expected = std::min(times[a], times[b]);
            // A sample covariance has a standard error of about sqrt(2 / n).
            const double tolerance = 4.0 * std::sqrt(2.0 / static_cast<double>(count));
            CHECK(covariance > (1.0 - tolerance) * expected);
            CHECK(covariance < (1.0 + tolerance) * expected);
        }
    }

    // A mean of zero, within five standard errors of the ensemble.
    for (std::size_t j = 0; j < n; ++j) {
        const double sd = std::sqrt(times[j] / static_cast<double>(count));
        CHECK(std::abs(mean[j]) < 5.0 * sd);
    }
}

TEST_CASE("the bridge leaves the variance its endpoints allow", "[keyed_brownian_path]") {
    // torchsde's test_normality_conditional: given the endpoints, a node must
    // carry exactly the variance the bridge formula states, not more.
    constexpr int count = 8192;
    constexpr unsigned depth = 10;

    std::vector<double> midpoint;
    std::vector<double> quarter;
    midpoint.reserve(count);
    quarter.reserve(count);
    for (int i = 0; i < count; ++i) {
        const keyed_path_params path = make_path(static_cast<std::uint64_t>(2000 + i), depth);
        const double start = keyed_path_value_at(path, 0.0);
        const double middle = keyed_path_value_at(path, 0.5 * one_hour);
        const double end = keyed_path_value_at(path, one_hour);
        midpoint.push_back(middle - 0.5 * (start + end));
        quarter.push_back(keyed_path_value_at(path, 0.25 * one_hour) - 0.5 * (start + middle));
    }

    // (t_m - t_a)(t_b - t_m) / (t_b - t_a) at the midpoint of a cell, and at the
    // quarter point of the first half.
    const double midpoint_variance = variance_of(midpoint);
    const double quarter_variance = variance_of(quarter);
    CHECK(midpoint_variance > 0.95 * (one_hour / 4.0));
    CHECK(midpoint_variance < 1.05 * (one_hour / 4.0));
    CHECK(quarter_variance > 0.95 * (one_hour / 8.0));
    CHECK(quarter_variance < 1.05 * (one_hour / 8.0));
}

TEST_CASE("a node is built from the draw its address names", "[keyed_brownian_path]") {
    // QuantLib's testVariates recomputes the path from known variates. Here the
    // variates are read straight from the stream at the documented indices, so
    // the test pins the addressing as well as the weights.
    constexpr unsigned depth = 4;
    const keyed_path_params path = make_path(3000, depth);
    const std::int64_t cell = 2;
    const double boundary = keyed_path_boundary_at(path, cell);
    const double increment =
        std::sqrt(one_hour) *
        standard_normal_at(path.key, static_cast<std::uint64_t>(cell) << depth);

    // The finest interval at the cell's start opens at the cell's boundary.
    double from = 0.0;
    double to = 0.0;
    keyed_path_span(path, cell, boundary, 0, from, to);
    CHECK(from == boundary);

    // The cell's end is its boundary plus its increment.
    CHECK(keyed_path_boundary_at(path, cell + 1) ==
          Catch::Approx(boundary + increment).epsilon(1e-12));

    // The first node a cell creates is its midpoint, at heap index one.
    const auto nodes = std::uint64_t{1} << depth;
    const double top =
        standard_normal_at(path.key, (static_cast<std::uint64_t>(cell) << depth) + 1);
    const double expected =
        0.5 * (boundary + boundary + increment) + 0.5 * std::sqrt(one_hour) * top;
    double mid_from = 0.0;
    double mid_to = 0.0;
    keyed_path_span(path, cell, boundary, nodes / 2, mid_from, mid_to);
    CHECK(mid_from == Catch::Approx(expected).epsilon(1e-12));
}

TEST_CASE("repeated reads of one time return one value", "[keyed_brownian_path]") {
    // torchsde's test_determinism: asking twice must not move the answer.
    const keyed_path_params path = make_path(4000, 12);
    keyed_walk walk = make_keyed_walk(path);
    const double t = 1.2345 * one_hour;
    const double first = keyed_walk_value(path, walk, t);
    CHECK(keyed_walk_value(path, walk, t) == first);
    CHECK(keyed_walk_value(path, walk, t) == first);
}

TEST_CASE("the read pattern does not change the values", "[keyed_brownian_path]") {
    // torchsde's cache_size cases: a cache must not alter what a query returns.
    const keyed_path_params path = make_path(5000, 10);
    constexpr int n = 40;
    std::vector<double> times;
    times.reserve(n);
    for (int i = 0; i < n; ++i)
        times.push_back(one_hour * (0.01 + 0.05 * static_cast<double>(i)));

    std::vector<double> expected;
    expected.reserve(n);
    for (const double t : times)
        expected.push_back(keyed_path_value_at(path, t));

    keyed_walk dense = make_keyed_walk(path);
    std::vector<double> actual;
    actual.reserve(n);
    for (const double t : times) {
        actual.push_back(keyed_walk_value(path, dense, t));
        static_cast<void>(keyed_walk_value(path, dense, t));
    }
    CHECK(actual == expected);

    // A second walk, each time read in a pair, must agree with the first.
    keyed_walk paired = make_keyed_walk(path);
    std::vector<double> again;
    again.reserve(n);
    for (const double t : times) {
        static_cast<void>(keyed_walk_value(path, paired, t));
        again.push_back(keyed_walk_value(path, paired, t));
    }
    CHECK(again == expected);
}

TEST_CASE("its increments have the quadratic variation of their duration",
          "[keyed_brownian_path]") {
    // A Brownian path has sum of squared increments equal to time; a path with
    // the right margins but the wrong dependence fails this.
    constexpr int paths = 64;
    constexpr unsigned depth = 9;
    const auto steps = std::uint64_t{1} << depth;
    const double step = one_hour / static_cast<double>(steps);

    double total = 0.0;
    for (int p = 0; p < paths; ++p) {
        const keyed_path_params path = make_path(static_cast<std::uint64_t>(6000 + p), depth);
        keyed_walk walk = make_keyed_walk(path);
        double previous = keyed_walk_value(path, walk, 0.0);
        double sum = 0.0;
        for (std::uint64_t i = 1; i <= steps; ++i) {
            const double value = keyed_walk_value(path, walk, static_cast<double>(i) * step);
            const double increment = value - previous;
            sum += increment * increment;
            previous = value;
        }
        total += sum;
    }
    // One path's sum of squares has a relative standard error near 6% at this
    // depth, so the ensemble is what carries a 5% claim.
    const double mean_sum = total / static_cast<double>(paths);
    CHECK(std::abs(mean_sum - one_hour) < 0.05 * one_hour);
}

TEST_CASE("a cell boundary is the sum of the increments before it", "[keyed_brownian_path]") {
    const keyed_path_params path = make_path(47, 8);
    for (std::int64_t cell = 0; cell < 5; ++cell) {
        const double summed =
            keyed_path_boundary_at(path, cell + 1) - keyed_path_boundary_at(path, cell);
        const double drawn = keyed_path_cell_increment(path, cell);
        CHECK(summed == Catch::Approx(drawn).epsilon(1e-12));
    }
    CHECK(keyed_path_boundary_at(path, 0) == 0.0);
}

TEST_CASE("the path is zero at its origin and refuses bad arguments", "[keyed_brownian_path]") {
    const double origin = 3.25 * one_hour;
    const keyed_path_params path = make_path(43, 8, origin);
    CHECK(keyed_path_value_at(path, origin) == 0.0);
    CHECK(path.origin == origin);
    CHECK(path.cell_width == one_hour);
    CHECK(path.origin_cell == 3);
    CHECK(path.depth == 8);
    CHECK(keyed_path_finest_step(path) == one_hour / 256.0);

    CHECK_THROWS_AS(keyed_path_value_at(path, origin - 1.0), std::invalid_argument);
    CHECK_THROWS_AS(make_path(1, 0), std::invalid_argument);
    CHECK_THROWS_AS(make_path(1, 33), std::invalid_argument);
    CHECK_THROWS_AS(make_keyed_path(factor_key(1), 0.0, 8, 0.0), std::invalid_argument);
    CHECK_THROWS_AS(make_keyed_path(factor_key(1), one_hour, 8, -1.0), std::invalid_argument);
}

TEST_CASE("a walk agrees with the path as it moves forward", "[keyed_brownian_path]") {
    const keyed_path_params path = make_path(53, 12);
    keyed_walk walk = make_keyed_walk(path);

    const std::vector<double> times{
        0.05 * one_hour, 0.4 * one_hour, 0.99 * one_hour, 2.5 * one_hour, 9.75 * one_hour};
    for (const double t : times)
        CHECK(keyed_walk_value(path, walk, t) == keyed_path_value_at(path, t));
}

TEST_CASE("a walk refuses a read behind the cell it holds", "[keyed_brownian_path]") {
    const keyed_path_params path = make_path(59, 8);
    keyed_walk walk = make_keyed_walk(path);
    keyed_walk_value(path, walk, 5.0 * one_hour);
    CHECK_THROWS_AS(keyed_walk_value(path, walk, 3.0 * one_hour), std::invalid_argument);
}

TEST_CASE("a walk answers a second tick in the same span from its cache", "[keyed_brownian_path]") {
    const keyed_path_params path = make_path(61, 10);
    keyed_walk walk = make_keyed_walk(path);
    const double step = keyed_path_finest_step(path);
    const double node = 0.5 * one_hour;

    const double first = keyed_walk_value(path, walk, node + 0.1 * step);
    const std::uint64_t span_node = walk.span_node;
    const double second = keyed_walk_value(path, walk, node + 0.6 * step);

    CHECK(walk.span_valid);
    CHECK(walk.span_node == span_node);
    CHECK(first == keyed_path_value_at(path, node + 0.1 * step));
    CHECK(second == keyed_path_value_at(path, node + 0.6 * step));
}

TEST_CASE("a jump over many cells leaves the values unchanged", "[keyed_brownian_path]") {
    const keyed_path_params path = make_path(71, 10);
    keyed_walk walk = make_keyed_walk(path);
    keyed_walk_value(path, walk, 0.5 * one_hour);
    CHECK(keyed_walk_value(path, walk, 1000.5 * one_hour) ==
          keyed_path_value_at(path, 1000.5 * one_hour));
}

TEST_CASE("a tick costs the depth, not the time since the run began",
          "[.benchmark][keyed_brownian_path]") {
    constexpr int reads = 100000;
    for (const unsigned depth : {16u, 30u}) {
        const keyed_path_params path = make_path(73, depth);
        const double step = keyed_path_finest_step(path);
        const double start_time = 0.5 * one_hour;
        double sink = 0.0;

        // Four ticks inside one span, then the next span: the market's pattern.
        keyed_walk warm = make_keyed_walk(path);
        keyed_walk_value(path, warm, start_time);
        auto started = std::chrono::steady_clock::now();
        for (int i = 0; i < reads; ++i)
            sink += keyed_walk_value(path,
                                     warm,
                                     start_time + static_cast<double>(i % 4) * 0.2 * step +
                                         static_cast<double>(i / 4) * step);
        const auto warm_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(
                                 std::chrono::steady_clock::now() - started)
                                 .count() /
                             static_cast<long long>(reads);

        // Every tick lands in a new span, so the bridge is walked in full.
        keyed_walk cold = make_keyed_walk(path);
        started = std::chrono::steady_clock::now();
        for (int i = 0; i < reads; ++i)
            sink += keyed_walk_value(path, cold, start_time + static_cast<double>(i) * step);
        const auto cold_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(
                                 std::chrono::steady_clock::now() - started)
                                 .count() /
                             static_cast<long long>(reads);

        std::printf("depth %u: %lld ns a cached tick, %lld ns a cold tick (sink %g)\n",
                    depth,
                    static_cast<long long>(warm_ns),
                    static_cast<long long>(cold_ns),
                    sink);
    }
}
