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
#include "ores.analytics.quant/math/mixture_tilt.hpp"
#include "ores.analytics.quant/service/processes/gaussian_mixture_model_process.hpp"
#include "price_stream.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstddef>
#include <vector>

using ores::analytics::quant::math::mixture;
using ores::analytics::quant::service::gaussian_mixture_model_process;

namespace {

/// Mostly quiet with a wide fifth, and means that differ, so a moved mean would show.
const mixture quiet_and_wide{
    .means = {0.0002, -0.0003}, .stdevs = {0.001, 0.004}, .weights = {0.8, 0.2}};

struct moments final {
    double mean;
    double variance;
    double kurtosis;
};

moments of(const mixture& m) {
    double mean = 0.0;
    for (std::size_t k = 0; k < m.means.size(); ++k)
        mean += m.weights[k] * m.means[k];
    double second = 0.0;
    double fourth = 0.0;
    for (std::size_t k = 0; k < m.means.size(); ++k) {
        const double d = m.means[k] - mean;
        const double s2 = m.stdevs[k] * m.stdevs[k];
        second += m.weights[k] * (s2 + d * d);
        fourth += m.weights[k] * (d * d * d * d + 6.0 * d * d * s2 + 3.0 * s2 * s2);
    }
    return {.mean = mean, .variance = second, .kurtosis = fourth / (second * second)};
}

moments of(const std::vector<double>& xs) {
    double mean = 0.0;
    for (const double x : xs)
        mean += x;
    mean /= static_cast<double>(xs.size());
    double second = 0.0;
    double fourth = 0.0;
    for (const double x : xs) {
        const double d = (x - mean) * (x - mean);
        second += d;
        fourth += d * d;
    }
    second /= static_cast<double>(xs.size());
    fourth /= static_cast<double>(xs.size());
    return {.mean = mean, .variance = second, .kurtosis = fourth / (second * second)};
}

/// One log-return a nominal tick, @p n of them, at a fixed tilt.
std::vector<double> log_returns(double tilt, std::size_t n) {
    gaussian_mixture_model_process p(quiet_and_wide.means,
                                     quiet_and_wide.stdevs,
                                     quiet_and_wide.weights,
                                     100.0,
                                     price_stream(31));
    p.set_tail_tilt(tilt);
    std::vector<double> xs;
    xs.reserve(n);
    double last = p.current();
    for (std::size_t i = 0; i < n; ++i) {
        const double now = p.next(1.0);
        xs.push_back(std::log(now / last));
        last = now;
    }
    return xs;
}

}

TEST_CASE("a tilt holds the weights, the mean and the variance, and raises the kurtosis",
          "[mixture_tilt]") {
    const auto base = of(quiet_and_wide);
    double kurtosis = 0.0;
    for (const double t : {-1.0, -0.5, 0.0, 0.5, 1.0, 2.0}) {
        const auto tilted = ores::analytics::quant::math::tilt(quiet_and_wide, t);
        CHECK(tilted.weights == quiet_and_wide.weights);
        CHECK(tilted.means == quiet_and_wide.means);
        const auto m = of(tilted);
        CHECK(m.mean == Catch::Approx(base.mean).epsilon(1e-12));
        CHECK(m.variance == Catch::Approx(base.variance).epsilon(1e-12));
        CHECK(m.kurtosis > kurtosis);
        kurtosis = m.kurtosis;
    }
}

TEST_CASE("a tilt of minus one leaves one shared stdev, and goes no further", "[mixture_tilt]") {
    const auto flat = ores::analytics::quant::math::tilt(quiet_and_wide, -1.0);
    CHECK(flat.stdevs[0] == Catch::Approx(flat.stdevs[1]));
    CHECK(ores::analytics::quant::math::tilt(quiet_and_wide, -3.0).stdevs == flat.stdevs);
}

TEST_CASE("a zero tilt, or a component with no spread, leaves the mixture as it is",
          "[mixture_tilt]") {
    CHECK(ores::analytics::quant::math::tilt(quiet_and_wide, 0.0).stdevs == quiet_and_wide.stdevs);
    const mixture degenerate{.means = {0.0, 0.0}, .stdevs = {0.0, 0.01}, .weights = {0.5, 0.5}};
    CHECK(ores::analytics::quant::math::tilt(degenerate, 1.0).stdevs == degenerate.stdevs);
}

TEST_CASE("a component with no weight takes the tilt and carries none of the variance",
          "[mixture_tilt]") {
    const mixture idle{
        .means = {0.0, 0.0, 0.0}, .stdevs = {0.001, 0.004, 0.02}, .weights = {0.8, 0.2, 0.0}};
    const auto tilted = ores::analytics::quant::math::tilt(idle, 1.0);
    const mixture live{.means = {0.0, 0.0}, .stdevs = {0.001, 0.004}, .weights = {0.8, 0.2}};
    const auto without = ores::analytics::quant::math::tilt(live, 1.0);
    CHECK(of(tilted).variance == Catch::Approx(of(idle).variance).epsilon(1e-12));
    CHECK(of(tilted).kurtosis == Catch::Approx(of(without).kurtosis).epsilon(1e-12));
}

TEST_CASE("a mixture process with no tilt draws the path it always did", "[mixture_tilt]") {
    gaussian_mixture_model_process untouched(quiet_and_wide.means,
                                             quiet_and_wide.stdevs,
                                             quiet_and_wide.weights,
                                             100.0,
                                             price_stream(37));
    gaussian_mixture_model_process zeroed(quiet_and_wide.means,
                                          quiet_and_wide.stdevs,
                                          quiet_and_wide.weights,
                                          100.0,
                                          price_stream(37));
    for (int i = 0; i < 200; ++i) {
        zeroed.set_tail_tilt(0.0);
        CHECK(untouched.next(1.0) == zeroed.next(1.0));
    }
}

TEST_CASE("a tilted process's returns are fatter tailed with the same mean and variance",
          "[mixture_tilt]") {
    // 200,000 log-returns of one nominal tick each, from one seed, untilted and at the
    // tilt a pinned active market gives at a tail loading of two: 2 ln 2.
    constexpr std::size_t n = 200'000;
    const auto calm = of(log_returns(0.0, n));
    const auto busy = of(log_returns(2.0 * std::log(2.0), n));
    const auto expected =
        of(ores::analytics::quant::math::tilt(quiet_and_wide, 2.0 * std::log(2.0)));

    CHECK(busy.kurtosis > calm.kurtosis + 2.0);
    CHECK(busy.kurtosis == Catch::Approx(expected.kurtosis).epsilon(0.1));
    CHECK(busy.variance == Catch::Approx(calm.variance).epsilon(0.03));
    // The mean is a few hundredths of a stdev; it must stay within sampling error of it.
    const double error = std::sqrt(calm.variance / static_cast<double>(n));
    CHECK(std::abs(busy.mean - calm.mean) < 4.0 * error);
}

TEST_CASE("a calm market's negative tilt thins a process's tails and holds its variance",
          "[mixture_tilt]") {
    // The same sample at the tilt a pinned calm market gives at a loading of two: 2 ln 0.5.
    constexpr std::size_t n = 200'000;
    const auto base = of(log_returns(0.0, n));
    const auto quiet = of(log_returns(2.0 * std::log(0.5), n));
    CHECK(quiet.kurtosis < base.kurtosis - 2.0);
    CHECK(quiet.variance == Catch::Approx(base.variance).epsilon(0.03));
}
