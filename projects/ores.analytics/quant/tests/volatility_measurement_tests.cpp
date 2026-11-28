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
#include "ores.analytics.quant/math/volatility_measurement.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <string>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

using std::chrono::year_month_day;
using std::chrono::January;
using std::chrono::sys_days;

year_month_day observation_day(int day) {
    return year_month_day{sys_days{std::chrono::year{2026} / January / day}};
}

std::vector<dated_level> levels_of(const std::vector<double>& values) {
    std::vector<dated_level> out;
    for (std::size_t i = 0; i < values.size(); ++i)
        out.push_back({.date = observation_day(static_cast<int>(i) + 1), .level = values[i]});
    return out;
}

}

TEST_CASE("a log return is the change of two logs, dated by the later day",
          "[volatility_measurement]") {
    const auto returns =
        returns_of(levels_of({100.0, 110.0, 99.0}), volatility_transform::log_return);
    REQUIRE(returns.returns.size() == 2);
    CHECK(returns.returns[0] == Catch::Approx(std::log(110.0) - std::log(100.0)).epsilon(1e-15));
    CHECK(returns.returns[1] == Catch::Approx(std::log(99.0) - std::log(110.0)).epsilon(1e-15));
    CHECK(returns.dates[0] == observation_day(2));
    CHECK(returns.dates[1] == observation_day(3));
}

TEST_CASE("a level that is not positive is no observation, and the return spans it",
          "[volatility_measurement]") {
    const auto returns =
        returns_of(levels_of({100.0, 0.0, 121.0, -5.0, 121.0}), volatility_transform::log_return);
    REQUIRE(returns.returns.size() == 2);
    CHECK(returns.returns[0] == Catch::Approx(std::log(121.0) - std::log(100.0)).epsilon(1e-15));
    CHECK(returns.dates[0] == observation_day(3));
    CHECK(returns.returns[1] == Catch::Approx(0.0).margin(1e-15));
}

TEST_CASE("a difference keeps a series that crosses zero", "[volatility_measurement]") {
    const auto returns =
        returns_of(levels_of({0.03, -0.01, 0.04}), volatility_transform::difference);
    REQUIRE(returns.returns.size() == 2);
    CHECK(returns.returns[0] == Catch::Approx(-0.04).epsilon(1e-15));
    CHECK(returns.returns[1] == Catch::Approx(0.05).epsilon(1e-15));
}

TEST_CASE("a level that is not finite is no observation", "[volatility_measurement]") {
    const auto returns =
        returns_of(levels_of({100.0, std::nan(""), 121.0}), volatility_transform::log_return);
    REQUIRE(returns.returns.size() == 1);
    CHECK(returns.returns[0] == Catch::Approx(std::log(121.0) - std::log(100.0)).epsilon(1e-15));
    CHECK(returns.dates[0] == observation_day(3));
}

TEST_CASE("one observation has no return", "[volatility_measurement]") {
    CHECK(returns_of(levels_of({100.0}), volatility_transform::log_return).returns.empty());
    CHECK(returns_of({}, volatility_transform::difference).returns.empty());
}

TEST_CASE("a window is the longer of the pillar and the minimum, and never below two",
          "[volatility_measurement]") {
    const measurement_settings settings{};
    CHECK(measurement_window(1.0, settings) == 252);
    CHECK(measurement_window(7.0 / 365.0, settings) == 21);
    CHECK(measurement_window(1.0 / 24.0, settings) == 21);
    CHECK(measurement_window(0.0, settings) == 21);
    CHECK(measurement_window(5.0, {.minimum_window = 0}) == 1260);
    CHECK(measurement_window(0.0, {.minimum_window = 0}) == 2);
}

TEST_CASE("a pillar's level is the annualised spread of its window, scaled",
          "[volatility_measurement]") {
    // Alternating changes of one basis point in a rate: a sample stdev of one basis point.
    std::vector<double> rates{0.04};
    for (int i = 1; i < 400; ++i)
        rates.push_back(rates.back() + (i % 2 == 0 ? 0.0001 : -0.0001));
    const auto returns = returns_of(levels_of(rates), volatility_transform::difference);
    const auto measured =
        measure_pillar(returns.returns,
                       1.0,
                       {.transform = volatility_transform::difference, .level_multiplier = 1.0});
    REQUIRE(measured);
    CHECK(measured->window == 252);
    const auto expected = *realised_volatility(returns.returns, measured->window, 252.0);
    CHECK(measured->level == Catch::Approx(expected).epsilon(1e-15));
    CHECK(measured->level == Catch::Approx(0.0001 * std::sqrt(252.0)).epsilon(5e-3));

    const auto scaled =
        measure_pillar(returns.returns,
                       1.0,
                       {.transform = volatility_transform::difference, .level_multiplier = 1.5});
    REQUIRE(scaled);
    CHECK(scaled->level == Catch::Approx(1.5 * expected).epsilon(1e-15));
}

TEST_CASE("a pillar the sample does not cover is refused, and one past the maximum with it",
          "[volatility_measurement]") {
    std::vector<double> returns;
    for (int i = 0; i < 100; ++i)
        returns.push_back(i % 2 == 0 ? 0.001 : -0.001);
    CHECK_FALSE(measure_pillar(returns, 1.0, {}));
    CHECK_FALSE(measure_pillar(returns, 0.0, {}));
    CHECK_FALSE(measure_pillar(returns, -1.0, {}));
    CHECK_FALSE(measure_pillar(returns, maximum_pillar_years + 1.0, {}));
}

TEST_CASE("a series' shape is its moments shrunk toward the prior", "[volatility_measurement]") {
    std::vector<double> returns{0.01, -0.02, 0.015, -0.03, 0.005, -0.01, 0.02, -0.025};
    const shape_prior prior{.shape = {.skewness = 0.0, .excess_kurtosis = 1.0}, .weight = 500.0};
    const auto measured = measure_series(returns, {.prior = prior});
    REQUIRE(measured);
    const auto moments = moments_of(returns);
    REQUIRE(moments);
    CHECK(measured->moments.stdev == moments->stdev);
    const auto expected = shrink(*moments, prior);
    CHECK(measured->shape.skewness == expected.skewness);
    CHECK(measured->shape.excess_kurtosis == expected.excess_kurtosis);

    CHECK_FALSE(measure_series(std::vector<double>{0.1, 0.2, 0.3}, {}));
}
