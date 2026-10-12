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
#include "ores.synthetic.api/feeds/fx_simple_conversion.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <stdexcept>
#include <string>

namespace {

const std::string tags("[fx_simple_conversion]");

}

using Catch::Approx;
using ores::synthetic::feed::fx_mixture_from_simple;
using ores::synthetic::feed::fx_simple_from_mixture;
using ores::synthetic::feed::fx_simple_view;

namespace {

const fx_simple_view sample{.trend_pct_per_hour = 0.05,
                            .volatility_pct_per_hour = 0.1,
                            .jump_share_pct = 10.0,
                            .half_life_minutes = 15.0};

}

TEST_CASE("fx_simple_conversion spreads a figure per hour over the updates of the hour", tags) {
    const auto m = fx_mixture_from_simple(sample, "geometric", 1.1, 3600);
    REQUIRE(m.means.size() == 2);
    CHECK(m.means[0] == Approx(0.05 / 100.0 / 3600.0));
    CHECK(m.stdevs[0] == Approx(0.1 / 100.0 / 60.0));
    CHECK(m.weights[0] == Approx(0.9));
}

TEST_CASE("fx_simple_conversion adds a jump regime of four times the volatility", tags) {
    const auto m = fx_mixture_from_simple(sample, "geometric", 1.1, 3600);
    CHECK(m.means[1] == 0.0);
    CHECK(m.stdevs[1] == Approx(4.0 * m.stdevs[0]));
    CHECK(m.weights[1] == Approx(0.1));
}

TEST_CASE("fx_simple_conversion writes one component when there are no jumps", tags) {
    auto view = sample;
    view.jump_share_pct = 0.0;
    const auto m = fx_mixture_from_simple(view, "geometric", 1.1, 3600);
    CHECK(m.means.size() == 1);
    CHECK(m.weights[0] == Approx(1.0));
}

TEST_CASE("fx_simple_conversion scales the arithmetic engine by the price", tags) {
    const auto geometric = fx_mixture_from_simple(sample, "geometric", 2.0, 3600);
    const auto arithmetic = fx_mixture_from_simple(sample, "arithmetic", 2.0, 3600);
    CHECK(arithmetic.stdevs[0] == Approx(2.0 * geometric.stdevs[0]));
    CHECK(arithmetic.means[0] == Approx(2.0 * geometric.means[0]));
}

TEST_CASE("fx_simple_conversion gives Ornstein-Uhlenbeck one component with κ from the half-life",
          tags) {
    const auto m = fx_mixture_from_simple(sample, "ornstein_uhlenbeck", 2.0, 60);
    REQUIRE(m.weights.size() == 1);
    CHECK(m.weights[0] == Approx(std::log(2.0) / (15.0 * 60.0 / 60.0)));
    CHECK(m.means[0] == 0.0);
    CHECK(m.stdevs[0] == Approx(0.1 / 100.0 / std::sqrt(60.0) * 2.0));
}

TEST_CASE("fx_simple_conversion reads what it wrote back to the same view", tags) {
    for (const auto* engine : {"geometric", "arithmetic", "ornstein_uhlenbeck"}) {
        CAPTURE(engine);
        const auto m = fx_mixture_from_simple(sample, engine, 1.3, 1800);
        const auto reading = fx_simple_from_mixture(m, engine, 1.3, 1800);
        CHECK(reading.exact);
        CHECK(reading.view.volatility_pct_per_hour == Approx(sample.volatility_pct_per_hour));
        if (std::string(engine) == "ornstein_uhlenbeck") {
            CHECK(reading.view.half_life_minutes == Approx(sample.half_life_minutes));
        } else {
            CHECK(reading.view.trend_pct_per_hour == Approx(sample.trend_pct_per_hour));
            CHECK(reading.view.jump_share_pct == Approx(sample.jump_share_pct));
        }
    }
}

TEST_CASE("fx_simple_conversion reads the seeded primary and tail as a simple form", tags) {
    const ores::synthetic::feed::fx_mixture seeded{
        .means = {0.0, 0.0}, .stdevs = {2.14e-05, 8.56e-05}, .weights = {0.95, 0.05}};
    const auto reading = fx_simple_from_mixture(seeded, "geometric", 1.1, 3600);
    CHECK(reading.exact);
    CHECK(reading.view.jump_share_pct == Approx(5.0));
    CHECK(reading.view.volatility_pct_per_hour == Approx(2.14e-05 * 60.0 * 100.0));
}

TEST_CASE("fx_simple_conversion allows for the rounding of a seeded tail", tags) {
    const ores::synthetic::feed::fx_mixture seeded{
        .means = {0.0, 0.0}, .stdevs = {1.89e-05, 7.55e-05}, .weights = {0.95, 0.05}};
    CHECK(fx_simple_from_mixture(seeded, "geometric", 1.0, 3600).exact);
}

TEST_CASE("fx_simple_conversion approximates a mixture set by hand, and says so", tags) {
    const ores::synthetic::feed::fx_mixture hand{
        .means = {0.0, 0.0001, 0.0}, .stdevs = {0.001, 0.0011, 0.01}, .weights = {0.6, 0.3, 0.1}};
    const auto reading = fx_simple_from_mixture(hand, "geometric", 1.0, 3600);
    CHECK_FALSE(reading.exact);
    CHECK_FALSE(reading.reason.empty());
    CHECK(reading.view.jump_share_pct == Approx(10.0));
}

TEST_CASE("fx_simple_conversion has no exact form for a mean-reverting engine with no pull", tags) {
    const ores::synthetic::feed::fx_mixture still{.means = {0.0}, .stdevs = {0.001}, .weights = {0.0}};
    const auto reading = fx_simple_from_mixture(still, "ornstein_uhlenbeck", 1.0, 3600);
    CHECK_FALSE(reading.exact);
}

TEST_CASE("fx_simple_conversion refuses what no engine can read", tags) {
    CHECK_THROWS_AS(fx_mixture_from_simple(sample, "nope", 1.0, 3600), std::invalid_argument);
    CHECK_THROWS_AS(fx_mixture_from_simple(sample, "geometric", 0.0, 3600), std::invalid_argument);
    auto wide = sample;
    wide.jump_share_pct = 120.0;
    CHECK_THROWS_AS(fx_mixture_from_simple(wide, "geometric", 1.0, 3600), std::invalid_argument);
    auto still = sample;
    still.half_life_minutes = 0.0;
    CHECK_THROWS_AS(fx_mixture_from_simple(still, "ornstein_uhlenbeck", 1.0, 3600),
                    std::invalid_argument);
}
