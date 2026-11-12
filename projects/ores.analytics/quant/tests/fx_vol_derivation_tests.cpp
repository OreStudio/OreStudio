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
#include "ores.analytics.quant/math/fx_vol_derivation.hpp"
#include "ores.analytics.quant/math/return_moments.hpp"
#include "price_stream.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstdint>
#include <random>
#include <string>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

double expansion_minimum(const return_shape& shape) {
    double lowest = 1.0;
    for (double z = -12.0; z <= 12.0; z += 1e-3) {
        const double z2 = z * z;
        const double p = 1.0 + shape.skewness * z * (z2 - 3.0) / 6.0 +
                         shape.excess_kurtosis * (z2 * z2 - 6.0 * z2 + 3.0) / 24.0;
        lowest = std::min(lowest, p);
    }
    return lowest;
}

std::vector<double> derivation_normal_returns(double daily_stdev, int n, std::uint64_t seed) {
    auto rng = price_stream(seed);
    std::normal_distribution<double> draw(0.0, daily_stdev);
    std::vector<double> out(static_cast<std::size_t>(n));
    for (auto& r : out)
        r = draw(rng);
    return out;
}

/// Daily returns with a negative skew: mostly small rises, now and then a larger fall.
std::vector<double> derivation_skewed_returns(int n, std::uint64_t seed) {
    auto rng = price_stream(seed);
    std::bernoulli_distribution crash(0.05);
    std::normal_distribution<double> calm(0.0003, 0.005);
    std::normal_distribution<double> fall(-0.006, 0.012);
    std::vector<double> out(static_cast<std::size_t>(n));
    for (auto& r : out)
        r = crash(rng) ? fall(rng) : calm(rng);
    return out;
}

fx_quote_convention derivation_convention() {
    return {.atm = fx_atm_type::delta_neutral,
            .delta = fx_delta_type::spot,
            .favour = fx_risk_reversal_favour::call,
            .butterfly = fx_butterfly_style::broker};
}

}

TEST_CASE("the moments of a small sample are its central moments", "[return_moments]") {
    const std::vector<double> sample{1.0, 2.0, 3.0, 4.0, 10.0};
    const auto m = moments_of(sample);
    REQUIRE(m);
    CHECK(m->count == 5);
    CHECK(m->mean == Catch::Approx(4.0).epsilon(1e-15));
    CHECK(m->stdev == Catch::Approx(std::sqrt(12.5)).epsilon(1e-15));
    CHECK(m->skewness == Catch::Approx(36.0 / std::pow(10.0, 1.5)).epsilon(1e-14));
    CHECK(m->excess_kurtosis == Catch::Approx(278.8 / 100.0 - 3.0).epsilon(1e-14));
}

TEST_CASE("normal returns have no skewness and no excess kurtosis", "[return_moments]") {
    const auto m = moments_of(derivation_normal_returns(0.01, 200'000, 3));
    REQUIRE(m);
    CHECK(m->stdev == Catch::Approx(0.01).epsilon(5e-3));
    CHECK(std::abs(m->skewness) < 3.0 * std::sqrt(6.0 / 200'000.0));
    CHECK(std::abs(m->excess_kurtosis) < 3.0 * std::sqrt(24.0 / 200'000.0));
}

TEST_CASE("a sample of fewer than four returns, or of no spread, has no moments",
          "[return_moments]") {
    CHECK_FALSE(moments_of(std::vector<double>{0.1, 0.2, 0.3}));
    CHECK_FALSE(moments_of(std::vector<double>(50, 0.002)));
}

TEST_CASE("the shape moves from the prior by n over n plus the prior's weight",
          "[return_moments]") {
    const return_moments sample{
        .count = 100, .mean = 0.0, .stdev = 0.01, .skewness = -0.8, .excess_kurtosis = 4.0};
    const auto s =
        shrink(sample, {.shape = {.skewness = 0.0, .excess_kurtosis = 1.0}, .weight = 300.0});
    CHECK(s.skewness == Catch::Approx(0.25 * -0.8).epsilon(1e-15));
    CHECK(s.excess_kurtosis == Catch::Approx(0.25 * 4.0 + 0.75 * 1.0).epsilon(1e-15));
    const auto unshrunk =
        shrink(sample, {.shape = {.skewness = 1.0, .excess_kurtosis = 9.0}, .weight = 0.0});
    CHECK(unshrunk.skewness == -0.8);
    CHECK(unshrunk.excess_kurtosis == 4.0);
}

TEST_CASE("an n-day return's skewness falls as one over root n and its kurtosis as one over n",
          "[return_moments]") {
    const auto s = over_days({.skewness = -0.4, .excess_kurtosis = 3.0}, 25.0);
    CHECK(s.skewness == Catch::Approx(-0.08).epsilon(1e-15));
    CHECK(s.excess_kurtosis == Catch::Approx(0.12).epsilon(1e-15));
}

TEST_CASE("the Gram-Charlier region holds the normal and the edges Jondeau and Rockinger map",
          "[return_moments]") {
    CHECK(is_gram_charlier_density({.skewness = 0.0, .excess_kurtosis = 0.0}));
    CHECK(is_gram_charlier_density({.skewness = 0.0, .excess_kurtosis = 4.0}));
    CHECK(is_gram_charlier_density({.skewness = 0.5, .excess_kurtosis = 2.0}));
    CHECK_FALSE(is_gram_charlier_density({.skewness = 0.0, .excess_kurtosis = 4.1}));
    CHECK_FALSE(is_gram_charlier_density({.skewness = 0.0, .excess_kurtosis = -0.1}));
    CHECK_FALSE(is_gram_charlier_density({.skewness = 0.1, .excess_kurtosis = 0.0}));
    CHECK_FALSE(is_gram_charlier_density({.skewness = 1.5, .excess_kurtosis = 2.4}));
    // Negative far out, beyond any fixed window: about -0.13 near z = -30.
    CHECK_FALSE(is_gram_charlier_density({.skewness = 0.001, .excess_kurtosis = 0.0001}));
}

TEST_CASE("a shape outside the region is held on its edge with the kurtosis kept",
          "[return_moments]") {
    const return_shape inside{.skewness = -0.3, .excess_kurtosis = 1.5};
    CHECK(hold_in_gram_charlier_region(inside).skewness == inside.skewness);
    CHECK(hold_in_gram_charlier_region(inside).excess_kurtosis == inside.excess_kurtosis);

    for (const return_shape outside : {return_shape{.skewness = -1.6, .excess_kurtosis = 2.0},
                                       return_shape{.skewness = 0.9, .excess_kurtosis = 0.6},
                                       return_shape{.skewness = -0.5, .excess_kurtosis = 7.0}}) {
        const auto held = hold_in_gram_charlier_region(outside);
        INFO("skewness " << outside.skewness << ", kurtosis " << outside.excess_kurtosis);
        CHECK(is_gram_charlier_density(held));
        CHECK(expansion_minimum(held) > -1e-6);
        CHECK(held.excess_kurtosis == std::clamp(outside.excess_kurtosis, 0.0, 4.0));
        CHECK(held.skewness * outside.skewness >= 0.0);
        if (held.skewness != 0.0)
            CHECK_FALSE(is_gram_charlier_density(
                {.skewness = held.skewness * 1.01, .excess_kurtosis = held.excess_kurtosis}));
    }
}

TEST_CASE("the realised volatility is the window's sample stdev, annualised", "[return_moments]") {
    std::vector<double> alternating;
    for (int i = 0; i < 30; ++i)
        alternating.push_back(i % 2 == 0 ? 0.01 : -0.01);
    alternating.insert(alternating.begin(), 0.5);
    const auto v = realised_volatility(alternating, 10, 252.0);
    REQUIRE(v);
    CHECK(*v == Catch::Approx(std::sqrt(10.0 * 1e-4 / 9.0 * 252.0)).epsilon(1e-14));
    CHECK_FALSE(realised_volatility(alternating, 40, 252.0));
    CHECK_FALSE(realised_volatility(alternating, 1, 252.0));
}

TEST_CASE("a derivation reads each expiry's level over its own window and its shape over the "
          "whole history",
          "[fx_vol_derivation]") {
    const auto returns = derivation_skewed_returns(1260, 7);
    const std::vector<fx_expiry> expiries{
        {.label = "1W", .years = 7.0 / 365.0, .convention = derivation_convention()},
        {.label = "3M", .years = 0.25, .convention = derivation_convention()},
        {.label = "1Y", .years = 1.0, .convention = derivation_convention()}};
    const fx_derivation_settings settings{
        .prior = {.shape = {.skewness = 0.0, .excess_kurtosis = 1.0}, .weight = 500.0},
        .level_multiplier = 1.1};
    const auto d = derive_fx_quotes(returns, expiries, settings);
    REQUIRE(d);
    REQUIRE(d->expiries.size() == 3);
    CHECK(d->moments.skewness < -0.2);
    const auto expected_shape = shrink(d->moments, settings.prior);
    CHECK(d->shape.skewness == expected_shape.skewness);

    const std::vector<int> windows{21, 63, 252};
    for (std::size_t i = 0; i < expiries.size(); ++i) {
        REQUIRE(d->expiries[i]);
        const auto& e = *d->expiries[i];
        INFO(e.label);
        CHECK(e.window == windows[i]);
        CHECK(e.smile.sigma ==
              Catch::Approx(1.1 * *realised_volatility(returns, e.window, 252.0)).epsilon(1e-14));
        const auto shape =
            hold_in_gram_charlier_region(over_days(d->shape, expiries[i].years * 252.0));
        CHECK(e.smile.skewness == shape.skewness);
        CHECK(e.smile.excess_kurtosis == shape.excess_kurtosis);
        CHECK(e.quotes.rr25 < 0.0);
        CHECK(e.quotes.bf25 > 0.0);
    }
    const auto& month = *d->expiries[1];
    const auto& year = *d->expiries[2];
    CHECK(year.smile.skewness / month.smile.skewness == Catch::Approx(0.5).epsilon(1e-12));
    CHECK(std::abs(year.quotes.rr25 / year.quotes.atm) <
          std::abs(month.quotes.rr25 / month.quotes.atm));
}

TEST_CASE("normal returns derive their own volatility and an almost flat smile",
          "[fx_vol_derivation]") {
    const auto returns = derivation_normal_returns(0.006, 1260, 5);
    const std::vector<fx_expiry> year{
        {.label = "1Y", .years = 1.0, .convention = derivation_convention()}};
    const auto d = derive_fx_quotes(
        returns,
        year,
        {.prior = {.shape = {.skewness = 0.0, .excess_kurtosis = 0.0}, .weight = 0.0}});
    REQUIRE(d);
    REQUIRE(d->expiries[0]);
    const auto& q = d->expiries[0]->quotes;
    CHECK(q.atm == Catch::Approx(0.006 * std::sqrt(252.0)).epsilon(0.1));
    CHECK(std::abs(q.rr25) < 0.002 * q.atm);
    CHECK(std::abs(q.bf25) < 0.002 * q.atm);
}

TEST_CASE("an expiry longer than the history is refused alone, naming itself",
          "[fx_vol_derivation]") {
    const auto returns = derivation_normal_returns(0.006, 300, 9);
    const std::vector<fx_expiry> expiries{
        {.label = "1M", .years = 1.0 / 12.0, .convention = derivation_convention()},
        {.label = "2Y", .years = 2.0, .convention = derivation_convention()}};
    const auto d = derive_fx_quotes(returns, expiries, {});
    REQUIRE(d);
    CHECK(d->expiries[0].has_value());
    REQUIRE_FALSE(d->expiries[1].has_value());
    CHECK(d->expiries[1].error().starts_with("2Y: "));
    CHECK_FALSE(derive_fx_quotes(std::vector<double>{0.01, -0.01, 0.02}, expiries, {}));
    const std::vector<fx_expiry> absurd{
        {.label = "1E9Y", .years = 1e9, .convention = derivation_convention()}};
    const auto far_quotes = derive_fx_quotes(returns, absurd, {});
    REQUIRE(far_quotes);
    CHECK_FALSE(far_quotes->expiries[0].has_value());
    CHECK_FALSE(derive_fx_quotes(returns, expiries, {.trading_days_per_year = 0.0}));
}
