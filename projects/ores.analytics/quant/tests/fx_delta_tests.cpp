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
#include "ores.analytics.quant/math/fx_delta.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <vector>

using namespace ores::analytics::quant::math;

namespace {

struct delta_case {
    option_side side;
    fx_delta_type type;
    fx_market market;
    double stdev;
    double strike;
    double delta;
};

/// QuantLib's black delta calculator test, testDeltaValues: values from an R implementation,
/// and the JPY/USD case from Castagna, FX Options and Smile Risk (Wiley, 2009).
std::vector<delta_case> quantlib_delta_cases() {
    using enum fx_delta_type;
    const auto c = option_side::call;
    const auto p = option_side::put;
    const fx_market a{.spot = 1.421, .domestic_discount = 0.997306, .foreign_discount = 0.992266};
    const fx_market b{
        .spot = 122.121, .domestic_discount = 0.9695434, .foreign_discount = 0.9872347};
    const fx_market d{.spot = 3.4582, .domestic_discount = 0.99979, .foreign_discount = 0.9250616};
    const fx_market e{.spot = 103.00, .domestic_discount = 0.99482, .foreign_discount = 0.98508};
    return {
        {c, spot, a, 0.1180654, 1.608080, 0.15},
        {c, premium_adjusted_spot, a, 0.1180654, 1.600545, 0.15},
        {c, forward, a, 0.1180654, 1.609029, 0.15},
        {c, premium_adjusted_forward, a, 0.1180654, 1.601550, 0.15},
        {c, spot, b, 0.0887676, 119.8031, 0.67},
        {c, premium_adjusted_spot, b, 0.0887676, 117.7096, 0.67},
        {c, forward, b, 0.0887676, 120.0592, 0.67},
        {c, premium_adjusted_forward, b, 0.0887676, 118.0532, 0.67},
        {p, spot, d, 0.3199034, 4.964924, -0.821},
        {p, premium_adjusted_spot, d, 0.3199034, 3.778327, -0.821},
        {p, forward, d, 0.3199034, 4.51896, -0.821},
        {p, premium_adjusted_forward, d, 0.3199034, 3.65728, -0.821},
        {p, spot, e, 0.07247845, 97.47, -0.25},
        {p, premium_adjusted_spot, e, 0.07247845, 97.22, -0.25},
    };
}

double discounted_price(option_side side, const fx_market& m, double stdev, double strike) {
    return m.domestic_discount * black_forward_price(side, m.forward(), strike, stdev);
}

}

TEST_CASE("the delta of a strike matches QuantLib's delta calculator", "[fx_delta]") {
    for (const auto& c : quantlib_delta_cases()) {
        INFO("strike " << c.strike << ", delta type " << static_cast<int>(c.type));
        CHECK(std::abs(fx_delta(c.type, c.side, c.market, c.stdev, c.strike) - c.delta) < 1e-3);
    }
}

TEST_CASE("the strike of a delta matches QuantLib's delta calculator", "[fx_delta]") {
    for (const auto& c : quantlib_delta_cases()) {
        INFO("strike " << c.strike << ", delta type " << static_cast<int>(c.type));
        const auto k = fx_strike_from_delta(c.type, c.side, c.market, c.stdev, c.delta);
        REQUIRE(k);
        CHECK(std::abs(*k - c.strike) < 1e-2);
        CHECK(fx_delta(c.type, c.side, c.market, c.stdev, *k) ==
              Catch::Approx(c.delta).epsilon(1e-10));
    }
}

TEST_CASE("a spot delta is the price's sensitivity to the spot, and its premium-adjusted form "
          "subtracts the premium in the foreign currency",
          "[fx_delta]") {
    const fx_market m{.spot = 1.421, .domestic_discount = 0.997306, .foreign_discount = 0.992266};
    for (const auto side : {option_side::call, option_side::put})
        for (const double strike : {1.30, 1.42, 1.55}) {
            constexpr double h = 1e-6;
            fx_market up = m;
            fx_market down = m;
            up.spot += h;
            down.spot -= h;
            const double sens = (discounted_price(side, up, 0.12, strike) -
                                 discounted_price(side, down, 0.12, strike)) /
                                (2.0 * h);
            CHECK(fx_delta(fx_delta_type::spot, side, m, 0.12, strike) ==
                  Catch::Approx(sens).epsilon(1e-7));
            const double premium = discounted_price(side, m, 0.12, strike) / m.spot;
            CHECK(fx_delta(fx_delta_type::premium_adjusted_spot, side, m, 0.12, strike) ==
                  Catch::Approx(sens - premium).epsilon(1e-7));
        }
}

TEST_CASE("a call and a put at one strike satisfy put-call parity of their deltas", "[fx_delta]") {
    const fx_market m{
        .spot = 122.121, .domestic_discount = 0.9695434, .foreign_discount = 0.9872347};
    const double k = 120.0;
    const double s = 0.09;
    const auto gap = [&](fx_delta_type t) {
        return fx_delta(t, option_side::call, m, s, k) - fx_delta(t, option_side::put, m, s, k);
    };
    CHECK(gap(fx_delta_type::forward) == Catch::Approx(1.0).epsilon(1e-14));
    CHECK(gap(fx_delta_type::spot) == Catch::Approx(m.foreign_discount).epsilon(1e-14));
    CHECK(gap(fx_delta_type::premium_adjusted_forward) ==
          Catch::Approx(k / m.forward()).epsilon(1e-14));
    CHECK(gap(fx_delta_type::premium_adjusted_spot) ==
          Catch::Approx(m.foreign_discount * k / m.forward()).epsilon(1e-14));
}

TEST_CASE("the delta-neutral strike makes a call and a put delta sum to zero, as QuantLib checks",
          "[fx_delta]") {
    for (const auto& c : quantlib_delta_cases()) {
        const double k = fx_atm_strike(fx_atm_type::delta_neutral, c.type, c.market, c.stdev);
        const double sum = fx_delta(c.type, option_side::call, c.market, c.stdev, k) +
                           fx_delta(c.type, option_side::put, c.market, c.stdev, k);
        INFO("delta type " << static_cast<int>(c.type));
        CHECK(std::abs(sum) < 1e-12);
        CHECK(fx_atm_strike(fx_atm_type::forward, c.type, c.market, c.stdev) ==
              Catch::Approx(c.market.forward()).epsilon(1e-15));
    }
}

TEST_CASE("with no volatility the delta-neutral strike is the forward, as QuantLib checks",
          "[fx_delta]") {
    const fx_market m{.spot = 103.00, .domestic_discount = 0.99482, .foreign_discount = 0.98508};
    for (const auto t : {fx_delta_type::spot,
                         fx_delta_type::forward,
                         fx_delta_type::premium_adjusted_spot,
                         fx_delta_type::premium_adjusted_forward})
        CHECK(fx_atm_strike(fx_atm_type::delta_neutral, t, m, 0.0) == m.forward());
}

TEST_CASE("a premium-adjusted call takes the strike above its largest delta", "[fx_delta]") {
    const fx_market m{.spot = 1.0, .domestic_discount = 1.0, .foreign_discount = 1.0};
    const double s = 0.5;
    const auto k = fx_strike_from_delta(
        fx_delta_type::premium_adjusted_forward, option_side::call, m, s, 0.10);
    REQUIRE(k);
    constexpr double h = 1e-5;
    const double slope =
        (fx_delta(fx_delta_type::premium_adjusted_forward, option_side::call, m, s, *k + h) -
         fx_delta(fx_delta_type::premium_adjusted_forward, option_side::call, m, s, *k - h)) /
        (2.0 * h);
    CHECK(slope < 0.0);
    CHECK_FALSE(fx_strike_from_delta(
        fx_delta_type::premium_adjusted_forward, option_side::call, m, s, 0.95));
}

TEST_CASE("a delta of the wrong sign or beyond reach is refused", "[fx_delta]") {
    const fx_market m{.spot = 1.0, .domestic_discount = 1.0, .foreign_discount = 0.98};
    CHECK_FALSE(fx_strike_from_delta(fx_delta_type::forward, option_side::call, m, 0.1, -0.25));
    CHECK_FALSE(fx_strike_from_delta(fx_delta_type::forward, option_side::put, m, 0.1, 0.25));
    CHECK_FALSE(fx_strike_from_delta(fx_delta_type::spot, option_side::call, m, 0.1, 0.99));
    CHECK_FALSE(fx_strike_from_delta(fx_delta_type::forward, option_side::call, m, 0.0, 0.25));
}
