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
#include "ores.analytics.quant/math/fx_smile_quotes.hpp"
#include "ores.analytics.quant/math/normal.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>

using namespace ores::analytics::quant::math;

namespace {

const fx_market no_rates{.spot = 1.0, .domestic_discount = 1.0, .foreign_discount = 1.0};
const fx_market with_rates{.spot = 1.0842, .domestic_discount = 0.9752, .foreign_discount = 0.9851};

fx_quote_convention smile_convention_of(fx_delta_type delta,
                                        fx_butterfly_style butterfly = fx_butterfly_style::smile,
                                        fx_atm_type atm = fx_atm_type::delta_neutral) {
    return {.atm = atm,
            .delta = delta,
            .favour = fx_risk_reversal_favour::call,
            .butterfly = butterfly};
}

/// The strangle at a pillar delta, priced once at one volatility and once on the smile.
std::pair<double, double> broker_strangle(const gram_charlier_smile& smile,
                                          const fx_market& m,
                                          fx_delta_type type,
                                          double delta,
                                          double flat) {
    const double root_t = std::sqrt(smile.years);
    const double f = m.forward();
    const double s = flat * root_t;
    const double kc = *fx_strike_from_delta(type, option_side::call, m, s, delta);
    const double kp = *fx_strike_from_delta(type, option_side::put, m, s, -delta);
    const double at_flat = black_forward_price(option_side::call, f, kc, s) +
                           black_forward_price(option_side::put, f, kp, s);
    const double on_smile =
        black_forward_price(option_side::call, f, kc, smile.volatility_at_strike(f, kc) * root_t) +
        black_forward_price(option_side::put, f, kp, smile.volatility_at_strike(f, kp) * root_t);
    return {at_flat, on_smile};
}

}

TEST_CASE("the smile is the paper's equation 16 in d", "[fx_smile_quotes]") {
    const gram_charlier_smile smile{
        .sigma = 0.1, .skewness = -0.3, .excess_kurtosis = 0.6, .years = 0.5};
    CHECK(smile.volatility_at_d(0.0) == Catch::Approx(0.1 * (1.0 - 0.6 / 24.0)).epsilon(1e-15));
    CHECK(smile.volatility_at_d(1.0) == Catch::Approx(0.1 * (1.0 + 0.3 / 6.0)).epsilon(1e-15));
    const double s = 0.1 * std::sqrt(0.5);
    const double k = std::exp(0.5 * s * s);
    CHECK(smile.volatility_at_strike(1.0, k) == Catch::Approx(smile.volatility_at_d(0.0)));
}

TEST_CASE("a flat smile quotes its level at the money and nothing in the wings",
          "[fx_smile_quotes]") {
    const gram_charlier_smile flat{
        .sigma = 0.12, .skewness = 0.0, .excess_kurtosis = 0.0, .years = 1.0};
    for (const auto type : {fx_delta_type::spot,
                            fx_delta_type::forward,
                            fx_delta_type::premium_adjusted_spot,
                            fx_delta_type::premium_adjusted_forward})
        for (const auto style : {fx_butterfly_style::smile, fx_butterfly_style::broker}) {
            const auto q = quote_smile(flat, with_rates, smile_convention_of(type, style));
            REQUIRE(q);
            CHECK(q->atm == Catch::Approx(0.12).epsilon(1e-14));
            CHECK(std::abs(q->rr25) < 1e-14);
            CHECK(std::abs(q->rr10) < 1e-14);
            CHECK(std::abs(q->bf25) < 1e-12);
            CHECK(std::abs(q->bf10) < 1e-12);
        }
}

TEST_CASE("the pillars follow the paper's coefficients for small moments", "[fx_smile_quotes]") {
    const double sigma = 0.1;
    const double skew = -0.1;
    const double kurt = 0.2;
    const gram_charlier_smile smile{
        .sigma = sigma, .skewness = skew, .excess_kurtosis = kurt, .years = 0.5};
    const auto q = quote_smile(smile, no_rates, smile_convention_of(fx_delta_type::forward));
    REQUIRE(q);
    const double d25 = normal_quantile(0.75);
    const double d10 = normal_quantile(0.90);
    CHECK(q->atm == Catch::Approx(sigma * (1.0 - kurt / 24.0)).epsilon(2e-3));
    CHECK(q->rr25 == Catch::Approx(sigma * skew * d25 / 3.0).epsilon(0.03));
    CHECK(q->bf25 == Catch::Approx(sigma * kurt * d25 * d25 / 24.0).epsilon(0.05));
    CHECK(q->rr10 == Catch::Approx(sigma * skew * d10 / 3.0).epsilon(0.05));
    CHECK(q->bf10 == Catch::Approx(sigma * kurt * d10 * d10 / 24.0).epsilon(0.08));
    CHECK(d25 / 3.0 == Catch::Approx(0.225).epsilon(1e-3));
    CHECK(d10 / 3.0 == Catch::Approx(0.427).epsilon(1e-3));
}

TEST_CASE("a broker butterfly reprices the broker strangle on the smile, as ORE's BF/RR surface "
          "test checks",
          "[fx_smile_quotes]") {
    const gram_charlier_smile smile{
        .sigma = 0.11, .skewness = -0.45, .excess_kurtosis = 1.2, .years = 0.75};
    for (const auto type : {fx_delta_type::spot,
                            fx_delta_type::forward,
                            fx_delta_type::premium_adjusted_spot,
                            fx_delta_type::premium_adjusted_forward}) {
        INFO("delta type " << static_cast<int>(type));
        const auto broker =
            quote_smile(smile, with_rates, smile_convention_of(type, fx_butterfly_style::broker));
        const auto plain =
            quote_smile(smile, with_rates, smile_convention_of(type, fx_butterfly_style::smile));
        REQUIRE(broker);
        REQUIRE(plain);
        for (const auto& [delta, bf] :
             {std::pair{0.25, broker->bf25}, std::pair{0.10, broker->bf10}}) {
            const auto [at_flat, on_smile] =
                broker_strangle(smile, with_rates, type, delta, broker->atm + bf);
            CHECK(at_flat == Catch::Approx(on_smile).epsilon(1e-10));
        }
        CHECK(broker->atm == plain->atm);
        CHECK(broker->rr25 == plain->rr25);
        CHECK(broker->rr10 == plain->rr10);
        CHECK(broker->bf25 != Catch::Approx(plain->bf25).epsilon(1e-6));
    }
}

TEST_CASE("a delta-neutral ATM is neutral at the smile's own volatility there",
          "[fx_smile_quotes]") {
    const gram_charlier_smile smile{
        .sigma = 0.15, .skewness = -0.5, .excess_kurtosis = 0.8, .years = 2.0};
    const double t = smile.years;
    const double f = with_rates.forward();
    const auto plain = quote_smile(smile, with_rates, smile_convention_of(fx_delta_type::forward));
    const auto adjusted = quote_smile(
        smile, with_rates, smile_convention_of(fx_delta_type::premium_adjusted_forward));
    const auto at_forward =
        quote_smile(smile,
                    with_rates,
                    smile_convention_of(
                        fx_delta_type::forward, fx_butterfly_style::smile, fx_atm_type::forward));
    REQUIRE(plain);
    REQUIRE(adjusted);
    REQUIRE(at_forward);
    const double v = plain->atm;
    CHECK(smile.volatility_at_strike(f, f * std::exp(0.5 * v * v * t)) ==
          Catch::Approx(v).epsilon(1e-12));
    const double w = adjusted->atm;
    CHECK(smile.volatility_at_strike(f, f * std::exp(-0.5 * w * w * t)) ==
          Catch::Approx(w).epsilon(1e-12));
    CHECK(at_forward->atm == Catch::Approx(smile.volatility_at_strike(f, f)).epsilon(1e-15));
    CHECK(plain->atm != Catch::Approx(adjusted->atm).epsilon(1e-6));
}

TEST_CASE("a risk reversal in favour of the put is the call's turned round", "[fx_smile_quotes]") {
    const gram_charlier_smile smile{
        .sigma = 0.09, .skewness = 0.35, .excess_kurtosis = 0.5, .years = 0.25};
    auto convention = smile_convention_of(fx_delta_type::spot, fx_butterfly_style::broker);
    const auto call = quote_smile(smile, with_rates, convention);
    convention.favour = fx_risk_reversal_favour::put;
    const auto put = quote_smile(smile, with_rates, convention);
    REQUIRE(call);
    REQUIRE(put);
    CHECK(call->rr25 > 0.0);
    CHECK(put->rr25 == -call->rr25);
    CHECK(put->rr10 == -call->rr10);
    CHECK(put->bf25 == call->bf25);
}

TEST_CASE("a smile with no level or no expiry is refused", "[fx_smile_quotes]") {
    const auto convention = smile_convention_of(fx_delta_type::forward);
    CHECK_FALSE(quote_smile({.sigma = 0.0, .skewness = 0.0, .excess_kurtosis = 0.0, .years = 1.0},
                            no_rates,
                            convention));
    CHECK_FALSE(quote_smile({.sigma = 0.1, .skewness = 0.0, .excess_kurtosis = 0.0, .years = 0.0},
                            no_rates,
                            convention));
}
