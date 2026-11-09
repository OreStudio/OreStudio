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
#include "ores.analytics.quant/service/ssvi_calibration_engine.hpp"
#include "ssvi_quantext_equity_smiles.hpp"
#include <algorithm>
#include <array>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/generators/catch_generators_range.hpp>
#include <cmath>
#include <cstddef>
#include <limits>
#include <string>
#include <utility>

using namespace ores::analytics::quant::math;
using namespace ores::analytics::quant::service;

namespace {

const fx_market market{.spot = 1.0842, .domestic_discount = 0.9752, .foreign_discount = 0.9851};

constexpr double true_rho = -0.3;
constexpr double true_eta = 0.9;
constexpr double true_gamma = 0.35;

double variance(double k, double theta, double rho, double eta, double gamma = 0.5) {
    return ssvi_total_variance(k, theta, rho, ssvi_phi(theta, eta, gamma));
}

struct tenor final {
    const char* id;
    double years;
    double theta;
};

const std::array<tenor, 4> tenors{tenor{"3M", 0.25, 0.0030},
                                  tenor{"6M", 0.5, 0.0064},
                                  tenor{"1Y", 1.0, 0.0140},
                                  tenor{"2Y", 2.0, 0.0300}};

fx_quote_convention
convention_of(fx_atm_type atm, fx_delta_type delta, fx_butterfly_style butterfly) {
    return {.atm = atm,
            .delta = delta,
            .favour = fx_risk_reversal_favour::call,
            .butterfly = butterfly};
}

const fx_quote_convention spot_smile =
    convention_of(fx_atm_type::forward, fx_delta_type::spot, fx_butterfly_style::smile);

ssvi_expiry_quotes quotes_of(const tenor& t, const fx_quote_convention& convention) {
    const double delta = 0.25;
    std::array<fx_wing_reading, 1> wings{};
    const auto atm = *read_smile(
        [&](double k) {
            return std::sqrt(variance(k, t.theta, true_rho, true_eta, true_gamma) / t.years);
        },
        std::sqrt(t.theta / t.years),
        t.years,
        market,
        convention,
        std::array{delta},
        wings);
    const std::string id(t.id);
    return {.expiry_id = id,
            .years = t.years,
            .market = market,
            .convention = convention,
            .pillars = {{.point_id = id + "/ATM",
                         .kind = ssvi_quote_kind::atm,
                         .delta = 0.0,
                         .contributions = {{atm.volatility, 1.0}}},
                        {.point_id = id + "/25RR",
                         .kind = ssvi_quote_kind::risk_reversal,
                         .delta = delta,
                         .contributions = {{wings[0].risk_reversal, 1.0}}},
                        {.point_id = id + "/25BF",
                         .kind = ssvi_quote_kind::butterfly,
                         .delta = delta,
                         .contributions = {{wings[0].butterfly, 1.0}}}}};
}

ssvi_request request_of(const fx_quote_convention& convention) {
    ssvi_request request;
    for (const auto& t : tenors)
        request.expiries.push_back(quotes_of(t, convention));
    request.pillar_tolerance = 1e-5;
    return request;
}

void check_recovers_the_surface(const fx_quote_convention& convention) {
    const auto fit = calibrate_ssvi(request_of(convention));
    REQUIRE(fit.has_value());
    CHECK(fit->converged);
    CHECK(fit->parameters.rho == Catch::Approx(true_rho).margin(1e-3));
    CHECK(fit->parameters.eta == Catch::Approx(true_eta).margin(1e-3));
    CHECK(fit->parameters.gamma == Catch::Approx(true_gamma).margin(1e-3));
    REQUIRE(fit->parameters.expiries.size() == tenors.size());
    for (std::size_t i = 0; i < tenors.size(); ++i)
        CHECK(fit->parameters.expiries[i].atm_total_variance ==
              Catch::Approx(tenors[i].theta).epsilon(1e-4));
    CHECK(std::ranges::none_of(fit->pillars, &ssvi_pillar_fit::beyond_tolerance));
    CHECK(fit->violations.empty());
}

}

TEST_CASE("the total variance is the published formula", "[ssvi_calibration_engine]") {
    CHECK(variance(0.0, 0.04, -0.4, 1.0) == Catch::Approx(0.04));
    CHECK(variance(0.2, 0.04, 0.0, 1.0) == Catch::Approx(variance(-0.2, 0.04, 0.0, 1.0)));
    const double phi = 1.0 / 0.2;
    const double x = phi * 0.1 - 0.4;
    CHECK(variance(0.1, 0.04, -0.4, 1.0) ==
          Catch::Approx(0.02 * (1.0 - 0.4 * phi * 0.1 + std::sqrt(x * x + 1.0 - 0.16))));
}

TEST_CASE("an SSVI slice is the raw SVI slice of Gatheral and Jacquier, read as QuantLib reads it",
          "[ssvi_calibration_engine]") {
    // QuantLib's testSviSmileSection checks w(m) = a + b * sigma; its rho is used here.
    const double theta = 0.04;
    const double rho = 0.439;
    const double eta = 1.2;
    const double phi = eta / std::sqrt(theta);
    const double a = 0.5 * theta * (1.0 - rho * rho);
    const double b = 0.5 * theta * phi;
    const double m = -rho / phi;
    const double sigma = std::sqrt(1.0 - rho * rho) / phi;
    CHECK(variance(m, theta, rho, eta) == Catch::Approx(a + b * sigma).epsilon(1e-10));
    for (const double k : {-0.3, -0.1, 0.05, 0.2}) {
        const double raw = a + b * (rho * (k - m) + std::sqrt((k - m) * (k - m) + sigma * sigma));
        CHECK(variance(k, theta, rho, eta) == Catch::Approx(raw).epsilon(1e-10));
    }
}

TEST_CASE("quotes generated from a surface fit back to that surface", "[ssvi_calibration_engine]") {
    SECTION("forward ATM, spot delta, smile butterfly") {
        check_recovers_the_surface(spot_smile);
    }
    SECTION("delta-neutral ATM, premium-adjusted delta, smile butterfly") {
        check_recovers_the_surface(convention_of(fx_atm_type::delta_neutral,
                                                 fx_delta_type::premium_adjusted_spot,
                                                 fx_butterfly_style::smile));
    }
    SECTION("forward ATM, forward delta, broker butterfly") {
        check_recovers_the_surface(convention_of(
            fx_atm_type::forward, fx_delta_type::forward, fx_butterfly_style::broker));
    }
}

TEST_CASE("the same butterfly number gives a different surface as a broker strangle",
          "[ssvi_calibration_engine]") {
    auto as_broker = request_of(spot_smile);
    for (auto& expiry : as_broker.expiries)
        expiry.convention =
            convention_of(fx_atm_type::forward, fx_delta_type::spot, fx_butterfly_style::broker);
    const auto smile = calibrate_ssvi(request_of(spot_smile));
    const auto broker = calibrate_ssvi(as_broker);
    REQUIRE(smile.has_value());
    REQUIRE(broker.has_value());
    CHECK(std::abs(smile->parameters.eta - broker->parameters.eta) > 1e-3);
}

TEST_CASE("a pillar's quote is the weighted mean of its contributions",
          "[ssvi_calibration_engine]") {
    auto request = request_of(spot_smile);
    auto& contributions = request.expiries[0].pillars[0].contributions;
    const double base = contributions[0].volatility;
    contributions = {{base - 0.002, 1.0}, {base + 0.002, 3.0}};
    const auto fit = calibrate_ssvi(request);
    REQUIRE(fit.has_value());
    CHECK(fit->pillars[0].quoted == Catch::Approx(base + 0.001));
}

TEST_CASE("each strike label resolves through the forward and the conventions",
          "[ssvi_calibration_engine]") {
    const auto fit = calibrate_ssvi(request_of(spot_smile));
    REQUIRE(fit.has_value());
    const auto& atm = fit->pillars[0];
    REQUIRE(atm.strikes.size() == 1);
    CHECK(atm.strikes[0] == Catch::Approx(market.forward()));

    const auto& rr = fit->pillars[1];
    REQUIRE(rr.strikes.size() == 2);
    const auto stdev_at = [&](double strike) {
        return std::sqrt(variance(std::log(strike / market.forward()),
                                  fit->parameters.expiries[0].atm_total_variance,
                                  fit->parameters.rho,
                                  fit->parameters.eta,
                                  fit->parameters.gamma));
    };
    CHECK(fx_delta(fx_delta_type::spot,
                   option_side::put,
                   market,
                   stdev_at(rr.strikes[0]),
                   rr.strikes[0]) == Catch::Approx(-0.25).margin(1e-8));
    CHECK(fx_delta(fx_delta_type::spot,
                   option_side::call,
                   market,
                   stdev_at(rr.strikes[1]),
                   rr.strikes[1]) == Catch::Approx(0.25).margin(1e-8));
}

TEST_CASE("the grid interpolates between expiries in total variance", "[ssvi_calibration_engine]") {
    auto request = request_of(spot_smile);
    request.grid_expiries = {{"9M", 0.75}};
    request.grid_strikes = {{"ATMF", 0.0}};
    const auto fit = calibrate_ssvi(request);
    REQUIRE(fit.has_value());
    REQUIRE(fit->grid.size() == 1);
    const double midpoint = 0.5 * (tenors[1].theta + tenors[2].theta);
    CHECK(fit->grid[0].total_variance == Catch::Approx(midpoint).epsilon(1e-3));
    CHECK(fit->grid[0].volatility == Catch::Approx(std::sqrt(midpoint / 0.75)).epsilon(1e-3));
}

TEST_CASE("a grid expiry outside the quotes is refused", "[ssvi_calibration_engine]") {
    auto request = request_of(spot_smile);
    request.grid_expiries = {{"5Y", 5.0}};
    request.grid_strikes = {{"ATMF", 0.0}};
    const auto fit = calibrate_ssvi(request);
    REQUIRE_FALSE(fit.has_value());
    CHECK(fit.error().find("5Y") != std::string::npos);
}

TEST_CASE("a grid expiry with no time is refused", "[ssvi_calibration_engine]") {
    auto request = request_of(spot_smile);
    request.grid_expiries = {{"NaN", std::numeric_limits<double>::quiet_NaN()}};
    request.grid_strikes = {{"ATMF", 0.0}};
    const auto fit = calibrate_ssvi(request);
    REQUIRE_FALSE(fit.has_value());
    CHECK(fit.error().find("NaN") != std::string::npos);
}

TEST_CASE("each arbitrage condition is detected", "[ssvi_calibration_engine]") {
    SECTION("a surface inside the region breaks nothing") {
        CHECK(ssvi_violations({.rho = -0.3,
                               .eta = 1.0,
                               .gamma = 0.5,
                               .expiries = {{"1Y", 1.0, 0.01}, {"2Y", 2.0, 0.02}}})
                  .empty());
    }
    SECTION("falling ATM total variance is a calendar spread") {
        const auto found = ssvi_violations({.rho = -0.3,
                                            .eta = 1.0,
                                            .gamma = 0.5,
                                            .expiries = {{"1Y", 1.0, 0.02}, {"2Y", 2.0, 0.01}}});
        REQUIRE(found.size() == 1);
        CHECK(found[0].kind == ssvi_arbitrage_kind::calendar);
        CHECK(found[0].expiry_id == "2Y");
    }
    SECTION("a large total variance breaks the level condition") {
        const auto found = ssvi_violations(
            {.rho = 0.5, .eta = 1.4, .gamma = 0.5, .expiries = {{"30Y", 30.0, 4.0}}});
        REQUIRE(found.size() == 1);
        CHECK(found[0].kind == ssvi_arbitrage_kind::butterfly);
        CHECK(found[0].expiry_id == "30Y");
    }
    SECTION("a large eta breaks the curvature condition") {
        const auto found = ssvi_violations(
            {.rho = 0.5, .eta = 2.0, .gamma = 0.5, .expiries = {{"1Y", 1.0, 0.01}}});
        REQUIRE(found.size() == 1);
        CHECK(found[0].kind == ssvi_arbitrage_kind::butterfly);
        CHECK(found[0].expiry_id == "1Y");
    }
}

TEST_CASE("a fit reports the calendar arbitrage its quotes imply", "[ssvi_calibration_engine]") {
    auto request = request_of(spot_smile);
    auto& atm = request.expiries[3].pillars[0].contributions[0].volatility;
    atm = std::sqrt(0.5 * tenors[2].theta / tenors[3].years);
    const auto fit = calibrate_ssvi(request);
    REQUIRE(fit.has_value());
    CHECK(std::ranges::any_of(fit->violations, [](const ssvi_violation& v) {
        return v.kind == ssvi_arbitrage_kind::calendar && v.expiry_id == "2Y";
    }));
}

TEST_CASE("a pillar beyond the tolerance is reported by name", "[ssvi_calibration_engine]") {
    auto request = request_of(spot_smile);
    request.pillar_tolerance = 1e-3;
    request.expiries[1].pillars[2].contributions[0].volatility += 0.02;
    const auto fit = calibrate_ssvi(request);
    REQUIRE(fit.has_value());
    const auto found = std::ranges::find(fit->pillars, "6M/25BF", &ssvi_pillar_fit::point_id);
    REQUIRE(found != fit->pillars.end());
    CHECK(found->beyond_tolerance);
}

TEST_CASE("an expiry short of its pillars is refused by name", "[ssvi_calibration_engine]") {
    SECTION("no butterfly") {
        auto request = request_of(spot_smile);
        request.expiries[2].pillars.pop_back();
        const auto fit = calibrate_ssvi(request);
        REQUIRE_FALSE(fit.has_value());
        CHECK(fit.error().find("1Y") != std::string::npos);
        CHECK(fit.error().find("butterfly") != std::string::npos);
    }
    SECTION("an ATM with no contribution counts as absent") {
        auto request = request_of(spot_smile);
        request.expiries[0].pillars[0].contributions.clear();
        const auto fit = calibrate_ssvi(request);
        REQUIRE_FALSE(fit.has_value());
        CHECK(fit.error().find("3M") != std::string::npos);
        CHECK(fit.error().find("ATM") != std::string::npos);
    }
}

namespace {

/// The bounds QuantExt's testCalibratedSsvi states for Gatheral2012SsviPowerLaw.
constexpr double quantext_rmse_bound = 0.071;
constexpr double quantext_quote_error_bound = 0.36;

ssvi_request quantext_request() {
    ssvi_request request;
    for (std::size_t e = 0; e < quantext_equity_smiles.size(); ++e) {
        const auto& smile = quantext_equity_smiles[e];
        ssvi_expiry_quotes expiry{.expiry_id = std::to_string(e),
                                  .years = smile.years,
                                  .market = {smile.forward, 1.0, 1.0},
                                  .convention = spot_smile,
                                  .pillars = {}};
        for (std::size_t i = 0; i < smile.strikes.size(); ++i)
            expiry.pillars.push_back({.point_id = expiry.expiry_id + "/" + std::to_string(i),
                                      .kind = ssvi_quote_kind::strike,
                                      .delta = 0.0,
                                      .log_moneyness = std::log(smile.strikes[i] / smile.forward),
                                      .contributions = {{smile.volatilities[i], 1.0}}});
        request.expiries.push_back(std::move(expiry));
    }
    request.pillar_tolerance = 1.0;
    return request;
}

double call_on_forward(const ssvi_parameters& surface, std::size_t expiry, double moneyness) {
    const double w = variance(std::log(moneyness),
                              surface.expiries[expiry].atm_total_variance,
                              surface.rho,
                              surface.eta,
                              surface.gamma);
    return black_forward_price(option_side::call, 1.0, moneyness, std::sqrt(w));
}

}

TEST_CASE("the equity smiles of QuantExt's testCalibratedSsvi fit within its bounds",
          "[ssvi_calibration_engine]") {
    const auto fit = calibrate_ssvi(quantext_request());
    REQUIRE(fit.has_value());
    CHECK(fit->converged);
    std::size_t first = 0;
    for (const auto& smile : quantext_equity_smiles) {
        const double largest = *std::ranges::max_element(smile.volatilities);
        double squares = 0.0;
        for (std::size_t i = 0; i < smile.strikes.size(); ++i) {
            const auto& pillar = fit->pillars[first + i];
            CHECK(std::abs(pillar.error / pillar.quoted) < quantext_quote_error_bound);
            squares += (pillar.error / largest) * (pillar.error / largest);
        }
        CHECK(std::sqrt(squares / static_cast<double>(smile.strikes.size())) < quantext_rmse_bound);
        first += smile.strikes.size();
    }
}

TEST_CASE("the fit of QuantExt's equity smiles has no static arbitrage on its moneyness grid",
          "[ssvi_calibration_engine]") {
    const auto fit = calibrate_ssvi(quantext_request());
    REQUIRE(fit.has_value());
    CHECK(fit->violations.empty());
    constexpr int points = 100;
    constexpr double low = 0.50;
    constexpr double high = 1.80;
    const double step = (high - low) / (points - 1);
    const auto& surface = fit->parameters;
    for (std::size_t e = 0; e < surface.expiries.size(); ++e) {
        for (int i = 1; i + 1 < points; ++i) {
            const double m = low + i * step;
            const double convexity = call_on_forward(surface, e, m - step) -
                                     2.0 * call_on_forward(surface, e, m) +
                                     call_on_forward(surface, e, m + step);
            CHECK(convexity > -1e-10);
            if (e > 0)
                CHECK(call_on_forward(surface, e, m) >= call_on_forward(surface, e - 1, m) - 1e-10);
        }
    }
}

TEST_CASE("three strike pillars fix a slice, and two do not", "[ssvi_calibration_engine]") {
    auto request = quantext_request();
    request.expiries[0].pillars.resize(2);
    const auto refused = calibrate_ssvi(request);
    REQUIRE_FALSE(refused.has_value());
    CHECK(refused.error().find("expiry 0") != std::string::npos);
}

namespace {

struct bfrr_slice final {
    const char* expiry_id;
    double years;
    fx_delta_type delta_type;
    double delta;
    double atm;
    double risk_reversal;
    double butterfly;

    [[nodiscard]] fx_market market() const {
        return {.spot = 1.2,
                .domestic_discount = std::exp(-0.01 * years),
                .foreign_discount = std::exp(-0.015 * years)};
    }
};

/// BFRRVolFixture of QuantExt's bfrrvolsurface.cpp. An expiry past its two-year switch quotes
/// forward delta. Each slice is fitted alone, because SSVI shares rho, eta and gamma across
/// expiries and a three-quote slice is only then an exact fit.
constexpr std::array bfrr_slices{
    bfrr_slice{"1Y", 1.0, fx_delta_type::premium_adjusted_spot, 0.10, 0.09, -0.015, 0.02},
    bfrr_slice{"1Y", 1.0, fx_delta_type::premium_adjusted_spot, 0.25, 0.09, -0.012, 0.01},
    bfrr_slice{"3Y", 3.0, fx_delta_type::premium_adjusted_forward, 0.10, 0.08, -0.011, 0.01},
    bfrr_slice{"3Y", 3.0, fx_delta_type::premium_adjusted_forward, 0.25, 0.08, -0.009, 0.005}};

ssvi_calibration bfrr_fit(const bfrr_slice& slice, fx_butterfly_style butterfly) {
    ssvi_request request;
    request.expiries = {
        {.expiry_id = slice.expiry_id,
         .years = slice.years,
         .market = slice.market(),
         .convention = convention_of(fx_atm_type::delta_neutral, slice.delta_type, butterfly),
         .pillars = {{.point_id = "ATM",
                      .kind = ssvi_quote_kind::atm,
                      .delta = 0.0,
                      .log_moneyness = 0.0,
                      .contributions = {{slice.atm, 1.0}}},
                     {.point_id = "RR",
                      .kind = ssvi_quote_kind::risk_reversal,
                      .delta = slice.delta,
                      .log_moneyness = 0.0,
                      .contributions = {{slice.risk_reversal, 1.0}}},
                     {.point_id = "BF",
                      .kind = ssvi_quote_kind::butterfly,
                      .delta = slice.delta,
                      .log_moneyness = 0.0,
                      .contributions = {{slice.butterfly, 1.0}}}}}};
    request.pillar_tolerance = 1e-5;
    const auto fit = calibrate_ssvi(request);
    REQUIRE(fit.has_value());
    return *fit;
}

double fitted_volatility(const ssvi_calibration& fit, const bfrr_slice& slice, double strike) {
    return std::sqrt(variance(std::log(strike / slice.market().forward()),
                              fit.parameters.expiries[0].atm_total_variance,
                              fit.parameters.rho,
                              fit.parameters.eta,
                              fit.parameters.gamma) /
                     slice.years);
}

}

TEST_CASE("QuantExt's testSmileBF identities hold on the fitted smile",
          "[ssvi_calibration_engine]") {
    const auto slice = GENERATE(from_range(bfrr_slices));
    CAPTURE(slice.expiry_id, slice.delta);
    const auto fit = bfrr_fit(slice, fx_butterfly_style::smile);
    const auto vol = [&](double strike) {
        return fitted_volatility(fit, slice, strike);
    };
    CHECK(vol(fit.pillars[0].strikes[0]) == Catch::Approx(slice.atm).margin(1e-5));
    CHECK(vol(fit.pillars[2].strikes[0]) ==
          Catch::Approx(slice.atm + slice.butterfly - 0.5 * slice.risk_reversal).margin(1e-5));
    CHECK(vol(fit.pillars[2].strikes[1]) ==
          Catch::Approx(slice.atm + slice.butterfly + 0.5 * slice.risk_reversal).margin(1e-5));
}

TEST_CASE("QuantExt's testBrokerBF premium identity holds on the fitted smile",
          "[ssvi_calibration_engine]") {
    const auto slice = GENERATE(from_range(bfrr_slices));
    CAPTURE(slice.expiry_id, slice.delta);
    const auto fit = bfrr_fit(slice, fx_butterfly_style::broker);
    const auto market = slice.market();
    const double forward = market.forward();
    const double root_years = std::sqrt(slice.years);
    const double flat = (slice.atm + slice.butterfly) * root_years;
    const auto vol = [&](double strike) {
        return fitted_volatility(fit, slice, strike);
    };
    const double kp =
        *fx_strike_from_delta(slice.delta_type, option_side::put, market, flat, -slice.delta);
    const double kc =
        *fx_strike_from_delta(slice.delta_type, option_side::call, market, flat, slice.delta);
    const double broker = black_forward_price(option_side::put, forward, kp, flat) +
                          black_forward_price(option_side::call, forward, kc, flat);
    const double on_smile =
        black_forward_price(option_side::put, forward, kp, vol(kp) * root_years) +
        black_forward_price(option_side::call, forward, kc, vol(kc) * root_years);
    CHECK(on_smile == Catch::Approx(broker).margin(1e-5));
    CHECK(vol(fit.pillars[1].strikes[1]) - vol(fit.pillars[1].strikes[0]) ==
          Catch::Approx(slice.risk_reversal).margin(1e-5));
    CHECK(vol(fit.pillars[0].strikes[0]) == Catch::Approx(slice.atm).margin(1e-5));
}

TEST_CASE("a published SSVI slice gives its published variance, skew and raw SVI form",
          "[ssvi_calibration_engine]") {
    // The 1Y hand check of riskhub.org, "SSVI and Surface-Wide Parameterization".
    const double theta = 0.028704;
    const double phi = 5.541;
    const double rho = -0.618;
    const double eta = phi * std::sqrt(theta);
    CHECK(variance(-0.2, theta, rho, eta) == Catch::Approx(0.05140).margin(5e-6));

    const double a = 0.5 * theta * (1.0 - rho * rho);
    const double b = 0.5 * theta * phi;
    CHECK(a == Catch::Approx(0.00887).margin(5e-6));
    CHECK(b == Catch::Approx(0.0795).margin(5e-5));
    CHECK(-rho / phi == Catch::Approx(0.1115).margin(5e-5));
    CHECK(std::sqrt(1.0 - rho * rho) / phi == Catch::Approx(0.142).margin(5e-4));

    const double h = 1e-5;
    const auto volatility = [&](double k) {
        return std::sqrt(variance(k, theta, rho, eta));
    };
    const double skew = (volatility(h) - volatility(-h)) / (2.0 * h);
    CHECK(skew == Catch::Approx(0.5 * rho * eta).epsilon(1e-6));
    CHECK(skew == Catch::Approx(-0.290).margin(5e-4));
}
