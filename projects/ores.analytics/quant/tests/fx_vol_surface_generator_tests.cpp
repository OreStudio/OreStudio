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
#include "ores.analytics.quant/service/fx_vol_surface_generator.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstddef>
#include <random>
#include <vector>

using namespace ores::analytics::quant::math;
using namespace ores::analytics::quant::service;

namespace {

const fx_market eur_usd{.spot = 1.0842, .domestic_discount = 0.9752, .foreign_discount = 0.9851};
const fx_quote_convention eur_usd_convention{.atm = fx_atm_type::delta_neutral,
                                             .delta = fx_delta_type::spot,
                                             .favour = fx_risk_reversal_favour::call,
                                             .butterfly = fx_butterfly_style::smile};
constexpr double one_day = 1.0 / 252.0;

/// A EUR/USD surface from 1W to 1Y, ATM near 7% and rising to 8%, with a light put skew.
ssvi_parameters start_surface() {
    ssvi_parameters p;
    p.rho = -0.1;
    p.eta = 0.8;
    p.gamma = 0.5;
    const std::vector<std::pair<const char*, double>> expiries{
        {"1W", 7.0 / 365.0}, {"1M", 1.0 / 12.0}, {"3M", 0.25}, {"6M", 0.5}, {"1Y", 1.0}};
    double vol = 0.070;
    for (const auto& [id, years] : expiries) {
        p.expiries.push_back(
            {.expiry_id = id, .years = years, .atm_total_variance = vol * vol * years});
        vol += 0.0025;
    }
    return p;
}

/// Spot moves the ATM level by 0.3 and the skew by -0.4; neighbouring segments move together.
fx_vol_surface_dynamics dynamics_for(std::size_t segments) {
    fx_vol_surface_dynamics d;
    d.variance_reversion.assign(segments, 2.0);
    d.variance_volatility.assign(segments, 0.08);
    d.skew_reversion = 3.0;
    d.skew_volatility = 0.6;
    d.curvature_reversion = 3.0;
    d.curvature_volatility = 0.4;
    const auto n = static_cast<Eigen::Index>(segments + 3);
    d.correlation = Eigen::MatrixXd::Identity(n, n);
    for (Eigen::Index i = 1; i <= static_cast<Eigen::Index>(segments); ++i) {
        d.correlation(0, i) = d.correlation(i, 0) = 0.3;
        for (Eigen::Index j = 1; j < i; ++j)
            d.correlation(i, j) = d.correlation(j, i) = std::pow(0.9, static_cast<double>(i - j));
    }
    d.correlation(0, n - 2) = d.correlation(n - 2, 0) = -0.4;
    return d;
}

fx_vol_surface_generator make_generator() {
    const auto start = start_surface();
    auto g = fx_vol_surface_generator::make(start, dynamics_for(start.expiries.size()), one_day);
    REQUIRE(g);
    return std::move(*g);
}

rng_stream stream_of(std::uint64_t series) {
    return rng_stream(
        stream_key{.run_seed = 42, .series = series, .purpose = draw_purpose::market});
}

double correlation_of(const std::vector<double>& x, const std::vector<double>& y) {
    const auto n = static_cast<double>(x.size());
    double mx = 0.0, my = 0.0;
    for (std::size_t i = 0; i < x.size(); ++i) {
        mx += x[i] / n;
        my += y[i] / n;
    }
    double sxy = 0.0, sxx = 0.0, syy = 0.0;
    for (std::size_t i = 0; i < x.size(); ++i) {
        sxy += (x[i] - mx) * (y[i] - my);
        sxx += (x[i] - mx) * (x[i] - mx);
        syy += (y[i] - my) * (y[i] - my);
    }
    return sxy / std::sqrt(sxx * syy);
}

}

TEST_CASE("the generator starts at the surface it is given, with gamma one half",
          "[fx_vol_surface_generator]") {
    const auto start = start_surface();
    const auto g = make_generator();
    const auto& s = g.surface();
    CHECK(s.rho == Catch::Approx(start.rho));
    CHECK(s.eta == Catch::Approx(start.eta));
    CHECK(s.gamma == 0.5);
    REQUIRE(s.expiries.size() == start.expiries.size());
    for (std::size_t i = 0; i < s.expiries.size(); ++i)
        CHECK(s.expiries[i].atm_total_variance ==
              Catch::Approx(start.expiries[i].atm_total_variance));
}

TEST_CASE("every surface the generator emits is free of static arbitrage",
          "[fx_vol_surface_generator]") {
    auto g = make_generator();
    auto spot = stream_of(1);
    auto factors = stream_of(2);
    std::normal_distribution<double> normal;
    for (int step = 0; step < 5000; ++step) {
        const auto& s = g.step(normal(spot), factors);
        REQUIRE(ssvi_violations(s).empty());
        REQUIRE(std::abs(s.rho) < 1.0);
        REQUIRE(s.eta > 0.0);
        REQUIRE(s.eta < std::sqrt(2.0));
        REQUIRE(s.expiries.back().atm_total_variance < 2.0);
        for (std::size_t i = 1; i < s.expiries.size(); ++i)
            REQUIRE(s.expiries[i].atm_total_variance > s.expiries[i - 1].atm_total_variance);
    }
}

TEST_CASE("the surface moves with spot by the configured correlation",
          "[fx_vol_surface_generator]") {
    auto g = make_generator();
    auto spot = stream_of(3);
    auto factors = stream_of(4);
    std::normal_distribution<double> normal;
    std::vector<double> shocks, level_moves, skew_moves;
    double theta = g.surface().expiries.front().atm_total_variance;
    double rho = g.surface().rho;
    for (int step = 0; step < 20000; ++step) {
        const double z = normal(spot);
        const auto& s = g.step(z, factors);
        shocks.push_back(z);
        level_moves.push_back(s.expiries.front().atm_total_variance - theta);
        skew_moves.push_back(s.rho - rho);
        theta = s.expiries.front().atm_total_variance;
        rho = s.rho;
    }
    CHECK(correlation_of(shocks, level_moves) == Catch::Approx(0.3).margin(0.03));
    CHECK(correlation_of(shocks, skew_moves) == Catch::Approx(-0.4).margin(0.03));
}

TEST_CASE("the generator refuses a start or dynamics it cannot hold",
          "[fx_vol_surface_generator]") {
    const auto start = start_surface();
    const auto d = dynamics_for(start.expiries.size());

    auto flat = start;
    flat.expiries[2].atm_total_variance = flat.expiries[1].atm_total_variance;
    CHECK(!fx_vol_surface_generator::make(flat, d, one_day));

    auto steep = start;
    steep.eta = 1.5;
    CHECK(!fx_vol_surface_generator::make(steep, d, one_day));

    auto short_dynamics = d;
    short_dynamics.variance_reversion.pop_back();
    CHECK(!fx_vol_surface_generator::make(start, short_dynamics, one_day));

    auto not_definite = d;
    not_definite.correlation(0, 1) = not_definite.correlation(1, 0) = 0.99;
    not_definite.correlation(0, 2) = not_definite.correlation(2, 0) = -0.99;
    CHECK(!fx_vol_surface_generator::make(start, not_definite, one_day));

    CHECK(!fx_vol_surface_generator::make(start, d, 0.0));
}

TEST_CASE("the clean quotes of a surface calibrate back to it", "[fx_vol_surface_generator]") {
    const auto surface = make_generator().surface();
    const std::vector<double> deltas{0.25, 0.10};
    std::vector<fx_vol_slice_market> markets;
    for (const auto& e : surface.expiries)
        markets.push_back(
            {.expiry_id = e.expiry_id, .market = eur_usd, .convention = eur_usd_convention});
    const auto quotes = fx_vol_surface_quotes(surface, markets, deltas);
    REQUIRE(quotes);

    ssvi_request request;
    for (std::size_t i = 0; i < quotes->size(); ++i) {
        const auto& q = (*quotes)[i];
        ssvi_expiry_quotes e{.expiry_id = q.expiry_id,
                             .years = surface.expiries[i].years,
                             .market = eur_usd,
                             .convention = eur_usd_convention,
                             .pillars = {}};
        e.pillars.push_back({.point_id = q.expiry_id + " ATM",
                             .kind = ssvi_quote_kind::atm,
                             .contributions = {{.volatility = q.atm}}});
        for (std::size_t d = 0; d < deltas.size(); ++d) {
            e.pillars.push_back({.point_id = q.expiry_id + " RR",
                                 .kind = ssvi_quote_kind::risk_reversal,
                                 .delta = deltas[d],
                                 .contributions = {{.volatility = q.risk_reversals[d]}}});
            e.pillars.push_back({.point_id = q.expiry_id + " BF",
                                 .kind = ssvi_quote_kind::butterfly,
                                 .delta = deltas[d],
                                 .contributions = {{.volatility = q.butterflies[d]}}});
        }
        request.expiries.push_back(std::move(e));
    }
    request.pillar_tolerance = 1e-4;
    const auto fit = calibrate_ssvi(request);
    REQUIRE(fit);
    CHECK(fit->parameters.rho == Catch::Approx(surface.rho).margin(1e-3));
    for (std::size_t i = 0; i < surface.expiries.size(); ++i)
        CHECK(fit->parameters.expiries[i].atm_total_variance ==
              Catch::Approx(surface.expiries[i].atm_total_variance).epsilon(1e-3));
    for (const auto& p : fit->pillars)
        CHECK(std::abs(p.error) < 1e-4);
}

TEST_CASE("the clean contributor publishes the truth", "[fx_vol_surface_generator]") {
    auto stream = stream_of(5);
    const fx_vol_noise_profile none{};
    for (const double clean : {0.071, -0.004, 0.0021}) {
        const auto seen = observe_fx_vol_point(clean, 0.25, 0.5, 0.07, none, stream);
        REQUIRE(seen.value);
        CHECK(*seen.value == clean);
        CHECK(!seen.stale);
    }
}

TEST_CASE("a noisy contributor differs from the truth by its profile",
          "[fx_vol_surface_generator]") {
    const fx_vol_noise_profile profile{.noise = 0.002,
                                       .wing_widening = 0.0,
                                       .expiry_widening = 0.0,
                                       .quote_step = 0.0,
                                       .missing_probability = 0.1,
                                       .stale_probability = 0.0,
                                       .outlier_probability = 0.0,
                                       .outlier_size = 0.0,
                                       .bias = 0.001};
    auto stream = stream_of(6);
    constexpr int draws = 20000;
    int missing = 0;
    double sum = 0.0, sum_squares = 0.0;
    int seen = 0;
    for (int i = 0; i < draws; ++i) {
        const auto o = observe_fx_vol_point(0.07, 0.5, 1.0, std::nullopt, profile, stream);
        if (!o.value) {
            ++missing;
            continue;
        }
        const double error = *o.value - 0.07;
        sum += error;
        sum_squares += error * error;
        ++seen;
    }
    const double mean = sum / seen;
    const double spread = std::sqrt(sum_squares / seen - mean * mean);
    CHECK(mean == Catch::Approx(profile.bias).margin(0.0001));
    CHECK(spread == Catch::Approx(profile.noise).epsilon(0.05));
    CHECK(static_cast<double>(missing) / draws == Catch::Approx(0.1).margin(0.01));
}

TEST_CASE("a stale point repeats the contributor's last value, and a quote is rounded to its step",
          "[fx_vol_surface_generator]") {
    auto stream = stream_of(7);
    const fx_vol_noise_profile stale{.stale_probability = 1.0};
    const auto repeated = observe_fx_vol_point(0.07, 0.5, 1.0, 0.068, stale, stream);
    REQUIRE(repeated.value);
    CHECK(*repeated.value == 0.068);
    CHECK(repeated.stale);

    const fx_vol_noise_profile stepped{.noise = 0.003, .quote_step = 0.0005};
    for (int i = 0; i < 100; ++i) {
        const auto o = observe_fx_vol_point(0.07, 0.25, 1.0, std::nullopt, stepped, stream);
        REQUIRE(o.value);
        const double steps = *o.value / 0.0005;
        CHECK(steps == Catch::Approx(std::round(steps)).margin(1e-9));
    }
}

TEST_CASE("one seed gives one contributor path", "[fx_vol_surface_generator]") {
    const fx_vol_noise_profile profile{.noise = 0.002, .missing_probability = 0.2};
    auto a = stream_of(8);
    auto b = stream_of(8);
    for (int i = 0; i < 50; ++i)
        CHECK(observe_fx_vol_point(0.07, 0.1, 2.0, 0.07, profile, a).value ==
              observe_fx_vol_point(0.07, 0.1, 2.0, 0.07, profile, b).value);
}
