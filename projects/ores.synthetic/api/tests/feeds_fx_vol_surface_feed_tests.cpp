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
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <string>
#include <vector>

using namespace ores::analytics::quant::math;
using namespace ores::analytics::quant::service;
using namespace ores::synthetic::feed;

namespace {

const fx_market feed_eur_usd{
    .spot = 1.0842, .domestic_discount = 0.9752, .foreign_discount = 0.9851};
const fx_quote_convention feed_convention{.atm = fx_atm_type::delta_neutral,
                                          .delta = fx_delta_type::spot,
                                          .favour = fx_risk_reversal_favour::call,
                                          .butterfly = fx_butterfly_style::smile};

ores::synthetic::domain::fx_vol_surface_feed feed_row() {
    ores::synthetic::domain::fx_vol_surface_feed row;
    row.source_name = "synthetic.fx_vol.eurusd";
    row.base_currency_code = "EUR";
    row.quote_currency_code = "USD";
    row.ticks_per_hour = 12;
    row.arrival_mode = "periodic";
    row.sensitivity = 1.0;
    row.skew_reversion = 3.0;
    row.skew_volatility = 0.6;
    row.curvature_reversion = 3.0;
    row.curvature_volatility = 0.4;
    row.spot_variance_correlation = 0.3;
    row.spot_skew_correlation = -0.4;
    row.segment_correlation = 0.9;
    row.start_skew = -0.1;
    row.start_curvature = 0.8;
    return row;
}

ores::synthetic::domain::fx_vol_surface_feed_expiry feed_expiry(std::string tenor, double atm_vol) {
    ores::synthetic::domain::fx_vol_surface_feed_expiry row;
    row.tenor = std::move(tenor);
    row.atm_vol = atm_vol;
    row.variance_reversion = 2.0;
    row.variance_volatility = 0.08;
    return row;
}

fx_vol_surface_feed_build_input feed_input() {
    fx_vol_surface_feed_build_input in;
    in.config = feed_row();
    // Given out of order, so the maker's own ordering is what the test sees.
    in.expiries = {feed_expiry("6M", 0.075), feed_expiry("1M", 0.070), feed_expiry("3M", 0.0725)};
    in.contributors = {"reference"};
    in.market = feed_eur_usd;
    in.convention = feed_convention;
    in.deltas = {0.25, 0.10};
    return in;
}

}

TEST_CASE("a tenor names a year fraction, or nothing", "[fx_vol_surface_feed]") {
    CHECK(*fx_vol_tenor_years("1W") == Catch::Approx(7.0 / 365.0));
    CHECK(*fx_vol_tenor_years("1M") == Catch::Approx(1.0 / 12.0));
    CHECK(*fx_vol_tenor_years("3M") == Catch::Approx(0.25));
    CHECK(*fx_vol_tenor_years("1Y") == Catch::Approx(1.0));
    CHECK(*fx_vol_tenor_years("10D") == Catch::Approx(10.0 / 365.0));
    CHECK_FALSE(fx_vol_tenor_years("1X").has_value());
    CHECK_FALSE(fx_vol_tenor_years("M").has_value());
    CHECK_FALSE(fx_vol_tenor_years("").has_value());
    CHECK_FALSE(fx_vol_tenor_years("0M").has_value());
    CHECK_FALSE(fx_vol_tenor_years("1.5Y").has_value());
}

TEST_CASE("a feed row makes an emitter that starts where the row says", "[fx_vol_surface_feed]") {
    const auto in = feed_input();
    auto emitter = make_fx_vol_surface_emitter(in, 99);
    REQUIRE(emitter);

    // The expiries come back in year fraction order, whatever order the rows were in.
    const auto& surface = emitter->surface();
    REQUIRE(surface.expiries.size() == 3);
    CHECK(surface.expiries[0].expiry_id == "1M");
    CHECK(surface.expiries[1].expiry_id == "3M");
    CHECK(surface.expiries[2].expiry_id == "6M");
    CHECK(surface.rho == Catch::Approx(-0.1));
    CHECK(surface.eta == Catch::Approx(0.8));

    // One normal for each segment, the skew and the curvature.
    CHECK(emitter->points_per_contributor() == 15);
    std::vector<double> drivers(5, 0.0);
    const auto points = emitter->step(drivers);
    REQUIRE(points.size() == 15);
    // No driver, so the surface steps nowhere: the 1M point is the row's own 7%, less the
    // delta-neutral adjustment a skew makes.
    REQUIRE(points[0].value);
    CHECK(*points[0].value == Catch::Approx(0.070).margin(1e-3));
}

TEST_CASE("a feed row whose expiries cannot make a surface is refused", "[fx_vol_surface_feed]") {
    // A tenor that does not read.
    auto bad_tenor = feed_input();
    bad_tenor.expiries[0].tenor = "6X";
    CHECK_FALSE(make_fx_vol_surface_emitter(bad_tenor, 1).has_value());

    // A total variance that falls: the later expiry quotes a lower volatility and does
    // not earn back enough from its extra time.
    auto falling = feed_input();
    falling.expiries = {feed_expiry("1M", 0.070), feed_expiry("3M", 0.030)};
    const auto refused = make_fx_vol_surface_emitter(falling, 1);
    REQUIRE_FALSE(refused);
    CHECK(refused.error().find("does not rise") != std::string::npos);

    // A surface with nobody to quote it.
    auto no_one = feed_input();
    no_one.contributors.clear();
    CHECK_FALSE(make_fx_vol_surface_emitter(no_one, 1).has_value());

    // The same contributor twice, and a profile list of another length.
    auto twice = feed_input();
    twice.contributors = {"reference", "reference"};
    CHECK_FALSE(make_fx_vol_surface_emitter(twice, 1).has_value());

    auto mismatched = feed_input();
    mismatched.contributor_profiles = {fx_vol_noise_profile{}, fx_vol_noise_profile{}};
    CHECK_FALSE(make_fx_vol_surface_emitter(mismatched, 1).has_value());

    // No delta beyond ATM is a surface nobody can read a risk reversal from.
    auto no_deltas = feed_input();
    no_deltas.deltas.clear();
    CHECK_FALSE(make_fx_vol_surface_emitter(no_deltas, 1).has_value());
}

TEST_CASE("each contributor of a surface is its own stream", "[fx_vol_surface_feed]") {
    auto in = feed_input();
    in.contributors = {"reference", "broker"};
    in.contributor_profiles = {fx_vol_noise_profile{}, fx_vol_noise_profile{.noise = 0.01}};
    auto emitter = make_fx_vol_surface_emitter(in, 7);
    REQUIRE(emitter);
    CHECK(emitter->contributor_count() == 2);
    std::vector<double> drivers(5, 0.0);
    const auto points = emitter->step(drivers);
    REQUIRE(points.size() == 30);

    // The clean contributor is the surface, and the noisy one moves somewhere else.
    bool moved = false;
    for (std::size_t i = 15; i < 30; ++i) {
        REQUIRE(points[i].value);
        if (*points[i].value != *points[i - 15].value)
            moved = true;
    }
    CHECK(moved);
}
