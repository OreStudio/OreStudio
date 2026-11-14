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
#include "ores.analytics.quant/domain/market_board.hpp"
#include "ores.analytics.quant/service/run_clock.hpp"
#include "ores.analytics.quant/service/run_market.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include "ores.testing/nats_options_helper.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
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

/// The markets a surface's step reads, one for each expiry it quotes.
std::vector<ores::analytics::quant::math::fx_market>
feed_markets(std::size_t expiries, ores::analytics::quant::math::fx_market market = feed_eur_usd) {
    return std::vector<ores::analytics::quant::math::fx_market>(expiries, market);
}

fx_vol_surface_feed_build_input feed_input() {
    fx_vol_surface_feed_build_input in;
    in.config = feed_row();
    // Given out of order, so the maker's own ordering is what the test sees.
    in.expiries = {feed_expiry("6M", 0.075), feed_expiry("1M", 0.070), feed_expiry("3M", 0.0725)};
    in.contributors = {"reference"};
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
    const auto points = emitter->step(drivers, feed_markets(3));
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

TEST_CASE("a surface is quoted against the market a step hands it", "[fx_vol_surface_feed]") {
    auto emitter = make_fx_vol_surface_emitter(feed_input(), 1);
    REQUIRE(emitter);
    std::vector<double> drivers(3 + 2, 0.0);

    // One market for each expiry, and the count is checked rather than assumed.
    const auto one_short = feed_markets(2);
    CHECK_THROWS_AS(emitter->step(drivers, one_short), std::invalid_argument);

    const auto unit = feed_markets(3);
    const fx_market tilted{.spot = 1.0, .domestic_discount = 0.97, .foreign_discount = 0.99};
    const auto moved_market = feed_markets(3, tilted);

    // The step answers a view of the emitter's own points, so the first set is copied before
    // the second step writes over it.
    std::vector<double> flat;
    for (const auto& point : emitter->step(drivers, unit))
        flat.push_back(point.value.value_or(0.0));
    const auto moved = emitter->step(drivers, moved_market);
    REQUIRE(moved.size() == flat.size());
    bool any_differ = false;
    for (std::size_t i = 0; i < moved.size(); ++i) {
        REQUIRE(moved[i].value);
        any_differ = any_differ || std::abs(*moved[i].value - flat[i]) > 1e-6;
    }
    CHECK(any_differ);
}

TEST_CASE("each contributor of a surface is its own stream", "[fx_vol_surface_feed]") {
    auto in = feed_input();
    in.contributors = {"reference", "broker"};
    in.contributor_profiles = {fx_vol_noise_profile{}, fx_vol_noise_profile{.noise = 0.01}};
    auto emitter = make_fx_vol_surface_emitter(in, 7);
    REQUIRE(emitter);
    CHECK(emitter->contributor_count() == 2);
    std::vector<double> drivers(5, 0.0);
    const auto points = emitter->step(drivers, feed_markets(3));
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

TEST_CASE("a tick whose run has not published what the surface reads says which and sends nothing",
          "[fx_vol_surface_feed]") {
    // The gates are the sentences an operator greps for, so they are asserted through the tick
    // that logs them rather than through the function underneath it. Every one of them answers
    // before the publish, so this test needs no broker.
    ores::analytics::quant::service::manual_run_clock clock(std::chrono::system_clock::now());
    ores::analytics::quant::service::run_market market(clock, {}, 7);
    ores::nats::service::client nats(ores::testing::make_nats_options());
    fx_vol_surface_feed feed(
        nats,
        "synthetic.fx_vol.eurusd",
        "synthetic.fx_vol.eurusd.market",
        feed_input(),
        7,
        market,
        1.0,
        ores::analytics::quant::domain::arrival_spec{.mode = "periodic", .ticks_per_hour = 1.0});
    // Three forward variances and the skew and the curvature: the count the run drives it with.
    const std::vector<double> drivers(5, 0.0);
    const auto at = std::chrono::system_clock::time_point{};
    const auto tick = [&] {
        return feed.tick({.elapsed_seconds = 1.0, .observed_at = at, .drivers = drivers});
    };

    CHECK(tick() == "no market: curve:EUR");
    CHECK(feed.publish_count() == 0);

    // The gate refuses before it reads a curve, so two stated points are all these need.
    const auto flat = [](double factor) {
        return std::vector<ores::analytics::quant::domain::curve_point>{
            {.date = std::chrono::year_month_day{}, .discount_factor = 1.0},
            {.date = std::chrono::year_month_day{}, .discount_factor = factor}};
    };
    const auto eur =
        market.values().member_of(ores::analytics::quant::domain::market_member_kind::curve, "EUR");
    market.values().publish_curve(eur, std::chrono::year_month_day{}, flat(0.97), at);
    CHECK(tick() == "no market: curve:USD");
    CHECK(feed.publish_count() == 0);

    const auto usd_id =
        market.values().member_of(ores::analytics::quant::domain::market_member_kind::curve, "USD");
    market.values().publish_curve(usd_id, std::chrono::year_month_day{}, flat(0.99), at);
    CHECK(tick() == "no market: spot:EUR/USD");
    CHECK(feed.publish_count() == 0);

    // With all three published the gate stops refusing, so the next thing a tick would do is
    // send; the board read is what a test can follow without a broker, and the round trip does.
    const auto spot = market.values().member_of(
        ores::analytics::quant::domain::market_member_kind::spot, "EUR/USD");
    market.values().publish_spot(spot, 1.0842, at);
    const auto inputs = fx_vol_board_inputs(market.values(), eur, usd_id, spot);
    REQUIRE(inputs);
    CHECK(inputs->spot.spot == 1.0842);
}
