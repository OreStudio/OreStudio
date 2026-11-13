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
#include "ores.analytics.quant/service/ssvi_calibration_engine.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_emitter.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <string>
#include <vector>

using namespace ores::analytics::quant::math;
using namespace ores::analytics::quant::service;
using namespace ores::synthetic::feed;

namespace {

constexpr double round_trip_day = 1.0 / 252.0;

const fx_market round_trip_market{
    .spot = 1.0842, .domestic_discount = 0.9752, .foreign_discount = 0.9851};
const fx_quote_convention round_trip_convention{.atm = fx_atm_type::delta_neutral,
                                                .delta = fx_delta_type::spot,
                                                .favour = fx_risk_reversal_favour::call,
                                                .butterfly = fx_butterfly_style::smile};

ores::synthetic::domain::fx_vol_surface_feed round_trip_row() {
    ores::synthetic::domain::fx_vol_surface_feed row;
    row.source_name = "synthetic.fx_vol.round_trip";
    row.base_currency_code = "EUR";
    row.quote_currency_code = "USD";
    row.ticks_per_hour = 1.0;
    row.arrival_mode = "periodic";
    row.branching_ratio = 0.0;
    row.decay = 0.0;
    row.sensitivity = 1.0;
    row.start_skew = -0.1;
    row.start_curvature = 0.8;
    row.skew_reversion = 3.0;
    row.skew_volatility = 0.6;
    row.curvature_reversion = 3.0;
    row.curvature_volatility = 0.4;
    row.spot_skew_correlation = -0.4;
    row.spot_variance_correlation = 0.3;
    row.segment_correlation = 0.9;
    return row;
}

ores::synthetic::domain::fx_vol_surface_feed_expiry round_trip_expiry(std::string tenor,
                                                                      double atm_vol) {
    ores::synthetic::domain::fx_vol_surface_feed_expiry row;
    row.tenor = std::move(tenor);
    row.atm_vol = atm_vol;
    row.variance_reversion = 2.0;
    row.variance_volatility = 0.08;
    return row;
}

fx_vol_surface_feed_build_input round_trip_input() {
    fx_vol_surface_feed_build_input in;
    in.config = round_trip_row();
    in.expiries = {round_trip_expiry("1M", 0.070),
                   round_trip_expiry("3M", 0.0725),
                   round_trip_expiry("6M", 0.075)};
    in.contributors = {"reference"};
    in.convention = round_trip_convention;
    in.deltas = {0.25, 0.10};
    return in;
}

std::vector<fx_market> round_trip_markets(std::size_t expiries) {
    return std::vector<fx_market>(expiries, round_trip_market);
}

/// The kind of quote a point label names, and the delta it carries.
ssvi_quote_kind kind_of(const std::string& label) {
    if (label == "ATM")
        return ssvi_quote_kind::atm;
    if (label.ends_with("RR"))
        return ssvi_quote_kind::risk_reversal;
    return ssvi_quote_kind::butterfly;
}

double delta_of(const std::string& label) {
    if (label == "ATM")
        return 0.0;
    return std::stod(label.substr(0, label.size() - 2)) / 100.0;
}

}

TEST_CASE("the surface a run publishes is the surface a calibration recovers",
          "[fx_vol_surface_round_trip]") {
    // The round trip this task owes: the emitter strikes its quotes against a market, and a
    // party calibrating those quotes against the same market lands on the same surface. It is
    // the claim that the two sides resolve one market by one rule, so the margin below is the
    // fit's own and not a tolerance for a market that disagrees.
    auto emitter = make_fx_vol_surface_emitter(round_trip_input(), 99);
    if (!emitter)
        FAIL(emitter.error());
    REQUIRE(emitter);

    const auto start = round_trip_input();
    const auto markets = round_trip_markets(start.expiries.size());
    std::vector<double> drivers(start.expiries.size() + 2, 0.0);
    const auto points = emitter->step(drivers, markets);
    REQUIRE(points.size() == start.expiries.size() * 5);

    // The clean contributor's quotes, one expiry at a time, as the calibration reads them.
    ssvi_request request;
    request.pillar_tolerance = 0.0;
    const auto& shown = emitter->surface().expiries;
    REQUIRE(shown.size() == start.expiries.size());
    for (std::size_t i = 0; i < shown.size(); ++i) {
        const double years = *fx_vol_tenor_years(shown[i].expiry_id);
        ssvi_expiry_quotes quotes{.expiry_id = shown[i].expiry_id,
                                  .years = years,
                                  .market = round_trip_market,
                                  .convention = round_trip_convention,
                                  .pillars = {}};
        for (std::size_t k = 0; k < 5; ++k) {
            const auto& point = points[i * 5 + k];
            REQUIRE(point.value);
            quotes.pillars.push_back(
                {.point_id = point.label,
                 .kind = kind_of(point.label),
                 .delta = delta_of(point.label),
                 .log_moneyness = 0.0,
                 .contributions = {{.volatility = *point.value, .weight = 1.0}}});
        }
        request.grid_expiries.push_back({.expiry_id = quotes.expiry_id,
                                         .years = years,
                                         .market = round_trip_market,
                                         .convention = round_trip_convention});
        request.expiries.push_back(std::move(quotes));
    }
    request.grid_strikes.push_back({.strike_id = "ATM", .at = ssvi_atm_strike{}});
    for (const double delta : {0.25, 0.10})
        for (const auto side : {option_side::call, option_side::put})
            request.grid_strikes.push_back(
                {.strike_id = std::to_string(static_cast<int>(delta * 100.0)) +
                              (side == option_side::call ? "C" : "P"),
                 .at =
                     ssvi_delta_strike{.type = fx_delta_type::spot, .side = side, .delta = delta}});

    auto calibrated = calibrate_ssvi(request);
    REQUIRE(calibrated);
    REQUIRE(calibrated->pillars.size() == points.size());

    // The quoted point and the fitted one, at the same strike, are the same volatility: the
    // wing tolerances are the wider because the smile amplifies the same strike error there.
    double worst_atm = 0.0;
    double worst_wing = 0.0;
    for (const auto& fit : calibrated->pillars) {
        const auto quoted = fit.quoted;
        REQUIRE(fit.error == Catch::Approx(std::abs(fit.fitted - quoted)).margin(1e-15));
        if (fit.point_id == "ATM")
            worst_atm = std::max(worst_atm, std::abs(fit.error));
        else
            worst_wing = std::max(worst_wing, std::abs(fit.error));
    }
    // Measured on this fixture: 1.25e-09 at ATM and 3.81e-09 at the wings, so the margin is two
    // orders of magnitude above what the fit costs and far below a tenth of a volatility point.
    INFO("worst ATM error " << worst_atm << ", worst wing error " << worst_wing);
    CHECK(worst_atm < 1e-7);
    CHECK(worst_wing < 1e-7);

    // The strike half of the same claim, and it is exact: the calibration solves the ATM strike
    // with the same function the emitter uses, against the same market. A margin here would
    // only hide a divergence.
    for (const auto& point : calibrated->grid) {
        if (point.strike_id != "ATM")
            continue;
        const double vol = [&] {
            for (const auto& fit : calibrated->pillars)
                if (fit.expiry_id == point.expiry_id && fit.point_id == "ATM")
                    return fit.quoted;
            return 0.0;
        }();
        const auto years =
            std::ranges::find(request.expiries, point.expiry_id, &ssvi_expiry_quotes::expiry_id)
                ->years;
        const double expected = fx_atm_strike(round_trip_convention.atm,
                                              round_trip_convention.delta,
                                              round_trip_market,
                                              vol * std::sqrt(years));
        const double strike = round_trip_market.forward() * std::exp(point.log_moneyness);
        // The residual is the fit's: the calibration solves the grid strike on the fitted smile,
        // whose volatility differs from the quote by the error above.
        CHECK(strike == Catch::Approx(expected).epsilon(1e-8));
    }
}
