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
#include "ores.analytics.quant/domain/market_board.hpp"
#include "ores.analytics.quant/math/fx_delta.hpp"
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include "ores.analytics.quant/service/ssvi_calibration_engine.hpp"
#include "ores.refdata.core/mapping/fx_option_convention_mapping.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_emitter.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <string>
#include <vector>

using namespace ores::analytics::quant::domain;
using namespace ores::analytics::quant::math;
using namespace ores::analytics::quant::service;
using namespace ores::synthetic::feed;

/*
 * The round trip, through the run's own values: curves and a spot are published to a board as
 * three feeds publish them, the surface reads its markets off that board, and a party calibrating
 * the quotes the surface emitted against those same markets lands on the same surface. No market
 * is built by hand here, because a test that hands both sides its own market proves only that the
 * fit recovers the quotes -- which is a claim about the fit, not about the run.
 */

namespace {

constexpr double round_trip_day = 1.0 / 365.0;
constexpr double round_trip_spot = 1.0842;
/// The pair's base currency, which is the foreign side of the forward.
constexpr double round_trip_eur_rate = 0.03;
/// The pair's quote currency, which is the domestic side.
constexpr double round_trip_usd_rate = 0.01;

const std::chrono::year_month_day round_trip_spot_date{
    std::chrono::year{2026}, std::chrono::November, std::chrono::day{16}};

const fx_quote_convention round_trip_convention{.atm = fx_atm_type::delta_neutral,
                                                .delta = fx_delta_type::spot,
                                                .favour = fx_risk_reversal_favour::call,
                                                .butterfly = fx_butterfly_style::smile};

int round_trip_days_to(std::chrono::year_month_day date) {
    return static_cast<int>(
        (std::chrono::sys_days(date) - std::chrono::sys_days(round_trip_spot_date)).count());
}

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

/// The periods of the expiries the emitter quotes, in the emitter's own order.
std::vector<date_period> round_trip_periods(const std::vector<ssvi_expiry_fit>& expiries) {
    std::vector<date_period> out;
    out.reserve(expiries.size());
    for (const auto& expiry : expiries) {
        const auto period = ores::refdata::mapping::parse_tenor_text(expiry.expiry_id);
        REQUIRE(period);
        out.push_back(*period);
    }
    return out;
}

/// A flat curve as a curve feed writes one: the spot date at one, then a point per value date.
std::vector<curve_point> round_trip_curve(double rate, const std::vector<date_period>& periods) {
    std::vector<curve_point> out{{.date = round_trip_spot_date, .discount_factor = 1.0}};
    for (const auto& period : periods) {
        const auto date = calendar_arithmetic::add_period(round_trip_spot_date, period);
        out.push_back(
            {.date = date,
             .discount_factor = std::exp(-rate * round_trip_days_to(date) * round_trip_day)});
    }
    return out;
}

/// The run's board with the pair's three members, as the two feeds resolve them.
struct round_trip_board final {
    round_trip_board() {
        eur = values.member_of(market_member_kind::curve, "EUR");
        usd = values.member_of(market_member_kind::curve, "USD");
        spot = values.member_of(market_member_kind::spot, fx_pair_member_code("EUR", "USD"));
    }

    market_board values{4};
    market_board::member_id eur{};
    market_board::member_id usd{};
    market_board::member_id spot{};
};

/// What three feeds do in one tick: both curves and the spot.
void round_trip_publish(round_trip_board& board, const std::vector<date_period>& periods) {
    const auto at = std::chrono::system_clock::time_point{};
    const auto base = round_trip_curve(round_trip_eur_rate, periods);
    const auto quote = round_trip_curve(round_trip_usd_rate, periods);
    board.values.publish_curve(board.eur, round_trip_spot_date, base, at);
    board.values.publish_curve(board.usd, round_trip_spot_date, quote, at);
    board.values.publish_spot(board.spot, round_trip_spot, at);
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
    auto emitter = make_fx_vol_surface_emitter(round_trip_input(), 99);
    REQUIRE(emitter);

    // The three values the run's feeds publish, and the markets the surface reads off them.
    round_trip_board board;
    const auto& shown = emitter->surface().expiries;
    REQUIRE(shown.size() == 3);
    const auto periods = round_trip_periods(shown);
    round_trip_publish(board, periods);
    const auto inputs = fx_vol_board_inputs(board.values, board.eur, board.usd, board.spot);
    REQUIRE(inputs);
    std::vector<fx_market> markets;
    REQUIRE(fx_vol_expiry_markets(*inputs, periods, markets));
    REQUIRE(markets.size() == shown.size());

    // Each expiry is struck against the forward of the pair's own curves at its own value date,
    // so the three markets are three different markets and neither curve is assumed flat.
    for (std::size_t i = 0; i < markets.size(); ++i) {
        const auto value_date = calendar_arithmetic::add_period(round_trip_spot_date, periods[i]);
        const double years = round_trip_days_to(value_date) * round_trip_day;
        INFO("expiry " << shown[i].expiry_id << " discounts over " << years << " years");
        CHECK(markets[i].domestic_discount ==
              Catch::Approx(std::exp(-round_trip_usd_rate * years)).epsilon(1e-12));
        CHECK(markets[i].foreign_discount ==
              Catch::Approx(std::exp(-round_trip_eur_rate * years)).epsilon(1e-12));
    }
    for (std::size_t i = 1; i < markets.size(); ++i)
        CHECK(markets[i].forward() < markets[i - 1].forward() - 1e-6);

    std::vector<double> drivers(shown.size() + 2, 0.0);
    const auto points = emitter->step(drivers, markets);
    REQUIRE(points.size() == shown.size() * 5);

    // The clean contributor's quotes, one expiry at a time, as the calibration reads them. The
    // calibration is handed the same markets the emitter struck against, because that is what a
    // party's own bootstrap resolves from the same values.
    ssvi_request request;
    request.pillar_tolerance = 0.0;
    for (std::size_t i = 0; i < shown.size(); ++i) {
        const double years = *fx_vol_tenor_years(shown[i].expiry_id);
        ssvi_expiry_quotes quotes{.expiry_id = shown[i].expiry_id,
                                  .years = years,
                                  .market = markets[i],
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
                                         .market = markets[i],
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
        // The error is the signed residual the fit reports, and the abs is taken only below.
        REQUIRE(fit.error == Catch::Approx(fit.fitted - quoted).margin(1e-15));
        if (fit.point_id == "ATM")
            worst_atm = std::max(worst_atm, std::abs(fit.error));
        else
            worst_wing = std::max(worst_wing, std::abs(fit.error));
    }
    INFO("worst ATM error " << worst_atm << ", worst wing error " << worst_wing);
    // Measured on this fixture: 2.55e-09 at ATM and 6.56e-09 at the wings, so the margin is an
    // order of magnitude above what the fit costs and far below a tenth of a volatility point.
    CHECK(worst_atm < 1e-7);
    CHECK(worst_wing < 1e-7);

    // The strike half of the same claim, and it is exact: the calibration solves the ATM strike
    // with the same function the emitter uses, against the same expiry's market. A margin here
    // would only hide a divergence.
    for (const auto& point : calibrated->grid) {
        if (point.strike_id != "ATM")
            continue;
        const auto& expiry =
            *std::ranges::find(request.expiries, point.expiry_id, &ssvi_expiry_quotes::expiry_id);
        const double vol = [&] {
            for (const auto& fit : calibrated->pillars)
                if (fit.expiry_id == point.expiry_id && fit.point_id == "ATM")
                    return fit.quoted;
            return 0.0;
        }();
        const double expected = fx_atm_strike(round_trip_convention.atm,
                                              round_trip_convention.delta,
                                              expiry.market,
                                              vol * std::sqrt(expiry.years));
        const double strike = expiry.market.forward() * std::exp(point.log_moneyness);
        // The residual is the fit's: the calibration solves the grid strike on the fitted smile,
        // whose volatility differs from the quote by the error above.
        CHECK(strike == Catch::Approx(expected).epsilon(1e-8));
    }
}
