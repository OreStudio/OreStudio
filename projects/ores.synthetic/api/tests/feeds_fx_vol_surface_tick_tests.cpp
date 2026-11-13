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
#include "ores.analytics.quant/math/fx_pair_market.hpp"
#include "ores.analytics.quant/math/rng_stream.hpp"
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include "ores.analytics.quant/service/fx_vol_surface_generator.hpp"
#include "ores.refdata.core/mapping/fx_option_convention_mapping.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_emitter.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_feed.hpp"
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
 * A surface's tick, without a wire: the run's board is published to with the points a curve feed
 * would write, and the surface's own read is what is exercised. The gates are the two sentences a
 * reader of the run's log sees when a member is missing, and they are asserted character for
 * character because that is what an operator greps for.
 */

namespace {

constexpr double tick_day = 1.0 / 365.0;
constexpr double tick_spot = 1.0842;
/// The pair's base currency, which is the foreign side of the forward.
constexpr double tick_eur_rate = 0.03;
/// The pair's quote currency, which is the domestic side.
constexpr double tick_usd_rate = 0.01;

const std::chrono::year_month_day tick_spot_date{
    std::chrono::year{2026}, std::chrono::November, std::chrono::day{16}};

const fx_quote_convention tick_convention{.atm = fx_atm_type::delta_neutral,
                                          .delta = fx_delta_type::spot,
                                          .favour = fx_risk_reversal_favour::call,
                                          .butterfly = fx_butterfly_style::smile};

int tick_days_to(std::chrono::year_month_day date) {
    return static_cast<int>(
        (std::chrono::sys_days(date) - std::chrono::sys_days(tick_spot_date)).count());
}

/// The expiries the surface quotes, parsed by the function the feed itself parses them with.
std::vector<date_period> tick_periods() {
    std::vector<date_period> out;
    for (const std::string_view tenor : {"1M", "3M", "6M"}) {
        const auto period = ores::refdata::mapping::parse_tenor_text(tenor);
        REQUIRE(period);
        out.push_back(*period);
    }
    return out;
}

/// A flat curve as a curve feed writes one: the spot date at one, then a point per value date.
std::vector<curve_point> tick_curve(double rate, const std::vector<date_period>& periods) {
    std::vector<curve_point> out{{.date = tick_spot_date, .discount_factor = 1.0}};
    for (const auto& period : periods) {
        const auto date = calendar_arithmetic::add_period(tick_spot_date, period);
        out.push_back(
            {.date = date, .discount_factor = std::exp(-rate * tick_days_to(date) * tick_day)});
    }
    return out;
}

/// The run's board with the pair's three members, resolved as the two feeds resolve them.
struct tick_board final {
    tick_board() {
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
void tick_publish(tick_board& board,
                  const std::vector<date_period>& periods,
                  std::chrono::system_clock::time_point at) {
    const auto base = tick_curve(tick_eur_rate, periods);
    const auto quote = tick_curve(tick_usd_rate, periods);
    board.values.publish_curve(board.eur, tick_spot_date, base, at);
    board.values.publish_curve(board.usd, tick_spot_date, quote, at);
    board.values.publish_spot(board.spot, tick_spot, at);
}

/// A surface whose ATM rises with its expiry, as the emitter insists, and its laws.
ssvi_parameters tick_start() {
    ssvi_parameters p;
    p.rho = -0.1;
    p.eta = 0.8;
    p.gamma = 0.5;
    const std::vector<std::pair<const char*, double>> expiries{
        {"1M", 1.0 / 12.0}, {"3M", 0.25}, {"6M", 0.5}};
    double vol = 0.070;
    for (const auto& [id, years] : expiries) {
        p.expiries.push_back(
            {.expiry_id = id, .years = years, .atm_total_variance = vol * vol * years});
        vol += 0.0025;
    }
    return p;
}

fx_vol_surface_dynamics tick_dynamics(std::size_t segments) {
    fx_vol_surface_dynamics d;
    d.variance_reversion.assign(segments, 2.0);
    d.variance_volatility.assign(segments, 0.08);
    d.skew_reversion = 3.0;
    d.skew_volatility = 0.6;
    d.curvature_reversion = 3.0;
    d.curvature_volatility = 0.4;
    // The latent market correlates the shocks, so the surface's own matrix is the identity.
    d.correlation = Eigen::MatrixXd::Identity(static_cast<Eigen::Index>(segments + 3),
                                              static_cast<Eigen::Index>(segments + 3));
    return d;
}

fx_vol_surface_emitter tick_emitter(std::size_t expiries) {
    std::vector<fx_vol_contributor_spec> contributors{
        {.source_name = "reference",
         .profile = {},
         .stream = rng_stream(stream_key{.run_seed = 5,
                                         .series = series_key("synthetic.fx_vol.tick"),
                                         .purpose = draw_purpose::market})}};
    auto built = fx_vol_surface_emitter::make(tick_start(),
                                              tick_dynamics(expiries),
                                              tick_day,
                                              tick_convention,
                                              {0.25, 0.10},
                                              std::move(contributors));
    REQUIRE(built);
    return std::move(*built);
}

}

TEST_CASE("a surface names the member the run has not published", "[fx_vol_surface_tick]") {
    const auto periods = tick_periods();
    tick_board board;
    const auto at = std::chrono::system_clock::time_point{};

    // Nothing has ticked yet: the base currency is the first thing a reader of the run looks for.
    auto inputs = fx_vol_board_inputs(board.values, board.eur, board.usd, board.spot);
    REQUIRE_FALSE(inputs);
    CHECK(inputs.error() == "curve:EUR");

    board.values.publish_curve(board.eur, tick_spot_date, tick_curve(tick_eur_rate, periods), at);
    inputs = fx_vol_board_inputs(board.values, board.eur, board.usd, board.spot);
    REQUIRE_FALSE(inputs);
    CHECK(inputs.error() == "curve:USD");

    board.values.publish_curve(board.usd, tick_spot_date, tick_curve(tick_usd_rate, periods), at);
    inputs = fx_vol_board_inputs(board.values, board.eur, board.usd, board.spot);
    REQUIRE_FALSE(inputs);
    CHECK(inputs.error() == "spot:EUR/USD");

    // With all three published the surface reads them, under the name the spot feed writes.
    board.values.publish_spot(board.spot, tick_spot, at);
    inputs = fx_vol_board_inputs(board.values, board.eur, board.usd, board.spot);
    REQUIRE(inputs);
    CHECK(inputs->spot.spot == tick_spot);
    CHECK(inputs->spot_date == tick_spot_date);
    CHECK(inputs->base.size() == periods.size() + 1);
    CHECK(inputs->quote.size() == periods.size() + 1);
}

TEST_CASE("a surface resolves each expiry at the date its period gives", "[fx_vol_surface_tick]") {
    const auto periods = tick_periods();
    tick_board board;
    tick_publish(board, periods, std::chrono::system_clock::time_point{});

    const auto inputs = fx_vol_board_inputs(board.values, board.eur, board.usd, board.spot);
    REQUIRE(inputs);
    std::vector<fx_market> markets;
    REQUIRE(fx_vol_expiry_markets(*inputs, periods, markets));
    REQUIRE(markets.size() == periods.size());

    std::vector<double> forwards;
    for (std::size_t i = 0; i < periods.size(); ++i) {
        const auto value_date = calendar_arithmetic::add_period(tick_spot_date, periods[i]);
        const double years = tick_days_to(value_date) * tick_day;
        INFO("expiry " << i << " discounts over " << years << " years");
        // The two curves are read at the value date and at neither of the other expiries'
        // dates, so the period addition is observable and both sides are used.
        CHECK(markets[i].domestic_discount ==
              Catch::Approx(std::exp(-tick_usd_rate * years)).epsilon(1e-12));
        CHECK(markets[i].foreign_discount ==
              Catch::Approx(std::exp(-tick_eur_rate * years)).epsilon(1e-12));
        forwards.push_back(markets[i].forward());
        INFO("expiry " << i << " discounts over " << years << " years to a forward of "
                       << forwards.back());
        CHECK(forwards.back() ==
              Catch::Approx(tick_spot * std::exp(-(tick_eur_rate - tick_usd_rate) * years))
                  .epsilon(1e-12));
    }
    // Three expiries and three horizons: a market that repeated across them would pass the checks
    // above only if the two curves were flat across the whole six months, which they are not.
    for (std::size_t i = 1; i < forwards.size(); ++i)
        CHECK(forwards[i] < forwards[i - 1] - 1e-6);
}

TEST_CASE("two ticks against an unchanged board quote the same surface", "[fx_vol_surface_tick]") {
    const auto periods = tick_periods();
    tick_board board;
    tick_publish(board, periods, std::chrono::system_clock::time_point{});
    auto emitter = tick_emitter(periods.size());
    std::vector<double> drivers(periods.size() + 2, 0.0);

    const auto inputs = fx_vol_board_inputs(board.values, board.eur, board.usd, board.spot);
    REQUIRE(inputs);
    std::vector<fx_market> first_markets;
    REQUIRE(fx_vol_expiry_markets(*inputs, periods, first_markets));
    std::vector<double> first;
    for (const auto& point : emitter.step(drivers, first_markets)) {
        REQUIRE(point.value);
        first.push_back(*point.value);
    }

    // A tick later the same three values are published again, as a curve's feed does when the
    // market has not moved. Nothing between the board and the quote may have drifted.
    tick_publish(board, periods, std::chrono::system_clock::time_point{std::chrono::seconds(1)});
    const auto again = fx_vol_board_inputs(board.values, board.eur, board.usd, board.spot);
    REQUIRE(again);
    std::vector<fx_market> second_markets;
    REQUIRE(fx_vol_expiry_markets(*again, periods, second_markets));
    const auto second = emitter.step(drivers, second_markets);

    REQUIRE(second.size() == first.size());
    for (std::size_t i = 0; i < first.size(); ++i) {
        REQUIRE(second[i].value);
        // Bit for bit, not within a margin: the surface is at its fixed point and the board
        // handed it the same values, so anything that moved would be the board's own drift.
        CHECK(*second[i].value == first[i]);
    }
    for (std::size_t i = 0; i < periods.size(); ++i) {
        CHECK(second_markets[i].spot == first_markets[i].spot);
        CHECK(second_markets[i].domestic_discount == first_markets[i].domestic_discount);
        CHECK(second_markets[i].foreign_discount == first_markets[i].foreign_discount);
    }
}
