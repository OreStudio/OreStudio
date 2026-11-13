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
#include "ores.analytics.quant/service/fx_vol_surface_generator.hpp"
#include "ores.synthetic.api/feeds/fx_vol_surface_emitter.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <cstddef>
#include <stdexcept>
#include <string>
#include <vector>

using namespace ores::analytics::quant::math;
using namespace ores::analytics::quant::service;
using namespace ores::synthetic::feed;

namespace {

constexpr double emitter_day = 1.0 / 252.0;

const fx_market emitter_eur_usd{
    .spot = 1.0842, .domestic_discount = 0.9752, .foreign_discount = 0.9851};
const fx_quote_convention emitter_convention{.atm = fx_atm_type::delta_neutral,
                                             .delta = fx_delta_type::spot,
                                             .favour = fx_risk_reversal_favour::call,
                                             .butterfly = fx_butterfly_style::smile};

/// A EUR/USD surface from 1M to 6M, ATM near 7%, which is also where it starts.
ssvi_parameters emitter_start() {
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

std::vector<fx_vol_slice_market> emitter_markets_with(const ssvi_parameters& start,
                                                      const fx_market& market) {
    std::vector<fx_vol_slice_market> out;
    for (const auto& e : start.expiries)
        out.push_back(
            {.expiry_id = e.expiry_id, .market = market, .convention = emitter_convention});
    return out;
}

std::vector<fx_vol_slice_market> emitter_markets(const ssvi_parameters& start) {
    return emitter_markets_with(start, emitter_eur_usd);
}

/// The laws of the drivers. The matrix is the identity because the market correlates.
fx_vol_surface_dynamics emitter_dynamics(std::size_t segments) {
    fx_vol_surface_dynamics d;
    d.variance_reversion.assign(segments, 2.0);
    d.variance_volatility.assign(segments, 0.08);
    d.skew_reversion = 3.0;
    d.skew_volatility = 0.6;
    d.curvature_reversion = 3.0;
    d.curvature_volatility = 0.4;
    d.correlation = Eigen::MatrixXd::Identity(static_cast<Eigen::Index>(segments + 3),
                                              static_cast<Eigen::Index>(segments + 3));
    return d;
}

ores::analytics::quant::math::rng_stream emitter_stream(std::uint64_t series) {
    using namespace ores::analytics::quant::math;
    return rng_stream(stream_key{.run_seed = 3, .series = series, .purpose = draw_purpose::market});
}

fx_vol_contributor_spec emitter_clean() {
    return {.source_name = "reference", .profile = {}, .stream = emitter_stream(1)};
}

fx_vol_contributor_spec emitter_noisy(double noise) {
    fx_vol_noise_profile profile;
    profile.noise = noise;
    return {.source_name = "broker", .profile = profile, .stream = emitter_stream(2)};
}

const std::vector<double> emitter_deltas{0.25, 0.10};

std::expected<fx_vol_surface_emitter, std::string>
make_emitter(std::vector<fx_vol_contributor_spec> contributors,
             std::vector<fx_vol_slice_market> markets,
             fx_vol_surface_dynamics dynamics,
             std::vector<double> deltas = emitter_deltas) {
    const auto start = emitter_start();
    return fx_vol_surface_emitter::make(start,
                                        std::move(dynamics),
                                        emitter_day,
                                        std::move(markets),
                                        std::move(deltas),
                                        std::move(contributors));
}

}

TEST_CASE("a step publishes every point of every contributor in order",
          "[fx_vol_surface_emitter]") {
    const auto start = emitter_start();
    auto emitter = make_emitter({emitter_clean(), emitter_noisy(0.01)},
                                emitter_markets(start),
                                emitter_dynamics(start.expiries.size()));
    REQUIRE(emitter);

    // Three expiries, each ATM plus a risk reversal and a butterfly at two deltas.
    CHECK(emitter->points_per_contributor() == 15);
    CHECK(emitter->contributor_count() == 2);
    REQUIRE(emitter->points().size() == 30);
    CHECK(emitter->contributor_name(0) == "reference");
    CHECK(emitter->contributor_name(1) == "broker");

    std::vector<double> drivers(emitter->surface().expiries.size() + 2, 0.0);
    const auto points = emitter->step(drivers);
    REQUIRE(points.size() == 30);

    // A contributor's points are contiguous, and within one they run expiry by expiry.
    const std::vector<std::string> labels{"ATM", "25RR", "25BF", "10RR", "10BF"};
    for (std::size_t c = 0; c < 2; ++c) {
        for (std::size_t expiry = 0; expiry < 3; ++expiry) {
            for (std::size_t k = 0; k < labels.size(); ++k) {
                const auto& point = points[c * 15 + expiry * 5 + k];
                CHECK(point.label == labels[k]);
                CHECK(point.expiry_id == start.expiries[expiry].expiry_id);
            }
        }
    }
}

TEST_CASE("a clean contributor publishes the surface's own quotes", "[fx_vol_surface_emitter]") {
    const auto start = emitter_start();
    auto markets = emitter_markets(start);
    auto emitter =
        make_emitter({emitter_clean()}, markets, emitter_dynamics(start.expiries.size()));
    REQUIRE(emitter);

    // No driver, so the surface steps nowhere: every process reverts to where it started.
    std::vector<double> drivers(start.expiries.size() + 2, 0.0);
    const auto points = emitter->step(drivers);
    const auto quotes = fx_vol_surface_quotes(emitter->surface(), markets, emitter_deltas);
    REQUIRE(quotes);

    REQUIRE(points.size() == 15);
    // The 1M point is near the surface's own 7% and carries no noise at all. It sits
    // just under it because the ATM is delta neutral, and the surface has a skew.
    REQUIRE(points[0].value);
    CHECK(*points[0].value == Catch::Approx(0.070).margin(1e-3));

    for (std::size_t expiry = 0; expiry < 3; ++expiry) {
        const auto& slice = (*quotes)[expiry];
        CHECK(points[expiry * 5].value == Catch::Approx(slice.atm).margin(1e-15));
        for (std::size_t k = 0; k < emitter_deltas.size(); ++k) {
            CHECK(points[expiry * 5 + 1 + 2 * k].value ==
                  Catch::Approx(slice.risk_reversals[k]).margin(1e-15));
            CHECK(points[expiry * 5 + 2 + 2 * k].value ==
                  Catch::Approx(slice.butterflies[k]).margin(1e-15));
        }
    }
}

TEST_CASE("a noisy contributor moves its own quotes and not the clean ones",
          "[fx_vol_surface_emitter]") {
    const auto start = emitter_start();
    auto emitter = make_emitter({emitter_clean(), emitter_noisy(0.01)},
                                emitter_markets(start),
                                emitter_dynamics(start.expiries.size()));
    REQUIRE(emitter);
    std::vector<double> drivers(start.expiries.size() + 2, 0.0);
    const auto points = emitter->step(drivers);

    // The clean contributor is the truth, so its 1M ATM is still the surface's own.
    REQUIRE(points[0].value);
    CHECK(*points[0].value == Catch::Approx(0.070).margin(1e-3));
    // The noisy one moves somewhere, and by a plausible amount.
    bool moved = false;
    double worst = 0.0;
    for (std::size_t i = 15; i < 30; ++i) {
        REQUIRE(points[i].value);
        const double difference = std::abs(*points[i].value - *points[i - 15].value);
        worst = std::max(worst, difference);
        if (difference > 1e-6)
            moved = true;
    }
    CHECK(moved);
    // A noise of one volatility point is a few tenths of a point anywhere but a wing.
    CHECK(worst < 0.05);
}

TEST_CASE("a contributor can leave a point out and repeat a point", "[fx_vol_surface_emitter]") {
    const auto start = emitter_start();
    auto missing = make_emitter({{.source_name = "silent",
                                  .profile = {.missing_probability = 1.0},
                                  .stream = emitter_stream(5)}},
                                emitter_markets(start),
                                emitter_dynamics(start.expiries.size()));
    REQUIRE(missing);
    std::vector<double> drivers(start.expiries.size() + 2, 0.0);
    for (const auto& point : missing->step(drivers))
        CHECK_FALSE(point.value);

    auto stale = make_emitter({{.source_name = "sluggish",
                                .profile = {.stale_probability = 1.0},
                                .stream = emitter_stream(6)}},
                              emitter_markets(start),
                              emitter_dynamics(start.expiries.size()));
    REQUIRE(stale);
    const auto first = stale->step(drivers);
    std::vector<std::optional<double>> seen;
    for (const auto& point : first)
        seen.push_back(point.value);
    // Every point was published once, so the second step can repeat it.
    for (const auto& value : seen)
        REQUIRE(value);
    const auto second = stale->step(drivers);
    for (std::size_t i = 0; i < second.size(); ++i) {
        CHECK(second[i].stale);
        CHECK(second[i].value == seen[i]);
    }
}

TEST_CASE("the emitter refuses a step that does not hold one normal for each driver",
          "[fx_vol_surface_emitter]") {
    const auto start = emitter_start();
    auto emitter = make_emitter(
        {emitter_clean()}, emitter_markets(start), emitter_dynamics(start.expiries.size()));
    REQUIRE(emitter);
    const std::vector<double> too_few(start.expiries.size(), 0.0);
    CHECK_THROWS_AS(emitter->step(too_few), std::invalid_argument);
}

TEST_CASE("the emitter refuses what it cannot step", "[fx_vol_surface_emitter]") {
    const auto start = emitter_start();
    // The market correlates the shocks, so the surface's own matrix is the identity.
    auto correlated = emitter_dynamics(start.expiries.size());
    correlated.correlation(0, 1) = correlated.correlation(1, 0) = 0.3;
    CHECK_FALSE(make_emitter({emitter_clean()}, emitter_markets(start), correlated).has_value());
    // A delta outside (0, 0.5] has no risk reversal to quote.
    CHECK_FALSE(make_emitter({emitter_clean()},
                             emitter_markets(start),
                             emitter_dynamics(start.expiries.size()),
                             {0.6})
                    .has_value());
    // A surface with nobody to quote it is not a feed.
    CHECK_FALSE(make_emitter({}, emitter_markets(start), emitter_dynamics(start.expiries.size()))
                    .has_value());
    // A market per expiry, and one for each.
    auto few_markets = emitter_markets(start);
    few_markets.pop_back();
    CHECK_FALSE(
        make_emitter({emitter_clean()}, few_markets, emitter_dynamics(start.expiries.size()))
            .has_value());
}

TEST_CASE("a surface's quotes do not depend on the level of its spot", "[fx_vol_surface_emitter]") {
    // A smile is read at a moneyness -- the strike over the forward -- so while the two
    // discounts are equal a common scale of the spot cancels. This is what lets a feed quote
    // against a market it has not resolved the pair for, and it is a claim, not a hope.
    const auto start = emitter_start();
    const fx_market unit{.spot = 1.0, .domestic_discount = 1.0, .foreign_discount = 1.0};
    const fx_market scaled{.spot = 1.30, .domestic_discount = 1.0, .foreign_discount = 1.0};

    auto one = make_emitter({emitter_clean()},
                            emitter_markets_with(start, unit),
                            emitter_dynamics(start.expiries.size()));
    auto other = make_emitter({emitter_clean()},
                              emitter_markets_with(start, scaled),
                              emitter_dynamics(start.expiries.size()));
    REQUIRE(one);
    REQUIRE(other);

    std::vector<double> drivers(start.expiries.size() + 2, 0.0);
    const auto a = one->step(drivers);
    const auto b = other->step(drivers);
    REQUIRE(a.size() == b.size());
    for (std::size_t i = 0; i < a.size(); ++i) {
        REQUIRE(a[i].value);
        REQUIRE(b[i].value);
        CHECK(*b[i].value == Catch::Approx(*a[i].value).margin(1e-12));
    }
}

TEST_CASE("a surface's quotes do not depend on the level of its spot once its discounts differ",
          "[fx_vol_surface_emitter]") {
    // The scale argument does not need the two discounts to be equal: the foreign discount is
    // outside the delta solve and the forward scales with the spot, so every strike scales with
    // it and the volatilities read at them do not move. This is what lets a feed quote against
    // the pair's two discount factors without first knowing its spot.
    const auto start = emitter_start();
    const fx_market at_one{.spot = 1.0, .domestic_discount = 0.9752, .foreign_discount = 0.9851};
    const fx_market at_spot{
        .spot = 1.0842, .domestic_discount = 0.9752, .foreign_discount = 0.9851};

    auto one = make_emitter({emitter_clean()},
                            emitter_markets_with(start, at_one),
                            emitter_dynamics(start.expiries.size()));
    auto other = make_emitter({emitter_clean()},
                              emitter_markets_with(start, at_spot),
                              emitter_dynamics(start.expiries.size()));
    REQUIRE(one);
    REQUIRE(other);

    std::vector<double> drivers(start.expiries.size() + 2, 0.0);
    const auto a = one->step(drivers);
    const auto b = other->step(drivers);
    REQUIRE(a.size() == b.size());
    for (std::size_t i = 0; i < a.size(); ++i) {
        REQUIRE(a[i].value);
        REQUIRE(b[i].value);
        CHECK(*b[i].value == Catch::Approx(*a[i].value).margin(1e-12));
    }
}

TEST_CASE("a surface's quotes do depend on the ratio of its discounts",
          "[fx_vol_surface_emitter]") {
    // The forward is the spot times one discount over the other. While they differ the ATM a
    // delta neutral reader picks is not the forward, so the quoted volatilities move: the unit
    // spot is exact only while the pair has no rates, and this pins where that stops.
    const auto start = emitter_start();
    const fx_market unit{.spot = 1.0, .domestic_discount = 1.0, .foreign_discount = 1.0};
    const fx_market tilted{.spot = 1.0, .domestic_discount = 0.97, .foreign_discount = 0.99};

    auto a = make_emitter({emitter_clean()},
                          emitter_markets_with(start, unit),
                          emitter_dynamics(start.expiries.size()));
    auto b = make_emitter({emitter_clean()},
                          emitter_markets_with(start, tilted),
                          emitter_dynamics(start.expiries.size()));
    REQUIRE(a);
    REQUIRE(b);

    std::vector<double> drivers(start.expiries.size() + 2, 0.0);
    const auto pa = a->step(drivers);
    const auto pb = b->step(drivers);
    double worst = 0.0;
    for (std::size_t i = 0; i < pa.size(); ++i) {
        REQUIRE(pa[i].value);
        REQUIRE(pb[i].value);
        worst = std::max(worst, std::abs(*pb[i].value - *pa[i].value));
    }
    // A forward a fifth of a big figure away from the spot moves the quoted volatility by
    // about eight thousandths of a point: small, because a delta neutral ATM re-centres on the
    // new forward, and far above the 1e-12 the spot level itself moves it by. This is the
    // size of what the unit spot costs while the pair has no curves.
    CHECK(worst > 1e-5);
}
