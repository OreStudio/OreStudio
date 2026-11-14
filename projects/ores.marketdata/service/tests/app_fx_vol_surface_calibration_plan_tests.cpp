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
#include "ores.marketdata.service/app/fx_vol_surface_calibration_plan.hpp"
#include "ores.testing/fx_vol_surface_fixture.hpp"
#include "ores.testing/uuid_of.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cmath>
#include <format>

namespace {

using namespace ores::marketdata::service::app;
namespace qs = ores::analytics::quant::service;
namespace qm = ores::analytics::quant::math;
using Catch::Approx;
using ores::testing::uuid_of;
using std::chrono::year_month_day;

const auto calibration_as_of =
    year_month_day{std::chrono::year{2026}, std::chrono::month{11}, std::chrono::day{9}};
const auto forward_spot_date =
    year_month_day{std::chrono::year{2026}, std::chrono::month{11}, std::chrono::day{11}};
const auto derived_quotes_series = uuid_of("00000000-0000-4000-8000-0000000000e1");

/// EURUSD_SSVI at 1M, 1Y and 2Y with the given labels, every pillar reading the derived quotes.
surface_calibration_recipe eur_usd_recipe(const std::vector<std::string>& labels = {
                                              "ATM", "25RR", "25BF"}) {
    auto rows = ores::testing::eur_usd_ssvi_surface(uuid_of("00000000-0000-4000-8000-0000000000a2"),
                                                    derived_quotes_series,
                                                    {"1M", "1Y", "2Y"},
                                                    labels);
    return {.blueprint = std::move(rows.blueprint),
            .pillars = std::move(rows.pillars),
            .sources = std::move(rows.sources),
            .convention = ores::testing::eur_usd_fx_option_convention()};
}

surface_forward_set eur_usd_forward_set() {
    using namespace std::chrono;
    const auto at = [](int y, int m, int d) {
        return year_month_day{
            year{y}, month{static_cast<unsigned>(m)}, day{static_cast<unsigned>(d)}};
    };
    return {.publish_id = "fwd-1",
            .spot = 1.08,
            .spot_date = forward_spot_date,
            .points = {{"1M", at(2026, 12, 11), 0.998, 0.996},
                       {"1Y", at(2027, 11, 11), 0.975, 0.958},
                       {"2Y", at(2028, 11, 13), 0.950, 0.918}}};
}

/// A flat smile: every ATM at 10 percent, every risk reversal and butterfly at zero.
std::map<boost::uuids::uuid, surface_series_quotes> flat_smile_quotes() {
    surface_series_quotes q;
    for (const std::string expiry : {"1M", "1Y", "2Y"}) {
        q[{expiry, "ATM"}] = 0.10;
        q[{expiry, "25RR"}] = 0.0;
        q[{expiry, "25BF"}] = 0.0;
    }
    return {{derived_quotes_series, q}};
}

}

TEST_CASE("a recipe with a single call pillar is refused before any quote is read",
          "[app][fx_vol_surface_calibration_plan]") {
    const auto checked = check_recipe(eur_usd_recipe({"ATM", "25RR", "25BF", "10C"}));
    REQUIRE_FALSE(checked);
    CHECK(checked.error().find("1M/10C") != std::string::npos);
}

TEST_CASE("an expiry without a butterfly is refused, naming the expiry",
          "[app][fx_vol_surface_calibration_plan]") {
    const auto checked = check_recipe(eur_usd_recipe({"ATM", "25RR"}));
    REQUIRE_FALSE(checked);
    CHECK(checked.error().find("expiry 1M lacks") != std::string::npos);
}

TEST_CASE("a recipe the calibration cannot honour is refused by what it lacks",
          "[app][fx_vol_surface_calibration_plan]") {
    auto sabr = eur_usd_recipe();
    sabr.blueprint.smile_model = "sabr";
    CHECK(check_recipe(sabr).error().find("sabr") != std::string::npos);

    auto no_style = eur_usd_recipe();
    no_style.convention->butterfly_style.reset();
    CHECK(check_recipe(no_style).error().find("no butterfly style") != std::string::npos);

    auto no_convention = eur_usd_recipe();
    no_convention.convention.reset();
    CHECK(check_recipe(no_convention).error().find("EUR-USD-FXOPTION") != std::string::npos);

    auto wide = eur_usd_recipe();
    wide.blueprint.output_expiries = "1W,1Y";
    CHECK(check_recipe(wide).error().find("output expiry 1W") != std::string::npos);

    auto moneyness = eur_usd_recipe();
    moneyness.blueprint.output_strikes = "MNY/Fwd/1.1";
    CHECK(check_recipe(moneyness).error().find("MNY/Fwd/1.1") != std::string::npos);
}

TEST_CASE("a checked recipe orders its expiries and reads its grid",
          "[app][fx_vol_surface_calibration_plan]") {
    auto shuffled = eur_usd_recipe();
    std::ranges::reverse(shuffled.pillars);
    const auto checked = check_recipe(shuffled);
    REQUIRE(checked);
    REQUIRE(checked->expiries.size() == 3);
    CHECK(checked->expiries[0].code == "1M");
    CHECK(checked->expiries[1].code == "1Y");
    CHECK(checked->expiries[2].code == "2Y");
    REQUIRE(checked->grid_strikes.size() == 3);
    const auto* put = std::get_if<qs::ssvi_delta_strike>(&checked->grid_strikes[0].at);
    REQUIRE(put);
    CHECK(put->side == qm::option_side::put);
    CHECK(put->delta == Approx(0.25));
    CHECK(std::holds_alternative<qs::ssvi_atm_strike>(checked->grid_strikes[1].at));
}

TEST_CASE("each expiry reads its market from the forward set and its convention from the switch",
          "[app][fx_vol_surface_calibration_plan]") {
    const auto recipe = eur_usd_recipe();
    const auto prepared = prepare_calibration(recipe,
                                              *check_recipe(recipe),
                                              eur_usd_forward_set(),
                                              flat_smile_quotes(),
                                              calibration_as_of,
                                              0.001);
    REQUIRE(prepared);
    const auto& expiries = prepared->request.expiries;
    REQUIRE(expiries.size() == 3);
    CHECK(expiries[1].market.foreign_discount == Approx(0.975));
    CHECK(expiries[1].market.domestic_discount == Approx(0.958));
    CHECK(expiries[1].years == Approx(365.0 / 365.0));
    CHECK(expiries[1].convention.delta == qm::fx_delta_type::spot);
    CHECK(expiries[2].convention.delta == qm::fx_delta_type::forward);

    const auto& grid = prepared->request.grid_expiries;
    REQUIRE(grid.size() == 3);
    CHECK(grid[1].expiry_id == "6M");
    CHECK(grid[1].market.foreign_discount < 0.998);
    CHECK(grid[1].market.foreign_discount > 0.975);
}

TEST_CASE("a pillar's quote is each contributor's at its weight",
          "[app][fx_vol_surface_calibration_plan]") {
    auto recipe = eur_usd_recipe();
    const auto& atm_1y = *std::ranges::find_if(recipe.pillars, [](const auto& p) {
        return p.expiry_code == "1Y" && p.strike_label == "ATM";
    });
    ores::refdata::domain::fx_vol_surface_pillar_source broker;
    broker.pillar_id = atm_1y.id;
    broker.market_series_id = uuid_of("00000000-0000-4000-8000-0000000000e2");
    broker.contributor_code = "broker";
    broker.weight = 3.0;
    recipe.sources.push_back(broker);
    auto quotes = flat_smile_quotes();
    quotes[broker.market_series_id][{"1Y", "ATM"}] = 0.12;

    const auto prepared = prepare_calibration(
        recipe, *check_recipe(recipe), eur_usd_forward_set(), quotes, calibration_as_of, 0.001);
    REQUIRE(prepared);
    const auto& pillars = prepared->request.expiries[1].pillars;
    const auto atm = std::ranges::find(pillars, std::string("1Y/ATM"), &qs::ssvi_pillar::point_id);
    REQUIRE(atm != pillars.end());
    REQUIRE(atm->contributions.size() == 2);
    CHECK(atm->contributions[1].weight == Approx(3.0));
    CHECK(prepared->sources_by_point.at("1Y/ATM").size() == 2);
}

TEST_CASE("an unquoted pillar is reported, and an expiry it leaves short is refused",
          "[app][fx_vol_surface_calibration_plan]") {
    auto recipe = eur_usd_recipe({"ATM", "25RR", "25BF", "10RR", "10BF"});
    auto quotes = flat_smile_quotes();
    quotes[derived_quotes_series][{"1M", "10RR"}] = 0.0;

    const auto prepared = prepare_calibration(
        recipe, *check_recipe(recipe), eur_usd_forward_set(), quotes, calibration_as_of, 0.001);
    REQUIRE(prepared);
    CHECK(prepared->missing.size() == 5);
    CHECK(prepared->missing.front().point == "1M/10BF");
    CHECK(prepared->missing.front().series_ids == std::vector{derived_quotes_series});

    quotes[derived_quotes_series].erase({"1Y", "25BF"});
    const auto refused = prepare_calibration(
        recipe, *check_recipe(recipe), eur_usd_forward_set(), quotes, calibration_as_of, 0.001);
    REQUIRE_FALSE(refused);
    CHECK(refused.error().find("expiry 1Y") != std::string::npos);
    CHECK(refused.error().find("1Y/25BF") != std::string::npos);
}

TEST_CASE("a flat market calibrates to a flat surface on the grid",
          "[app][fx_vol_surface_calibration_plan]") {
    const auto recipe = eur_usd_recipe();
    const auto prepared = prepare_calibration(recipe,
                                              *check_recipe(recipe),
                                              eur_usd_forward_set(),
                                              flat_smile_quotes(),
                                              calibration_as_of,
                                              0.001);
    REQUIRE(prepared);
    const auto fit = qs::calibrate_ssvi(prepared->request);
    REQUIRE(fit);
    REQUIRE(fit->grid.size() == 9);
    for (const auto& point : fit->grid)
        CHECK(point.volatility == Approx(0.10).margin(5e-4));
    CHECK(fit->grid[0].log_moneyness < fit->grid[1].log_moneyness);
    CHECK(fit->grid[1].log_moneyness < fit->grid[2].log_moneyness);
}

TEST_CASE("a pillar named twice is refused before any quote is read",
          "[app][fx_vol_surface_calibration_plan]") {
    auto recipe = eur_usd_recipe();
    recipe.pillars.push_back(recipe.pillars.front());
    const auto checked = check_recipe(recipe);
    REQUIRE_FALSE(checked);
    CHECK(checked.error().find("1M/ATM appears twice") != std::string::npos);
}

TEST_CASE("a forward set that ends before an expiry refuses the calibration, naming it",
          "[app][fx_vol_surface_calibration_plan]") {
    auto forwards = eur_usd_forward_set();
    forwards.points.pop_back();
    const auto recipe = eur_usd_recipe();
    const auto prepared = prepare_calibration(
        recipe, *check_recipe(recipe), forwards, flat_smile_quotes(), calibration_as_of, 0.001);
    REQUIRE_FALSE(prepared);
    CHECK(prepared.error().find("expiry 2Y") != std::string::npos);
    CHECK(prepared.error().find("ends at") != std::string::npos);
}

TEST_CASE("a_published_forward_set_is_read_from_spot_and_not_from_its_as_of",
          "[app][fx_vol_surface_calibration_plan]") {
    using namespace std::chrono;
    const auto at = [](int d) {
        return year_month_day{
            year{2026}, month{static_cast<unsigned>(11)}, day{static_cast<unsigned>(d)}};
    };
    // A published set as the ladder writes it: ON before spot, and factors that discount
    // from the as-of rather than from spot, so the spot row is not one.
    const surface_forward_set published{.publish_id = "fwd-2",
                                        .spot = 1.08,
                                        .spot_date = at(11),
                                        .points = {{"ON", at(9), 0.9997500, 0.9996316},
                                                   {"TN", at(11), 0.9996667, 0.9995088},
                                                   {"1M", at(11), 0.9980000, 0.9960000}}};

    const auto anchored = spot_starting(published);

    INFO("error: " << (anchored ? std::string{} : anchored.error()));
    REQUIRE(anchored);
    REQUIRE(anchored->points.size() == 2);
    // ON is gone, and the spot row discounts to exactly one on both sides.
    CHECK(anchored->points.front().tenor == "TN");
    CHECK(anchored->points.front().base_discount == Approx(1.0).epsilon(1e-12));
    CHECK(anchored->points.front().quote_discount == Approx(1.0).epsilon(1e-12));
    // A later row is rebased by the same anchor, so spot-relative discounting is preserved.
    CHECK(anchored->points.back().base_discount == Approx(0.9980000 / 0.9996667).epsilon(1e-12));
    CHECK(anchored->points.back().quote_discount == Approx(0.9960000 / 0.9995088).epsilon(1e-12));
}

TEST_CASE("a_forward_set_with_no_row_on_its_spot_date_is_refused_naming_it",
          "[app][fx_vol_surface_calibration_plan]") {
    auto forwards = eur_usd_forward_set();
    forwards.spot_date = std::chrono::year_month_day{
        std::chrono::year{2026}, std::chrono::month{12}, std::chrono::day{1}};

    const auto anchored = spot_starting(forwards);

    REQUIRE_FALSE(anchored);
    CHECK(anchored.error().find("no row on its spot date") != std::string::npos);
    CHECK(anchored.error().find("2026-12-01") != std::string::npos);
}
