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
                       {"6M", at(2027, 5, 11), 0.987, 0.982},
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
    CHECK(checked.error().contains("1M/10C"));
}

TEST_CASE("an expiry without a butterfly is refused, naming the expiry",
          "[app][fx_vol_surface_calibration_plan]") {
    const auto checked = check_recipe(eur_usd_recipe({"ATM", "25RR"}));
    REQUIRE_FALSE(checked);
    CHECK(checked.error().contains("expiry 1M lacks"));
}

TEST_CASE("a recipe the calibration cannot honour is refused by what it lacks",
          "[app][fx_vol_surface_calibration_plan]") {
    auto sabr = eur_usd_recipe();
    sabr.blueprint.smile_model = "sabr";
    CHECK(check_recipe(sabr).error().contains("sabr"));

    auto no_style = eur_usd_recipe();
    no_style.convention->butterfly_style.reset();
    CHECK(check_recipe(no_style).error().contains("no butterfly style"));

    auto no_convention = eur_usd_recipe();
    no_convention.convention.reset();
    CHECK(check_recipe(no_convention).error().contains("EUR-USD-FXOPTION"));

    auto wide = eur_usd_recipe();
    wide.blueprint.output_expiries = "1W,1Y";
    CHECK(check_recipe(wide).error().contains("output expiry 1W"));

    auto moneyness = eur_usd_recipe();
    moneyness.blueprint.output_strikes = "MNY/Fwd/1.1";
    CHECK(check_recipe(moneyness).error().contains("MNY/Fwd/1.1"));
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
    const auto prepared = prepare_calibration(
        recipe, *check_recipe(recipe), eur_usd_forward_set(), flat_smile_quotes(), 0.001, 3);
    REQUIRE(prepared);
    const auto& expiries = prepared->request.expiries;
    REQUIRE(expiries.size() == 3);
    CHECK(expiries[1].market.foreign_discount == Approx(0.975));
    CHECK(expiries[1].market.domestic_discount == Approx(0.958));
    // The time axis is the tenor grid, not the calendar: 1M is one twelfth of a year whatever
    // the month does, and a days-over-365 walk would make it 30/365 and shift every vol.
    CHECK(expiries[0].years == Approx(1.0 / 12.0));
    CHECK(expiries[1].years == Approx(1.0));
    CHECK(expiries[2].years == Approx(2.0));
    CHECK(expiries[1].convention.delta == qm::fx_delta_type::spot);
    CHECK(expiries[2].convention.delta == qm::fx_delta_type::forward);

    const auto& grid = prepared->request.grid_expiries;
    REQUIRE(grid.size() == 3);
    CHECK(grid[1].expiry_id == "6M");
    CHECK(grid[1].market.foreign_discount < 0.998);
    CHECK(grid[1].market.foreign_discount > 0.975);
}

TEST_CASE("a ladder's settlement run-up does not reach the pair's market",
          "[app][fx_vol_surface_calibration_plan]") {
    // An FX forward ladder carries an overnight and a tom-next row, and both value before spot.
    // Those are instruments, not discounts from spot: the pair's market is anchored at the spot
    // date, and a point before it is refused rather than interpolated.
    auto forwards = eur_usd_forward_set();
    const std::chrono::year_month_day before_spot{
        std::chrono::year{2026}, std::chrono::November, std::chrono::day{10}};
    forwards.points.insert(forwards.points.begin(),
                           {.tenor = "ON",
                            .value_date = before_spot,
                            .base_discount = 0.9999,
                            .quote_discount = 0.9999});

    const auto recipe = eur_usd_recipe();
    const auto prepared =
        prepare_calibration(recipe, *check_recipe(recipe), forwards, flat_smile_quotes(), 0.001, 3);
    REQUIRE(prepared);
    // The discounts come from the ladder's own points at and after spot, unchanged.
    const auto& expiries = prepared->request.expiries;
    REQUIRE(expiries.size() == 3);
    CHECK(expiries[0].market.foreign_discount == Approx(0.998));
    CHECK(expiries[0].market.domestic_discount == Approx(0.996));
}

TEST_CASE("an expiry discounts to the ladder's value date, not to one of its own",
          "[app][fx_vol_surface_calibration_plan]") {
    // Spot is a Friday and one month later is a Sunday, which the ladder rolls to the Monday
    // and publishes with the row. A surface that added the period here would discount to the
    // Sunday, interpolating between two rows instead of reading the one written for the tenor
    // it quotes.
    auto forwards = eur_usd_forward_set();
    const auto friday =
        year_month_day{std::chrono::year{2026}, std::chrono::month{11}, std::chrono::day{13}};
    const auto monday =
        year_month_day{std::chrono::year{2026}, std::chrono::month{12}, std::chrono::day{14}};
    forwards.spot_date = friday;
    for (auto& point : forwards.points)
        if (point.tenor == "1M")
            point.value_date = monday;

    const auto recipe = eur_usd_recipe();
    const auto prepared =
        prepare_calibration(recipe, *check_recipe(recipe), forwards, flat_smile_quotes(), 0.001, 3);
    REQUIRE(prepared);
    const auto& expiries = prepared->request.expiries;
    REQUIRE(expiries.size() == 3);
    // The ladder's own row, to the last digit: the Sunday would sit just above it.
    CHECK(expiries[0].market.foreign_discount == Approx(0.998).epsilon(1e-12));
    CHECK(expiries[0].market.domestic_discount == Approx(0.996).epsilon(1e-12));
}

TEST_CASE("a tenor the ladder does not carry is refused, naming it",
          "[app][fx_vol_surface_calibration_plan]") {
    // The grid quotes 6M. A forward set without that row cannot date the expiry, and adding the
    // period here would be a second way of computing a date: it is refused instead.
    auto forwards = eur_usd_forward_set();
    std::erase_if(forwards.points, [](const auto& point) { return point.tenor == "6M"; });

    const auto recipe = eur_usd_recipe();
    const auto prepared =
        prepare_calibration(recipe, *check_recipe(recipe), forwards, flat_smile_quotes(), 0.001, 3);
    REQUIRE_FALSE(prepared);
    CHECK(prepared.error().contains("6M"));
    CHECK(prepared.error().contains("no row"));
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

    const auto prepared =
        prepare_calibration(recipe, *check_recipe(recipe), eur_usd_forward_set(), quotes, 0.001, 3);
    REQUIRE(prepared);
    const auto& pillars = prepared->request.expiries[1].pillars;
    const auto atm = std::ranges::find(pillars, std::string("1Y/ATM"), &qs::ssvi_pillar::point_id);
    REQUIRE(atm != pillars.end());
    REQUIRE(atm->contributions.size() == 2);
    CHECK(atm->contributions[1].weight == Approx(3.0));
    CHECK(prepared->sources_by_point.at("1Y/ATM").size() == 2);
}

TEST_CASE("an unquoted pillar is reported, and an expiry it leaves short is not fitted",
          "[app][fx_vol_surface_calibration_plan]") {
    auto recipe = eur_usd_recipe({"ATM", "25RR", "25BF", "10RR", "10BF"});
    auto quotes = flat_smile_quotes();
    quotes[derived_quotes_series][{"1M", "10RR"}] = 0.0;

    const auto prepared =
        prepare_calibration(recipe, *check_recipe(recipe), eur_usd_forward_set(), quotes, 0.001, 3);
    REQUIRE(prepared);
    CHECK(prepared->missing.size() == 5);
    CHECK(prepared->missing.front().point == "1M/10BF");
    CHECK(prepared->missing.front().series_ids == std::vector{derived_quotes_series});
    CHECK(prepared->unfitted_expiries.empty());

    // 1Y without its 25 delta butterfly is short of what SSVI needs: it is left out and named,
    // and the other two expiries still make a surface.
    quotes[derived_quotes_series].erase({"1Y", "25BF"});
    const auto short_one =
        prepare_calibration(recipe, *check_recipe(recipe), eur_usd_forward_set(), quotes, 0.001, 2);
    REQUIRE(short_one);
    REQUIRE(short_one->unfitted_expiries.size() == 1);
    // The reason names every point 1Y lacks: its 25 delta butterfly, and the 10 delta points the
    // flat quotes never held.
    CHECK(short_one->unfitted_expiries.front().starts_with(
        "1Y: too few pillars for SSVI without 1Y/25BF"));
    CHECK(std::ranges::contains(
        short_one->missing, std::string("1Y/25BF"), &missing_surface_pillar::point));
    // Two fitted at a minimum of two is enough.
    REQUIRE(short_one->request.expiries.size() == 2);
    CHECK(short_one->request.expiries[0].expiry_id == "1M");
    CHECK(short_one->request.expiries[1].expiry_id == "2Y");
    // 6M lies between the fitted 1M and 2Y, so the grid keeps it: the term structure reads
    // across the gap 1Y left, as it reads between any two fitted expiries.
    REQUIRE(short_one->request.grid_expiries.size() == 3);
    CHECK(short_one->request.grid_expiries[1].expiry_id == "6M");

    // Below the minimum the surface is refused, naming how many it fitted and why the rest
    // were left out.
    const auto refused =
        prepare_calibration(recipe, *check_recipe(recipe), eur_usd_forward_set(), quotes, 0.001, 3);
    REQUIRE_FALSE(refused);
    CHECK(refused.error().contains("2 expiries fitted, 3 needed"));
    CHECK(refused.error().contains("1Y: "));
}

TEST_CASE("a surface whose longest expiry has no quote fits the rest and stops its grid there",
          "[app][fx_vol_surface_calibration_plan]") {
    const auto recipe = eur_usd_recipe();
    auto quotes = flat_smile_quotes();
    std::erase_if(quotes[derived_quotes_series],
                  [](const auto& entry) { return entry.first.first == "2Y"; });

    const auto prepared =
        prepare_calibration(recipe, *check_recipe(recipe), eur_usd_forward_set(), quotes, 0.001, 2);
    REQUIRE(prepared);
    REQUIRE(prepared->unfitted_expiries.size() == 1);
    CHECK(prepared->unfitted_expiries.front() == "2Y: no pillar quoted");
    REQUIRE(prepared->request.expiries.size() == 2);
    CHECK(prepared->request.expiries.back().expiry_id == "1Y");
    // The grid is 1M, 6M and 2Y; 2Y is past the fit, so the output carries 1M and 6M only.
    REQUIRE(prepared->request.grid_expiries.size() == 2);
    CHECK(prepared->request.grid_expiries[0].expiry_id == "1M");
    CHECK(prepared->request.grid_expiries[1].expiry_id == "6M");
    const auto fit = qs::calibrate_ssvi(prepared->request);
    REQUIRE(fit);
}

TEST_CASE("a flat market calibrates to a flat surface on the grid",
          "[app][fx_vol_surface_calibration_plan]") {
    const auto recipe = eur_usd_recipe();
    const auto prepared = prepare_calibration(
        recipe, *check_recipe(recipe), eur_usd_forward_set(), flat_smile_quotes(), 0.001, 3);
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
    CHECK(checked.error().contains("1M/ATM appears twice"));
}

TEST_CASE("a forward set that ends before an expiry refuses the calibration, naming it",
          "[app][fx_vol_surface_calibration_plan]") {
    auto forwards = eur_usd_forward_set();
    forwards.points.pop_back();
    const auto recipe = eur_usd_recipe();
    const auto prepared =
        prepare_calibration(recipe, *check_recipe(recipe), forwards, flat_smile_quotes(), 0.001, 3);
    REQUIRE_FALSE(prepared);
    CHECK(prepared.error().contains("expiry 2Y"));
    CHECK(prepared.error().contains("ends at"));
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
    CHECK(anchored.error().contains("no row on its spot date"));
    CHECK(anchored.error().contains("2026-12-01"));
}

namespace {

/// One source's quotes at 1M, 1Y and 2Y, quoted in the order a screen holds them.
std::vector<quoted_surface_point> source_quotes(double atm, double rr25, double bf25) {
    std::vector<quoted_surface_point> out;
    for (const std::string expiry : {"2Y", "1M", "1Y"}) {
        out.push_back({expiry, "ATM", atm});
        out.push_back({expiry, "25RR", rr25});
        out.push_back({expiry, "25BF", bf25});
    }
    return out;
}

/// The smile by delta a source's quotes fit to, by expiry and display label.
std::map<std::pair<std::string, std::string>, double>
fit_by_delta(const std::vector<quoted_surface_point>& quotes) {
    const auto made =
        make_quote_fit_recipe("EUR", "USD", *eur_usd_recipe().convention, "ice", quotes);
    REQUIRE(made);
    const auto checked = check_recipe(made->recipe);
    REQUIRE(checked);
    const auto prepared =
        prepare_calibration(made->recipe, *checked, eur_usd_forward_set(), made->quotes, 0.01, 1);
    REQUIRE(prepared);
    const auto fit = qs::calibrate_ssvi(prepared->request);
    REQUIRE(fit);
    std::map<std::pair<std::string, std::string>, double> out;
    for (const auto& point : fit->grid)
        out[{point.expiry_id, delta_label(point.strike_id)}] = point.volatility;
    return out;
}

}

TEST_CASE("a source's quotes become a recipe of its own expiries, in tenor order",
          "[app][fx_vol_surface_calibration_plan][quote_fit]") {
    auto quotes = source_quotes(0.10, 0.0, 0.0);
    quotes.push_back({"1M", "25C", 0.11});
    quotes.push_back({"1M", "10RR", std::nan("")});
    const auto made =
        make_quote_fit_recipe("EUR", "USD", *eur_usd_recipe().convention, "ice", quotes);
    REQUIRE(made);
    CHECK(made->recipe.blueprint.output_expiries == "1M,1Y,2Y");
    CHECK(made->recipe.blueprint.output_strikes.starts_with("DEL/"));
    // A call quote and a volatility that is not a number are left out.
    CHECK(made->recipe.pillars.size() == 9);
    CHECK(made->quotes.at(quote_fit_series()).size() == 9);
    CHECK(delta_label("DEL/PaSpot/Put/25") == "25P");
    CHECK(delta_label("DEL/Spot/Call/10") == "10C");
    CHECK(delta_label("ATM") == "ATM");

    CHECK_FALSE(make_quote_fit_recipe(
        "EUR", "USD", *eur_usd_recipe().convention, "ice", {{"soon", "ATM", 0.1}}));
    const auto empty = make_quote_fit_recipe("EUR", "USD", *eur_usd_recipe().convention, "B", {});
    REQUIRE_FALSE(empty);
    CHECK(empty.error().contains("B quotes no ATM"));
}

TEST_CASE("a flat smile reads the same volatility at every delta",
          "[app][fx_vol_surface_calibration_plan][quote_fit]") {
    const auto smile = fit_by_delta(source_quotes(0.10, 0.0, 0.0));
    REQUIRE(smile.size() == 15);
    for (const auto& [point, volatility] : smile)
        CHECK(volatility == Approx(0.10).margin(1e-4));
}

TEST_CASE("a positive risk reversal puts the call wing above the put wing",
          "[app][fx_vol_surface_calibration_plan][quote_fit]") {
    const auto smile = fit_by_delta(source_quotes(0.10, 0.01, 0.002));
    for (const std::string expiry : {"1M", "1Y", "2Y"}) {
        CHECK(smile.at({expiry, "25C"}) > smile.at({expiry, "25P"}));
        CHECK(smile.at({expiry, "25C"}) - smile.at({expiry, "25P"}) == Approx(0.01).margin(0.002));
        CHECK(smile.at({expiry, "ATM"}) == Approx(0.10).margin(0.002));
    }
}

TEST_CASE("a source that cannot be fitted is answered with why, and the others are still fitted",
          "[app][fx_vol_surface_calibration_plan][quote_fit]") {
    namespace mm = ores::marketdata::messaging;
    const auto source_of = [](const std::string& name, const std::vector<quoted_surface_point>& q) {
        mm::fx_vol_quote_source out;
        out.name = name;
        for (const auto& p : q)
            out.quotes.push_back(
                {.expiry = p.expiry, .label = p.label, .volatility = p.volatility});
        return out;
    };
    int reads = 0;
    const auto forwards_of = [&](const surface_calibration_recipe&) {
        ++reads;
        return eur_usd_forward_set();
    };
    const auto fits = fit_quote_sources("EUR",
                                        "USD",
                                        *eur_usd_recipe().convention,
                                        {source_of("empty", {}),
                                         source_of("ice", source_quotes(0.10, 0.0, 0.0)),
                                         source_of("tradeweb", source_quotes(0.12, 0.0, 0.0))},
                                        forwards_of,
                                        0.01);
    REQUIRE(fits);
    REQUIRE(fits->size() == 3);
    CHECK((*fits)[0].name == "empty");
    CHECK_FALSE((*fits)[0].fitted);
    CHECK((*fits)[0].message.contains("empty quotes no ATM"));
    CHECK((*fits)[1].fitted);
    CHECK((*fits)[1].points.size() == 15);
    CHECK((*fits)[2].fitted);
    CHECK((*fits)[2].points.front().volatility == Approx(0.12).margin(1e-4));
    CHECK(reads == 1);

    const std::vector<mm::fx_vol_quote_source> crowd(most_fit_sources + 1,
                                                     source_of("ice", source_quotes(0.1, 0, 0)));
    const auto refused =
        fit_quote_sources("EUR", "USD", *eur_usd_recipe().convention, crowd, forwards_of, 0.01);
    REQUIRE_FALSE(refused);
    CHECK(refused.error().contains("at most 8 sources"));

    std::vector<quoted_surface_point> many(most_fit_quotes + 1, {"1M", "ATM", 0.1});
    const auto large = fit_quote_sources(
        "EUR", "USD", *eur_usd_recipe().convention, {source_of("ice", many)}, forwards_of, 0.01);
    REQUIRE_FALSE(large);
    CHECK(large.error().contains("ice holds 1001"));
    CHECK(reads == 1);
}
