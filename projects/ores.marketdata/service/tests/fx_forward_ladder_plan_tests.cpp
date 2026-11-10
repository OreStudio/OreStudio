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
#include "../src/fx_forward_ladder_plan.hpp"
#include "ores.analytics.quant/service/fx_forward_calculator.hpp"
#include "ores.marketdata.service/app/fx_forward_ladder_service.hpp"
#include "ores.testing/database_helper.hpp"
#include <algorithm>
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <chrono>
#include <cmath>
#include <string>
#include <vector>

using namespace ores::marketdata::service;
using namespace std::chrono;
using ores::analytics::quant::domain::business_day_convention;
using ores::marketdata::domain::curve_publish;
using ores::refdata::domain::calendar_date;
using Catch::Matchers::ContainsSubstring;

namespace {

// A Monday.
const year_month_day monday = 2026y / November / 9d;

time_point<system_clock> at(year_month_day d) {
    return sys_days(d);
}

curve_publish applied(year_month_day as_of, year_month_day published) {
    curve_publish c;
    c.as_of = at(as_of);
    c.published_at = at(published);
    return c;
}

std::vector<calendar_date> calendar_closed_on(std::vector<year_month_day> days) {
    std::vector<calendar_date> out;
    for (const auto& d : days) {
        calendar_date c;
        c.date = d;
        c.is_business_day = false;
        out.push_back(c);
    }
    return out;
}

const auto open_calendar = [] {
    std::vector<std::vector<calendar_date>> none{{}};
    return joint_calendar(none);
}();

fx_ladder_dates
ladder(int spot_days,
       const ores::analytics::quant::domain::business_day_calendar_set& c = open_calendar) {
    return build_ladder_dates(
        monday, spot_days, business_day_convention::modified_following, false, c);
}

const auto& row(const fx_ladder_dates& l, const std::string& id) {
    const auto it = std::ranges::find(
        l.tenors, id, &ores::analytics::quant::service::fx_forward_tenor::tenor_id);
    REQUIRE(it != l.tenors.end());
    return *it;
}

}

TEST_CASE("a USD pair splits into base and quote", "[fx_forward_ladder]") {
    const auto eur = parse_pair("EUR/USD");
    REQUIRE(eur.has_value());
    CHECK(eur->base == "EUR");
    CHECK(eur->quote == "USD");
    CHECK(parse_pair("USD/JPY").has_value());
}

TEST_CASE("a cross is a pair with no USD leg", "[fx_forward_ladder]") {
    const auto eur_gbp = parse_pair("EUR/GBP");
    REQUIRE(eur_gbp.has_value());
    CHECK(is_cross(*eur_gbp));
    CHECK_FALSE(is_cross(*parse_pair("EUR/USD")));
    CHECK_FALSE(is_cross(*parse_pair("USD/JPY")));
}

TEST_CASE("a malformed pair is refused", "[fx_forward_ladder]") {
    CHECK_THAT(parse_pair("EURUSD").error(), ContainsSubstring("not a currency pair"));
    CHECK_THAT(parse_pair("/USD").error(), ContainsSubstring("not a currency pair"));
    CHECK_THAT(parse_pair("EUR/USD/GBP").error(), ContainsSubstring("not a currency pair"));
    CHECK_THAT(parse_pair("USD/USD").error(), ContainsSubstring("twice"));
}

TEST_CASE("a reader takes the latest set at or before its as-of", "[fx_forward_ladder]") {
    const std::vector sets{applied(2026y / November / 6d, 2026y / November / 6d),
                           applied(monday, monday),
                           applied(2026y / November / 10d, 2026y / November / 10d)};
    const auto chosen = select_published_set(sets, at(monday));
    REQUIRE(chosen.has_value());
    CHECK(chosen->as_of == at(monday));
}

TEST_CASE("the set applied last wins among equal as-ofs", "[fx_forward_ladder]") {
    auto first = applied(monday, monday);
    first.published_by = "first";
    auto second = applied(monday, 2026y / November / 10d);
    second.published_by = "second";
    const std::vector sets{second, first};
    CHECK(select_published_set(sets, at(2026y / November / 11d))->published_by == "second");
}

TEST_CASE("a withdrawn set is passed over and a superseded one is kept", "[fx_forward_ladder]") {
    auto withdrawn = applied(monday, monday);
    withdrawn.withdrawn_at = at(monday);
    auto superseded = applied(2026y / November / 6d, 2026y / November / 6d);
    superseded.superseded_at = at(monday);
    const std::vector sets{withdrawn, superseded};
    CHECK(select_published_set(sets, at(monday))->as_of == at(2026y / November / 6d));
}

TEST_CASE("no set stands when every set is later or waiting", "[fx_forward_ladder]") {
    curve_publish waiting;
    waiting.as_of = at(monday);
    const std::vector sets{waiting, applied(2026y / November / 10d, 2026y / November / 10d)};
    CHECK_FALSE(select_published_set(sets, at(monday)).has_value());
}

TEST_CASE("a stored curve gains its reference date once", "[fx_forward_ladder]") {
    const auto anchored = anchor_curve({{"1M", 2026y / December / 9d, 0.99}}, monday);
    REQUIRE(anchored.size() == 2);
    CHECK(anchored.front().date == monday);
    CHECK(anchored.front().discount_factor == 1.0);
    CHECK(anchor_curve(anchored, monday).size() == 2);
}

TEST_CASE("a T+2 pair has ON and TN chaining to spot", "[fx_forward_ladder]") {
    const auto l = ladder(2);
    CHECK(l.spot_date == 2026y / November / 11d);
    CHECK(row(l, "ON").near_date == monday);
    CHECK(row(l, "ON").value_date == 2026y / November / 10d);
    CHECK(row(l, "TN").near_date == 2026y / November / 10d);
    CHECK(row(l, "TN").value_date == l.spot_date);
    CHECK(row(l, "SP").value_date == l.spot_date);
    CHECK_FALSE(row(l, "SP").near_date.has_value());
    CHECK(row(l, "SN").value_date == 2026y / November / 12d);
}

TEST_CASE("a T+1 pair has no TN", "[fx_forward_ladder]") {
    const auto l = ladder(1);
    CHECK(l.spot_date == 2026y / November / 10d);
    CHECK(std::ranges::none_of(l.tenors, [](const auto& t) { return t.tenor_id == "TN"; }));
}

TEST_CASE("later tenors run from spot with no near date of their own", "[fx_forward_ladder]") {
    const auto l = ladder(2);
    CHECK(row(l, "1W").value_date == 2026y / November / 18d);
    CHECK(row(l, "1M").value_date == 2026y / December / 11d);
    CHECK(row(l, "1Y").value_date == 2027y / November / 11d);
    CHECK_FALSE(row(l, "1M").near_date.has_value());
}

TEST_CASE("a holiday in either calendar moves the spot date", "[fx_forward_ladder]") {
    // Tuesday closed in one calendar, Wednesday in the other.
    const std::vector calendars{calendar_closed_on({2026y / November / 10d}),
                                calendar_closed_on({2026y / November / 11d})};
    const auto joint = joint_calendar(calendars);
    CHECK(ladder(2, joint).spot_date == 2026y / November / 13d);
}

TEST_CASE("a value date on a weekend rolls under the convention", "[fx_forward_ladder]") {
    // Spot Friday 2026-11-13 plus one month is Sunday 13 December.
    const auto l = build_ladder_dates(2026y / November / 11d,
                                      2,
                                      business_day_convention::modified_following,
                                      false,
                                      open_calendar);
    CHECK(l.spot_date == 2026y / November / 13d);
    CHECK(row(l, "1M").value_date == 2026y / December / 14d);
}

TEST_CASE("an as-of on a weekend counts from the next business day", "[fx_forward_ladder]") {
    const auto l = build_ladder_dates(2026y / November / 7d,
                                      2,
                                      business_day_convention::modified_following,
                                      false,
                                      open_calendar);
    CHECK(row(l, "ON").near_date == monday);
    CHECK(l.spot_date == 2026y / November / 11d);
}

TEST_CASE("the compute names a pair with no convention", "[fx_forward_ladder][db]") {
    ores::testing::database_helper h;
    CHECK_THROWS_WITH(ores::marketdata::service::app::fx_forward_ladder_service::compute(
                          h.context(), {.pair_code = "ZZZ/USD", .as_of = at(monday)}),
                      ContainsSubstring("No convention is published for ZZZ/USD"));
}

TEST_CASE("a cross rate divides the USD legs in either direction", "[fx_forward_ladder]") {
    const usd_leg eur{.currency = "EUR", .value = 1.10, .usd_is_base = false};
    const usd_leg gbp{.currency = "GBP", .value = 1.25, .usd_is_base = false};
    const usd_leg jpy{.currency = "JPY", .value = 150.0, .usd_is_base = true};
    CHECK(cross_rate(eur, gbp) == Catch::Approx(0.88).epsilon(1e-12));
    CHECK(cross_rate(eur, jpy) == Catch::Approx(165.0).epsilon(1e-12));
    CHECK(cross_rate(gbp, eur) == Catch::Approx(1.0 / 0.88).epsilon(1e-12));
}

TEST_CASE("a cross forward equals the ratio of its USD-leg forwards on the same dates",
          "[fx_forward_ladder]") {
    namespace quant = ores::analytics::quant::service;
    const auto flat = [](double rate) {
        const auto far = year_month_day{sys_days(monday) + days{4000}};
        return std::vector<quant::bootstrapped_point>{
            {"", monday, 1.0}, {"LONG", far, std::exp(-rate * 4000 / 365.0)}};
    };
    const auto dates = ladder(2);
    const auto forwards = [&](double spot, double base_rate, double quote_rate) {
        const auto rows = quant::calculate_fx_forwards({.base_currency = "B",
                                                        .quote_currency = "Q",
                                                        .spot = spot,
                                                        .spot_date = dates.spot_date,
                                                        .pip_factor = 0.0001,
                                                        .base_curve = flat(base_rate),
                                                        .quote_curve = flat(quote_rate),
                                                        .tenors = dates.tenors});
        REQUIRE(rows.has_value());
        return *rows;
    };
    const usd_leg eur{.currency = "EUR", .value = 1.10, .usd_is_base = false};
    const usd_leg gbp{.currency = "GBP", .value = 1.25, .usd_is_base = false};
    const auto eur_usd = forwards(1.10, 0.020, 0.045);
    const auto gbp_usd = forwards(1.25, 0.038, 0.045);
    const auto eur_gbp = forwards(cross_rate(eur, gbp), 0.020, 0.038);

    REQUIRE(eur_gbp.size() == eur_usd.size());
    for (std::size_t i = 0; i < eur_gbp.size(); ++i)
        CHECK(eur_gbp[i].forward_rate ==
              Catch::Approx(eur_usd[i].forward_rate / gbp_usd[i].forward_rate).epsilon(1e-12));
}

namespace {

struct quantlib_cross_case {
    year_month_day as_of;
    std::vector<year_month_day> first_holidays;
    std::vector<year_month_day> second_holidays;
    year_month_day spot, sn, one_week, one_month, three_months, one_year;
};

}

// The dates QuantLib gives each case, from projects/ores.ore/scripts/quantlib_fx_dates
// (generate.sh, QuantLib in ORE 1.8.17.0). A cross advances on its two currencies' calendars
// only, as ORE's EUR-GBP-FX-CONVENTIONS does on TARGET,UK, so the first EUR/GBP spot falls on
// US Thanksgiving, 26 November 2026, and stays there.
TEST_CASE("a cross's spot and value dates match QuantLib's", "[fx_forward_ladder]") {
    const std::vector<quantlib_cross_case> cases{
        // EUR/GBP on TARGET and UnitedKingdom.Settlement.
        {2026y / 11 / 24d,
         {2026y / 12 / 25d, 2027y / 1 / 1d, 2027y / 3 / 26d, 2027y / 3 / 29d},
         {2026y / 12 / 25d, 2026y / 12 / 28d, 2027y / 1 / 1d, 2027y / 3 / 26d, 2027y / 3 / 29d,
          2027y / 5 / 3d, 2027y / 5 / 31d, 2027y / 8 / 30d},
         2026y / 11 / 26d, 2026y / 11 / 27d, 2026y / 12 / 3d, 2026y / 12 / 29d, 2027y / 2 / 26d,
         2027y / 11 / 26d},
        {2026y / 12 / 23d,
         {2026y / 12 / 25d, 2027y / 1 / 1d, 2027y / 3 / 26d, 2027y / 3 / 29d},
         {2026y / 12 / 25d, 2026y / 12 / 28d, 2027y / 1 / 1d, 2027y / 3 / 26d, 2027y / 3 / 29d,
          2027y / 5 / 3d, 2027y / 5 / 31d, 2027y / 8 / 30d, 2027y / 12 / 27d, 2027y / 12 / 28d,
          2028y / 1 / 3d},
         2026y / 12 / 29d, 2026y / 12 / 30d, 2027y / 1 / 5d, 2027y / 1 / 29d, 2027y / 3 / 30d,
         2027y / 12 / 29d},
        // EUR/JPY on TARGET and Japan.
        {2026y / 12 / 29d,
         {2027y / 1 / 1d, 2027y / 3 / 26d, 2027y / 3 / 29d},
         {2026y / 12 / 31d, 2027y / 1 / 1d, 2027y / 1 / 11d, 2027y / 2 / 11d, 2027y / 2 / 23d,
          2027y / 3 / 22d, 2027y / 4 / 29d, 2027y / 5 / 3d, 2027y / 5 / 4d, 2027y / 5 / 5d,
          2027y / 7 / 19d, 2027y / 8 / 11d, 2027y / 9 / 20d, 2027y / 9 / 23d, 2027y / 10 / 11d,
          2027y / 11 / 3d, 2027y / 11 / 23d, 2027y / 12 / 31d, 2028y / 1 / 3d, 2028y / 1 / 10d},
         2027y / 1 / 4d, 2027y / 1 / 5d, 2027y / 1 / 12d, 2027y / 2 / 4d, 2027y / 4 / 5d,
         2028y / 1 / 4d},
        // AUD/NZD on Australia and NewZealand.
        {2027y / 1 / 22d,
         {2027y / 1 / 26d, 2027y / 3 / 26d, 2027y / 3 / 29d, 2027y / 6 / 14d, 2027y / 8 / 2d,
          2027y / 10 / 4d, 2027y / 12 / 27d, 2027y / 12 / 28d, 2028y / 1 / 3d, 2028y / 1 / 26d},
         {2027y / 1 / 25d, 2027y / 2 / 8d, 2027y / 3 / 26d, 2027y / 3 / 29d, 2027y / 4 / 26d,
          2027y / 6 / 7d, 2027y / 6 / 25d, 2027y / 10 / 25d, 2027y / 12 / 27d, 2027y / 12 / 28d,
          2028y / 1 / 3d, 2028y / 1 / 4d, 2028y / 1 / 24d, 2028y / 2 / 7d},
         2027y / 1 / 28d, 2027y / 1 / 29d, 2027y / 2 / 4d, 2027y / 2 / 26d, 2027y / 4 / 28d,
         2028y / 1 / 28d},
    };
    for (const auto& c : cases) {
        const std::vector calendars{calendar_closed_on(c.first_holidays),
                                    calendar_closed_on(c.second_holidays)};
        const auto l = build_ladder_dates(
            c.as_of, 2, business_day_convention::modified_following, false,
            joint_calendar(calendars));
        INFO("as of " << static_cast<int>(c.as_of.year()) << "-"
                      << static_cast<unsigned>(c.as_of.month()) << "-"
                      << static_cast<unsigned>(c.as_of.day()));
        CHECK(l.spot_date == c.spot);
        CHECK(row(l, "SN").value_date == c.sn);
        CHECK(row(l, "1W").value_date == c.one_week);
        CHECK(row(l, "1M").value_date == c.one_month);
        CHECK(row(l, "3M").value_date == c.three_months);
        CHECK(row(l, "1Y").value_date == c.one_year);
    }
}
