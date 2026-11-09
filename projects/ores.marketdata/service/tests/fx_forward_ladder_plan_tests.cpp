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
#include "ores.marketdata.service/app/fx_forward_ladder_service.hpp"
#include "ores.testing/database_helper.hpp"
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <chrono>
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
    const auto eur = parse_usd_pair("EUR/USD");
    REQUIRE(eur.has_value());
    CHECK(eur->base == "EUR");
    CHECK(eur->quote == "USD");
    CHECK(parse_usd_pair("USD/JPY").has_value());
}

TEST_CASE("a cross and a malformed pair are refused", "[fx_forward_ladder]") {
    CHECK_THAT(parse_usd_pair("EUR/GBP").error(), ContainsSubstring("no USD leg"));
    CHECK_THAT(parse_usd_pair("EURUSD").error(), ContainsSubstring("not a currency pair"));
    CHECK_THAT(parse_usd_pair("/USD").error(), ContainsSubstring("not a currency pair"));
    CHECK_THAT(parse_usd_pair("EUR/USD/GBP").error(), ContainsSubstring("not a currency pair"));
    CHECK_THAT(parse_usd_pair("USD/USD").error(), ContainsSubstring("twice"));
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

TEST_CASE("the read refuses a cross through the service", "[fx_forward_ladder][db]") {
    ores::testing::database_helper h;
    const auto reply = ores::marketdata::service::app::fx_forward_ladder_service::read(
        h.context(), {.pair_code = "EUR/GBP", .as_of = at(monday)});
    CHECK_FALSE(reply.success);
    CHECK(reply.rows.empty());
    CHECK_THAT(reply.message, ContainsSubstring("no USD leg"));
}

TEST_CASE("the read names a pair with no convention", "[fx_forward_ladder][db]") {
    ores::testing::database_helper h;
    const auto reply = ores::marketdata::service::app::fx_forward_ladder_service::read(
        h.context(), {.pair_code = "ZZZ/USD", .as_of = at(monday)});
    CHECK_FALSE(reply.success);
    CHECK_THAT(reply.message, ContainsSubstring("No convention is published for ZZZ/USD"));
}
