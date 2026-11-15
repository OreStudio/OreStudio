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
#include "ores.analytics.quant/domain/business_day_calendar_set.hpp"
#include "ores.analytics.quant/domain/calendar_ruleset.hpp"
#include "ores.analytics.quant/domain/date_convention.hpp"
#include "ores.analytics.quant/service/calendar_arithmetic.hpp"
#include "ores.analytics.quant/service/calendar_rule_engine.hpp"
#include "ores.analytics.quant/service/fx_expiry_dates.hpp"
#include "ores.analytics.quant/service/quantlib_calendar_rulesets.hpp"
#include <bitset>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <stdexcept>
#include <utility>
#include <vector>

namespace {

using ores::analytics::quant::domain::business_day_calendar_set;
using ores::analytics::quant::domain::business_day_convention;
using ores::analytics::quant::domain::calendar_date_row;
using ores::analytics::quant::domain::calendar_ruleset;
using ores::analytics::quant::domain::date_period;
using ores::analytics::quant::domain::period_unit;
using ores::analytics::quant::service::calendar_arithmetic;
using ores::analytics::quant::service::calendar_rule_engine;
using ores::analytics::quant::service::fx_vol_tenor_years;
using ores::analytics::quant::service::fx_expiry_date_rule;
using ores::analytics::quant::service::quantlib_calendar_rulesets;

using namespace std::chrono;

constexpr std::size_t only_calendar = 0;

/// South Africa is the pair's calendar throughout: it has a holiday between the
/// dates the cases meet, so a rule that ignored it would answer the wrong day.
business_day_calendar_set south_africa_calendar(year from, year to) {
    const std::vector<calendar_ruleset> rulesets = {quantlib_calendar_rulesets::south_africa()};
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        rulesets, from / January / 1, to / December / 31);

    std::vector<calendar_date_row> rows;
    rows.reserve(holidays.size());
    for (const auto& holiday : holidays)
        rows.push_back({holiday.calendar_index, holiday.date});

    std::vector<std::bitset<7>> weekend_masks;
    for (const auto& ruleset : rulesets)
        weekend_masks.push_back(ruleset.weekend_mask);

    return business_day_calendar_set::from_rows(rows, rulesets.size(), std::move(weekend_masks));
}

fx_expiry_date_rule rule_today(business_day_convention roll, bool end_of_month = false) {
    return fx_expiry_date_rule(
        roll, end_of_month, only_calendar, south_africa_calendar(year{2025}, year{2027}));
}

date_period week_periods(int n) {
    return {.unit = period_unit::week, .multiplier = n};
}

date_period month_periods(int n) {
    return {.unit = period_unit::month, .multiplier = n};
}

}

TEST_CASE("a month expiry from a Friday spot lands on the Monday the ladder publishes",
          "[fx_expiry_dates]") {
    // 13 November 2026 is a Friday, so one month later is the Sunday of the 13th
    // December and the ladder's row for that tenor is the Monday after it.
    const auto spot_date = year{2026} / November / 13;
    const auto rule = rule_today(business_day_convention::following);

    CHECK(rule.expiry(spot_date, month_periods(1)) == year{2026} / December / 14);
}

TEST_CASE("the same rule with an unadjusted convention keeps the calendar date",
          "[fx_expiry_dates]") {
    // The pair's convention is a knob, not a second rule: a pair that settles
    // unadjusted keeps the Sunday, and no call site can date it another way.
    const auto spot_date = year{2026} / November / 13;
    const auto rule = rule_today(business_day_convention::unadjusted);

    CHECK(rule.expiry(spot_date, month_periods(1)) == year{2026} / December / 13);
}

TEST_CASE("a month-end spot snaps the expiry only when the pair's convention says it does",
          "[fx_expiry_dates]") {
    // 28 November 2014 is the last business day of its month, and three months
    // later the 28th falls on a Saturday: with the month-end rule the expiry is
    // the Friday before, and without it the Monday after.
    const auto spot_date = year{2014} / November / 28;
    const auto roll = business_day_convention::following;
    const auto snapped = fx_expiry_date_rule(
        roll, true, only_calendar, south_africa_calendar(year{2014}, year{2016}));
    const auto plain = fx_expiry_date_rule(
        roll, false, only_calendar, south_africa_calendar(year{2014}, year{2016}));

    CHECK(snapped.expiry(spot_date, month_periods(3)) == year{2015} / February / 27);
    CHECK(plain.expiry(spot_date, month_periods(3)) == year{2015} / March / 2);
}

TEST_CASE("a holiday on the expiry moves it to the next day the pair settles on",
          "[fx_expiry_dates]") {
    // 24 April 2014 plus one week is Workers' Day, a South African holiday, so
    // the expiry is the Friday after it.
    const auto rule = fx_expiry_date_rule(business_day_convention::following,
                                          false,
                                          only_calendar,
                                          south_africa_calendar(year{2014}, year{2016}));

    CHECK(rule.expiry(year{2014} / April / 24, week_periods(1)) == year{2014} / May / 2);
}

TEST_CASE("a business day count walks business days, not calendar days", "[fx_expiry_dates]") {
    // Three business days after Friday 19 December 2014 is Christmas Eve: the
    // weekend and the 16th are behind it, and the 25th is the next holiday.
    const auto rule = fx_expiry_date_rule(business_day_convention::following,
                                          false,
                                          only_calendar,
                                          south_africa_calendar(year{2014}, year{2016}));

    CHECK(rule.business_day(year{2014} / December / 19, 3) == year{2014} / December / 24);
}

TEST_CASE("a ladder starts from the pair's own business day", "[fx_expiry_dates]") {
    // Saturday 7 November 2026: a pair that rolls forward counts from the Monday, and one that
    // rolls back from the Friday. The horizon is the rule's answer, so neither is guessed here.
    const auto saturday = year{2026} / November / 7;
    const auto forward = rule_today(business_day_convention::following);
    const auto backward = rule_today(business_day_convention::preceding);

    CHECK(forward.horizon(saturday) == year{2026} / November / 9);
    CHECK(backward.horizon(saturday) == year{2026} / November / 6);
}

TEST_CASE("the spot date is the pair's spot days after its horizon", "[fx_expiry_dates]") {
    // Monday 9 November plus two business days settles on the Wednesday.
    const auto rule = rule_today(business_day_convention::following);

    CHECK(rule.spot_date(year{2026} / November / 9, 2) == year{2026} / November / 11);
    CHECK(rule.spot_date(year{2026} / November / 13, 2) == year{2026} / November / 17);
}

TEST_CASE("a tenor names its own length, not a calendar walk", "[fx_expiry_dates]") {
    // The surface's time axis: 1M is one twelfth of a year and 1W is seven 365ths, whatever the
    // calendar does to the date the cash moves.
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

TEST_CASE("a rule built without a calendar refuses to date anything", "[fx_expiry_dates]") {
    // The value a caller holds before it reads the pair's convention must be
    // recognisable as one, and it must not answer a date: an unread calendar is
    // not one that is closed only at weekends.
    const fx_expiry_date_rule unread;
    const auto spot_date = year{2026} / November / 13;

    CHECK(unread.calendar_count() == 0);
    CHECK_THROWS_AS(unread.expiry(spot_date, month_periods(1)), std::out_of_range);
}
