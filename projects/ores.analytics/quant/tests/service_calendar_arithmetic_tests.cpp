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
#include "ores.analytics.quant/service/quantlib_calendar_rulesets.hpp"
#include <catch2/catch_test_macros.hpp>
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
using ores::analytics::quant::service::quantlib_calendar_rulesets;

using namespace std::chrono;

const std::string tags("[calendar_arithmetic]");
constexpr std::size_t south_africa = 0;
constexpr std::size_t target = 0;

business_day_calendar_set build(const std::vector<calendar_ruleset>& rulesets, year from, year to) {
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

business_day_calendar_set make_south_africa() {
    return build({quantlib_calendar_rulesets::south_africa()}, year{2014}, year{2016});
}

business_day_calendar_set make_target() {
    return build({quantlib_calendar_rulesets::target()}, year{2025}, year{2027});
}

date_period d(int n) {
    return {period_unit::day, n};
}

date_period w(int n) {
    return {period_unit::week, n};
}

date_period m(int n) {
    return {period_unit::month, n};
}

date_period y(int n) {
    return {period_unit::year, n};
}

}

TEST_CASE("the SouthAfrica ruleset materialises the calendar QuantLib computes for 2015", tags) {
    // Transcribed from ql/time/calendars/southafrica.cpp, so the holiday list
    // is checked against the year on its own before any convention is driven
    // through it. A Saturday holiday -- 21 March, 26 December -- is a weekend
    // and so is not carried as a holiday at all; 9 August is a Sunday and
    // moves to the Monday.
    const std::vector<calendar_ruleset> rulesets = {quantlib_calendar_rulesets::south_africa()};
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        rulesets, year{2015} / January / 1, year{2015} / December / 31);

    std::vector<year_month_day> dates;
    for (const auto& holiday : holidays)
        dates.push_back(holiday.date);

    const std::vector<year_month_day> expected = {
        year{2015} / January / 1,    // New Year's Day
        year{2015} / April / 3,      // Good Friday
        year{2015} / April / 6,      // Family Day
        year{2015} / April / 27,     // Freedom Day
        year{2015} / May / 1,        // Workers' Day
        year{2015} / June / 16,      // Youth Day
        year{2015} / August / 10,    // National Women's Day, moved off Sunday
        year{2015} / September / 24, // Heritage Day
        year{2015} / December / 16,  // Day of Reconciliation
        year{2015} / December / 25,  // Christmas
    };
    CHECK(dates == expected);
}

TEST_CASE("QuantLib's own business day convention cases all resolve as it states", tags) {
    struct convention_case final {
        business_day_convention convention;
        year_month_day start;
        date_period period;
        bool end_of_month;
        year_month_day expected;
    };

    // Transcribed one for one from QuantLib's
    // test-suite/businessdayconventions.cpp, in its order, with its literal
    // expected dates. SouthAfrica is the calendar it drives them all through.
    const std::vector<convention_case> cases = {
        // Following
        {business_day_convention::following,
         year{2015} / February / 3,
         m(1),
         false,
         year{2015} / March / 3},
        {business_day_convention::following,
         year{2015} / February / 3,
         d(4),
         false,
         year{2015} / February / 9},
        {business_day_convention::following,
         year{2015} / January / 31,
         m(1),
         true,
         year{2015} / February / 27},
        {business_day_convention::following,
         year{2015} / January / 31,
         m(1),
         false,
         year{2015} / March / 2},
        // ModifiedFollowing
        {business_day_convention::modified_following,
         year{2015} / February / 3,
         m(1),
         false,
         year{2015} / March / 3},
        {business_day_convention::modified_following,
         year{2015} / February / 3,
         d(4),
         false,
         year{2015} / February / 9},
        {business_day_convention::modified_following,
         year{2015} / January / 31,
         m(1),
         true,
         year{2015} / February / 27},
        {business_day_convention::modified_following,
         year{2015} / January / 31,
         m(1),
         false,
         year{2015} / February / 27},
        {business_day_convention::modified_following,
         year{2015} / March / 25,
         m(1),
         false,
         year{2015} / April / 28},
        {business_day_convention::modified_following,
         year{2015} / February / 7,
         m(1),
         false,
         year{2015} / March / 9},
        // Preceding
        {business_day_convention::preceding,
         year{2015} / March / 3,
         m(-1),
         false,
         year{2015} / February / 3},
        {business_day_convention::preceding,
         year{2015} / February / 3,
         d(-2),
         false,
         year{2015} / January / 30},
        {business_day_convention::preceding,
         year{2015} / March / 1,
         m(-1),
         true,
         year{2015} / January / 30},
        {business_day_convention::preceding,
         year{2015} / March / 1,
         m(-1),
         false,
         year{2015} / January / 30},
        // ModifiedPreceding
        {business_day_convention::modified_preceding,
         year{2015} / March / 3,
         m(-1),
         false,
         year{2015} / February / 3},
        {business_day_convention::modified_preceding,
         year{2015} / February / 3,
         d(-2),
         false,
         year{2015} / January / 30},
        {business_day_convention::modified_preceding,
         year{2015} / March / 1,
         m(-1),
         true,
         year{2015} / February / 2},
        {business_day_convention::modified_preceding,
         year{2015} / March / 1,
         m(-1),
         false,
         year{2015} / February / 2},
        // Unadjusted
        {business_day_convention::unadjusted,
         year{2015} / February / 3,
         m(1),
         false,
         year{2015} / March / 3},
        {business_day_convention::unadjusted,
         year{2015} / February / 3,
         d(4),
         false,
         year{2015} / February / 9},
        {business_day_convention::unadjusted,
         year{2015} / January / 31,
         m(1),
         true,
         year{2015} / February / 27},
        {business_day_convention::unadjusted,
         year{2015} / January / 31,
         m(1),
         false,
         year{2015} / February / 28},
        // HalfMonthModifiedFollowing
        {business_day_convention::half_month_modified_following,
         year{2015} / February / 3,
         m(1),
         false,
         year{2015} / March / 3},
        {business_day_convention::half_month_modified_following,
         year{2015} / February / 3,
         d(4),
         false,
         year{2015} / February / 9},
        {business_day_convention::half_month_modified_following,
         year{2015} / January / 31,
         m(1),
         true,
         year{2015} / February / 27},
        {business_day_convention::half_month_modified_following,
         year{2015} / January / 31,
         m(1),
         false,
         year{2015} / February / 27},
        {business_day_convention::half_month_modified_following,
         year{2015} / January / 3,
         w(1),
         false,
         year{2015} / January / 12},
        {business_day_convention::half_month_modified_following,
         year{2015} / March / 21,
         w(1),
         false,
         year{2015} / March / 30},
        {business_day_convention::half_month_modified_following,
         year{2015} / February / 7,
         m(1),
         false,
         year{2015} / March / 9},
        // Nearest
        {business_day_convention::nearest,
         year{2015} / February / 3,
         m(1),
         false,
         year{2015} / March / 3},
        {business_day_convention::nearest,
         year{2015} / February / 3,
         d(4),
         false,
         year{2015} / February / 9},
        {business_day_convention::nearest,
         year{2015} / April / 16,
         m(1),
         false,
         year{2015} / May / 15},
        {business_day_convention::nearest,
         year{2015} / April / 17,
         m(1),
         false,
         year{2015} / May / 18},
        {business_day_convention::nearest,
         year{2015} / March / 4,
         m(1),
         false,
         year{2015} / April / 2},
        {business_day_convention::nearest,
         year{2015} / April / 2,
         m(1),
         false,
         year{2015} / May / 4},
    };

    const auto calendars = make_south_africa();
    REQUIRE(cases.size() == 35);

    for (std::size_t i = 0; i < cases.size(); ++i) {
        const auto& item = cases[i];
        const auto resolved = calendar_arithmetic::advance(
            item.start, item.period, item.convention, item.end_of_month, south_africa, calendars);
        CAPTURE(i);
        CHECK(resolved == item.expected);
    }
}

TEST_CASE("a period added as calendar time is not the same as one walked in business days", tags) {
    const auto calendars = make_south_africa();

    // Friday 6 February 2015. Four calendar days later is Tuesday the 10th;
    // four business days later is Thursday the 12th, because the weekend in
    // between does not count.
    const auto friday = year{2015} / February / 6;

    CHECK(calendar_arithmetic::add_period(friday, d(4)) == year{2015} / February / 10);
    CHECK(calendar_arithmetic::advance(
              friday, d(4), business_day_convention::following, false, south_africa, calendars) ==
          year{2015} / February / 12);
}

TEST_CASE("a day counted period lands on a business day whatever the convention says", tags) {
    const auto calendars = make_south_africa();

    // A day-counted walk skips holidays itself, so Unadjusted cannot put it
    // back onto one. Saturday 3 January 2015 plus one day is Monday the 5th.
    const auto saturday = year{2015} / January / 3;

    CHECK(
        calendar_arithmetic::advance(
            saturday, d(1), business_day_convention::unadjusted, false, south_africa, calendars) ==
        year{2015} / January / 5);
}

TEST_CASE("a month end that has no matching day clamps once, against the target month", tags) {
    // 31 January plus one month has no 31 February, so it clamps to the 28th;
    // plus two months there is a 31 March, so it does not. Clamping is against
    // the total months, not applied a month at a time.
    CHECK(calendar_arithmetic::add_period(year{2026} / January / 31, m(1)) ==
          year{2026} / February / 28);
    CHECK(calendar_arithmetic::add_period(year{2026} / January / 31, m(2)) ==
          year{2026} / March / 31);
}

TEST_CASE("a clamped month end is sticky on the next addition", tags) {
    // February has no 31st, so the 28th is where a January month end lands;
    // adding another month counts from there, not from the 31st it came from.
    const auto clamped = calendar_arithmetic::add_period(year{2026} / January / 31, m(1));

    CHECK(calendar_arithmetic::add_period(clamped, m(1)) == year{2026} / March / 28);
}

TEST_CASE("a year added to a leap day clamps to the twenty eighth", tags) {
    CHECK(calendar_arithmetic::add_period(year{2024} / February / 29, y(1)) ==
          year{2025} / February / 28);
}

TEST_CASE("a negative month period clamps the same way a positive one does", tags) {
    CHECK(calendar_arithmetic::add_period(year{2026} / March / 31, m(-1)) ==
          year{2026} / February / 28);
}

TEST_CASE("a period with no unit has no length and cannot be added", tags) {
    CHECK_THROWS_AS(calendar_arithmetic::add_period(year{2026} / January / 31, date_period{}),
                    std::invalid_argument);
}

TEST_CASE("end of month means at or after the last business day, not is the last one", tags) {
    const auto calendars = make_target();

    // 31 January 2026 is a Saturday, so no business day is left in the month
    // after it -- and the day itself is not one. Both facts are true of the
    // 31st, and QuantLib's predicate says true for both it and the Friday
    // before it, which is what makes it usable as a snap trigger.
    CHECK(calendar_arithmetic::is_end_of_month(year{2026} / January / 30, target, calendars));
    CHECK(calendar_arithmetic::is_end_of_month(year{2026} / January / 31, target, calendars));
    CHECK_FALSE(calendar_arithmetic::is_end_of_month(year{2026} / January / 29, target, calendars));
}

TEST_CASE("the last business day of a month stays inside the month it was asked about", tags) {
    const auto calendars = make_target();

    for (unsigned month_index = 1; month_index <= 12; ++month_index) {
        const auto any_day = year{2026} / std::chrono::month{month_index} / 15;
        const auto last = calendar_arithmetic::last_business_day(
            any_day, business_day_convention::preceding, target, calendars);

        CAPTURE(month_index);
        CHECK(last.month() == any_day.month());
        CHECK(last.year() == any_day.year());
        CHECK(calendars.is_business_day(target, last));
        CHECK(calendar_arithmetic::is_end_of_month(last, target, calendars));
    }
}

TEST_CASE("end of month turns a month counted period into the month's last business day", tags) {
    const auto calendars = make_target();

    // 31 January 2026 is the last business day of its month, so a one-month
    // period snapped to the end of February is the 27th: the 28th is a
    // Saturday and the snap carries its own Preceding convention, which the
    // rule's own convention does not override.
    CHECK(calendar_arithmetic::advance(year{2026} / January / 31,
                                       m(1),
                                       business_day_convention::following,
                                       true,
                                       target,
                                       calendars) == year{2026} / February / 27);
}

TEST_CASE("the nth weekday of a month counts from one", tags) {
    // March 2015 begins on a Sunday, so its Wednesdays are the 4th, 11th and
    // 18th.
    CHECK(calendar_arithmetic::nth_weekday(3, Wednesday, March, year{2015}) ==
          year{2015} / March / 18);
    CHECK(calendar_arithmetic::nth_weekday(1, Wednesday, March, year{2015}) ==
          year{2015} / March / 4);
    CHECK(calendar_arithmetic::nth_weekday(1, Sunday, March, year{2015}) == year{2015} / March / 1);
}

TEST_CASE("an nth weekday outside one to five is refused", tags) {
    CHECK_THROWS_AS(calendar_arithmetic::nth_weekday(0, Wednesday, March, year{2015}),
                    std::invalid_argument);
    CHECK_THROWS_AS(calendar_arithmetic::nth_weekday(6, Wednesday, March, year{2015}),
                    std::invalid_argument);
}

TEST_CASE("an IMM date is the third Wednesday of a quarter month", tags) {
    CHECK(calendar_arithmetic::imm_date(March, year{2015}) == year{2015} / March / 18);
    CHECK(calendar_arithmetic::imm_date(June, year{2020}) == year{2020} / June / 17);
    CHECK(calendar_arithmetic::imm_date(March, year{2026}) == year{2026} / March / 18);

    CHECK(calendar_arithmetic::is_imm_date(year{2015} / March / 18));
    // A Wednesday, but not in the 15th-to-21st window the third Wednesday
    // always falls in.
    CHECK_FALSE(calendar_arithmetic::is_imm_date(year{2015} / March / 11));
    // A third Wednesday, but not in a quarter month, so it is a serial
    // contract month and not a main-cycle one.
    CHECK(calendar_arithmetic::is_imm_date(year{2015} / January / 21, false));
    CHECK_FALSE(calendar_arithmetic::is_imm_date(year{2015} / January / 21, true));
}

TEST_CASE("the next IMM date is strictly after the one asked about", tags) {
    // QuantLib's own invariant, over a decade either side of the contract
    // dates this is used for.
    for (auto day = sys_days{year{2015} / January / 1}; day <= sys_days{year{2025} / December / 31};
         day += days{1}) {
        const year_month_day counter{day};
        const auto serial = calendar_arithmetic::next_imm_date(counter, false);
        const auto main = calendar_arithmetic::next_imm_date(counter, true);

        CAPTURE(counter);
        CHECK(serial > counter);
        CHECK(calendar_arithmetic::is_imm_date(serial, false));
        CHECK(main > counter);
        CHECK(calendar_arithmetic::is_imm_date(main, true));
        // The main cycle is a subset of the serial one, so its next date can
        // never come first.
        CHECK(serial <= main);
    }
}

TEST_CASE("the main IMM cycle steps a quarter at a time", tags) {
    auto imm = calendar_arithmetic::imm_date(March, year{2020});
    CHECK(imm == year{2020} / March / 18);

    imm = calendar_arithmetic::next_imm_date(imm, true);
    CHECK(imm == year{2020} / June / 17);
    imm = calendar_arithmetic::next_imm_date(imm, true);
    CHECK(imm == year{2020} / September / 16);
    imm = calendar_arithmetic::next_imm_date(imm, true);
    CHECK(imm == year{2020} / December / 16);
    imm = calendar_arithmetic::next_imm_date(imm, true);
    CHECK(imm == year{2021} / March / 17);
}

TEST_CASE("a calendar the set was not built with is refused rather than assumed open", tags) {
    const auto calendars = make_target();

    CHECK_THROWS_AS(
        calendar_arithmetic::adjust(
            year{2026} / January / 15, business_day_convention::following, 7, calendars),
        std::out_of_range);
    CHECK_THROWS_AS(calendar_arithmetic::add_period(year{2026} / January / 15, date_period{}),
                    std::invalid_argument);
}

TEST_CASE("rolling an already business day leaves it where it is", tags) {
    const auto calendars = make_target();

    // Thursday 15 January 2026 is a TARGET business day under every
    // convention that does not insist on moving.
    const auto business_day = year{2026} / January / 15;
    for (const auto convention : {business_day_convention::following,
                                  business_day_convention::modified_following,
                                  business_day_convention::preceding,
                                  business_day_convention::modified_preceding,
                                  business_day_convention::nearest,
                                  business_day_convention::half_month_modified_following,
                                  business_day_convention::unadjusted}) {
        CAPTURE(static_cast<int>(convention));
        CHECK(calendar_arithmetic::adjust(business_day, convention, target, calendars) ==
              business_day);
    }
}

TEST_CASE("an ASX date is the second Friday of a quarter month", tags) {
    CHECK(calendar_arithmetic::asx_date(March, year{2015}) == year{2015} / March / 13);
    CHECK(calendar_arithmetic::asx_date(June, year{2020}) == year{2020} / June / 12);

    CHECK(calendar_arithmetic::is_asx_date(year{2015} / March / 13));
    // A Friday, but before the eighth, so not a second Friday.
    CHECK_FALSE(calendar_arithmetic::is_asx_date(year{2015} / March / 6));
    // The third Friday, which is past the window a second Friday falls in.
    CHECK_FALSE(calendar_arithmetic::is_asx_date(year{2015} / March / 20));
    // A second Friday, but not in a quarter month.
    CHECK(calendar_arithmetic::is_asx_date(year{2020} / January / 10, false));
    CHECK_FALSE(calendar_arithmetic::is_asx_date(year{2020} / January / 10, true));
}

TEST_CASE("the next ASX date is strictly after the one asked about", tags) {
    // QuantLib's own invariant, over the same decade it states it for.
    for (auto day = sys_days{year{2015} / January / 1}; day <= sys_days{year{2025} / December / 31};
         day += days{1}) {
        const year_month_day counter{day};
        const auto serial = calendar_arithmetic::next_asx_date(counter, false);
        const auto main = calendar_arithmetic::next_asx_date(counter, true);

        CAPTURE(counter);
        CHECK(serial > counter);
        CHECK(calendar_arithmetic::is_asx_date(serial, false));
        CHECK(main > counter);
        CHECK(calendar_arithmetic::is_asx_date(main, true));
        CHECK(serial <= main);
    }
}

TEST_CASE("the main ASX cycle steps a quarter at a time", tags) {
    auto asx = calendar_arithmetic::asx_date(March, year{2020});
    CHECK(asx == year{2020} / March / 13);

    asx = calendar_arithmetic::next_asx_date(asx, true);
    CHECK(asx == year{2020} / June / 12);
    asx = calendar_arithmetic::next_asx_date(asx, true);
    CHECK(asx == year{2020} / September / 11);
    asx = calendar_arithmetic::next_asx_date(asx, true);
    CHECK(asx == year{2020} / December / 11);
    asx = calendar_arithmetic::next_asx_date(asx, true);
    CHECK(asx == year{2021} / March / 12);
}

TEST_CASE("an ASX date and an IMM date are different dates on the same cycle", tags) {
    // The two exchanges standardise on different weekdays, and a reader who
    // treats them as one convention gets the other's contracts.
    for (unsigned month_index : {3u, 6u, 9u, 12u}) {
        const auto m = std::chrono::month{month_index};
        CAPTURE(month_index);
        CHECK(calendar_arithmetic::asx_date(m, year{2020}) !=
              calendar_arithmetic::imm_date(m, year{2020}));
    }
}
