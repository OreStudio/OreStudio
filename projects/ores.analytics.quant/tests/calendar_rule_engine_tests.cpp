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
#include "ores.analytics.quant/domain/calendar_ruleset.hpp"
#include "ores.analytics.quant/service/calendar_rule_engine.hpp"
#include <catch2/catch_test_macros.hpp>
#include <vector>

using namespace std::chrono;
using ores::analytics::quant::domain::calendar_exception;
using ores::analytics::quant::domain::calendar_rule;
using ores::analytics::quant::domain::calendar_rule_kind;
using ores::analytics::quant::domain::calendar_ruleset;
using ores::analytics::quant::domain::instantiated_holiday;
using ores::analytics::quant::domain::observance_shift;
using ores::analytics::quant::service::calendar_rule_engine;

namespace {

// TARGET (the Eurosystem's calendar), transcribed rule-for-rule from
// QuantLib's ql/time/calendars/target.cpp -- the reference this test
// replicates against QuantLib's own testTARGET golden dataset
// (test-suite/calendars.cpp).
calendar_ruleset target_ruleset() {
    calendar_ruleset cal;
    cal.rules = {
        // New Year's Day
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = January, .day = 1u},
        // Good Friday (Easter Monday - 3 days = Easter Sunday - 2 days), from 2000
        calendar_rule{.kind = calendar_rule_kind::easter_offset, .day_offset = -2,
                      .effective_from = year{2000}},
        // Easter Monday (Easter Sunday + 1 day), from 2000
        calendar_rule{.kind = calendar_rule_kind::easter_offset, .day_offset = 1,
                      .effective_from = year{2000}},
        // Labour Day, from 2000
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = May, .day = 1u,
                      .effective_from = year{2000}},
        // Christmas
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 25u},
        // Day of Goodwill, from 2000
        calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 26u,
                      .effective_from = year{2000}},
    };
    cal.exceptions = {
        // December 31st, 1998, 1999, and 2001 only
        calendar_exception{.date = year{1998} / December / 31, .is_business_day = false},
        calendar_exception{.date = year{1999} / December / 31, .is_business_day = false},
        calendar_exception{.date = year{2001} / December / 31, .is_business_day = false},
    };
    return cal;
}

std::vector<year_month_day> dates_for(std::size_t calendar_index,
                                      const std::vector<instantiated_holiday>& all) {
    std::vector<year_month_day> result;
    for (const auto& h : all)
        if (h.calendar_index == calendar_index)
            result.push_back(h.date);
    return result;
}

} // namespace

TEST_CASE("easter_sunday matches known reference dates", "[calendar_rule_engine]") {
    // Cross-checked against QuantLib's Date::easterMonday table (Easter
    // Monday - 1 day) and independently-known Easter Sunday dates.
    CHECK(calendar_rule_engine::easter_sunday(year{2000}) == year{2000} / April / 23);
    CHECK(calendar_rule_engine::easter_sunday(year{2001}) == year{2001} / April / 15);
    CHECK(calendar_rule_engine::easter_sunday(year{2004}) == year{2004} / April / 11);
    CHECK(calendar_rule_engine::easter_sunday(year{2005}) == year{2005} / March / 27);
    CHECK(calendar_rule_engine::easter_sunday(year{2024}) == year{2024} / March / 31);
}

TEST_CASE("TARGET holiday list 1999-2006 matches QuantLib's testTARGET dataset",
         "[calendar_rule_engine][target]") {
    const std::vector<calendar_ruleset> calendars = {target_ruleset()};
    const auto start = year{1999} / January / 1;
    const auto end = year{2006} / December / 31;

    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(calendars, start, end);
    const auto actual = dates_for(0, holidays);

    const std::vector<year_month_day> expected = {
        year{1999} / January / 1,
        year{1999} / December / 31,

        year{2000} / April / 21,
        year{2000} / April / 24,
        year{2000} / May / 1,
        year{2000} / December / 25,
        year{2000} / December / 26,

        year{2001} / January / 1,
        year{2001} / April / 13,
        year{2001} / April / 16,
        year{2001} / May / 1,
        year{2001} / December / 25,
        year{2001} / December / 26,
        year{2001} / December / 31,

        year{2002} / January / 1,
        year{2002} / March / 29,
        year{2002} / April / 1,
        year{2002} / May / 1,
        year{2002} / December / 25,
        year{2002} / December / 26,

        year{2003} / January / 1,
        year{2003} / April / 18,
        year{2003} / April / 21,
        year{2003} / May / 1,
        year{2003} / December / 25,
        year{2003} / December / 26,

        year{2004} / January / 1,
        year{2004} / April / 9,
        year{2004} / April / 12,

        year{2005} / March / 25,
        year{2005} / March / 28,
        year{2005} / December / 26,

        year{2006} / April / 14,
        year{2006} / April / 17,
        year{2006} / May / 1,
        year{2006} / December / 25,
        year{2006} / December / 26,
    };

    REQUIRE(actual.size() == expected.size());
    CHECK(actual == expected);
}

TEST_CASE("a rule with no effective_from/effective_to fires in every year",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 25u}};

    // 2018-12-25 (Tue), 2019-12-25 (Wed), 2020-12-25 (Fri) are all
    // weekdays -- unlike 2021/2022, which fall on the weekend.
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2018} / January / 1, year{2020} / December / 31);

    const auto actual = dates_for(0, holidays);
    REQUIRE(actual.size() == 3);
    CHECK(actual[0] == year{2018} / December / 25);
    CHECK(actual[1] == year{2019} / December / 25);
    CHECK(actual[2] == year{2020} / December / 25);
}

TEST_CASE("effective_from/effective_to gate a rule to a year range", "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date,
                               .month = May,
                               .day = 1u,
                               .effective_from = year{2001},
                               .effective_to = year{2001}}};

    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2000} / January / 1, year{2002} / December / 31);

    const auto actual = dates_for(0, holidays);
    REQUIRE(actual.size() == 1);
    CHECK(actual[0] == year{2001} / May / 1);
}

TEST_CASE("nth_weekday_of_month resolves the 4th Thursday of November (US Thanksgiving)",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::nth_weekday_of_month,
                               .month = November,
                               .weekday = Thursday,
                               .occurrence = 4u}};

    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2023} / January / 1, year{2025} / December / 31);

    const auto actual = dates_for(0, holidays);
    REQUIRE(actual.size() == 3);
    CHECK(actual[0] == year{2023} / November / 23);
    CHECK(actual[1] == year{2024} / November / 28);
    CHECK(actual[2] == year{2025} / November / 27);
}

TEST_CASE("last_weekday_of_month resolves the last Monday of August (UK Summer Bank Holiday)",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::last_weekday_of_month,
                               .month = August,
                               .weekday = Monday}};

    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2004} / January / 1, year{2006} / December / 31);

    const auto actual = dates_for(0, holidays);
    REQUIRE(actual.size() == 3);
    CHECK(actual[0] == year{2004} / August / 30);
    CHECK(actual[1] == year{2005} / August / 29);
    CHECK(actual[2] == year{2006} / August / 28);
}

TEST_CASE("nearest_weekday shift rolls Saturday back and Sunday forward",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date,
                               .month = July,
                               .day = 4u,
                               .shift = observance_shift::nearest_weekday}};

    // July 4th 2020 was a Saturday -> observed Friday July 3rd.
    // July 4th 2021 was a Sunday -> observed Monday July 5th.
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2020} / January / 1, year{2021} / December / 31);

    const auto actual = dates_for(0, holidays);
    REQUIRE(actual.size() == 2);
    CHECK(actual[0] == year{2020} / July / 3);
    CHECK(actual[1] == year{2021} / July / 5);
}

TEST_CASE("roll_forward_to_monday shift moves both Saturday and Sunday to the following Monday",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date,
                               .month = January,
                               .day = 1u,
                               .shift = observance_shift::roll_forward_to_monday}};

    // January 1st 2005 was a Saturday -> observed Monday January 3rd.
    // January 1st 2006 was a Sunday -> observed Monday January 2nd.
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2005} / January / 1, year{2006} / December / 31);

    const auto actual = dates_for(0, holidays);
    REQUIRE(actual.size() == 2);
    CHECK(actual[0] == year{2005} / January / 3);
    CHECK(actual[1] == year{2006} / January / 2);
}

TEST_CASE("a calendar_exception adds an additional holiday not covered by any rule",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.exceptions = {calendar_exception{.date = year{2012} / June / 5, .is_business_day = false}};

    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2012} / January / 1, year{2012} / December / 31);

    const auto actual = dates_for(0, holidays);
    REQUIRE(actual.size() == 1);
    CHECK(actual[0] == year{2012} / June / 5);
}

TEST_CASE("a calendar_exception can override a rule-generated holiday back to a business day",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 25u}};
    cal.exceptions = {calendar_exception{.date = year{2020} / December / 25, .is_business_day = true}};

    // 2019-12-25 (Wed) and 2020-12-25 (Fri) are weekdays; 2021-12-25 is a
    // Saturday and already excluded by the weekend check regardless of the
    // exception. The exception removes the 2020 occurrence, leaving only
    // 2019.
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2019} / January / 1, year{2021} / December / 31);

    const auto actual = dates_for(0, holidays);
    REQUIRE(actual.size() == 1);
    CHECK(actual[0] == year{2019} / December / 25);
}

TEST_CASE("weekend days are never included as holidays even when a rule would otherwise match",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    // Christmas 2021 fell on a Saturday.
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 25u}};

    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2021} / January / 1, year{2021} / December / 31);

    CHECK(dates_for(0, holidays).empty());
}

TEST_CASE("a custom weekend_mask (e.g. Friday/Saturday) is honoured instead of the default",
         "[calendar_rule_engine]") {
    calendar_ruleset cal;
    cal.weekend_mask = std::bitset<7>{0b0100001}; // Sunday(bit0) + Friday(bit5)
    cal.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = December, .day = 25u}};

    // December 25th 2020 was a Friday -- a holiday under the default
    // Sat/Sun weekend, but a weekend day itself under this custom mask.
    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal}, year{2020} / January / 1, year{2020} / December / 31);

    CHECK(dates_for(0, holidays).empty());
}

TEST_CASE("multiple calendars are instantiated independently in one batch call",
         "[calendar_rule_engine]") {
    calendar_ruleset cal_a;
    cal_a.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = January, .day = 1u}};
    calendar_ruleset cal_b;
    cal_b.rules = {calendar_rule{.kind = calendar_rule_kind::fixed_date, .month = March, .day = 1u}};

    const auto holidays = calendar_rule_engine::instantiate_holidays_batch(
        std::vector<calendar_ruleset>{cal_a, cal_b}, year{2021} / January / 1, year{2021} / December / 31);

    REQUIRE(dates_for(0, holidays).size() == 1);
    CHECK(dates_for(0, holidays)[0] == year{2021} / January / 1);
    REQUIRE(dates_for(1, holidays).size() == 1);
    CHECK(dates_for(1, holidays)[0] == year{2021} / March / 1); // a Monday
}
