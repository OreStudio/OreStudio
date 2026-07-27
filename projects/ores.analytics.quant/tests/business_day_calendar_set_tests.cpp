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
#include <catch2/catch_test_macros.hpp>
#include <vector>

using namespace std::chrono;
using ores::analytics::quant::domain::business_day_calendar_set;
using ores::analytics::quant::domain::calendar_date_row;
using ores::analytics::quant::domain::calendar_query;

TEST_CASE("a date matching a stored holiday row is not a business day",
          "[business_day_calendar_set]") {
    const std::vector<calendar_date_row> rows = {
        {0, year{2024} / January / 1},
        {0, year{2024} / December / 25},
    };
    const auto set = business_day_calendar_set::from_rows(rows, 1, {std::bitset<7>{0b1000001}});

    const std::vector<calendar_query> queries = {
        {0, year{2024} / January / 1},
        {0, year{2024} / December / 25},
        {0, year{2024} / March / 1}, // a Friday, not a holiday
    };
    const auto result = set.is_business_day_batch(queries);

    REQUIRE(result.size() == 3);
    CHECK_FALSE(result[0]);
    CHECK_FALSE(result[1]);
    CHECK(result[2]);
}

TEST_CASE("a weekend date is never a business day even without a matching holiday row",
          "[business_day_calendar_set]") {
    const std::vector<calendar_date_row> rows = {};
    const auto set = business_day_calendar_set::from_rows(rows, 1, {std::bitset<7>{0b1000001}});

    // 2024-03-02 was a Saturday, 2024-03-03 a Sunday, 2024-03-04 a Monday.
    const std::vector<calendar_query> queries = {
        {0, year{2024} / March / 2},
        {0, year{2024} / March / 3},
        {0, year{2024} / March / 4},
    };
    const auto result = set.is_business_day_batch(queries);

    REQUIRE(result.size() == 3);
    CHECK_FALSE(result[0]);
    CHECK_FALSE(result[1]);
    CHECK(result[2]);
}

TEST_CASE("each calendar's holiday segment is independent of the others",
          "[business_day_calendar_set]") {
    const std::vector<calendar_date_row> rows = {
        {0, year{2024} / January / 1},
        {1, year{2024} / July / 4},
    };
    const auto set = business_day_calendar_set::from_rows(
        rows, 2, {std::bitset<7>{0b1000001}, std::bitset<7>{0b1000001}});

    CHECK(set.calendar_count() == 2);
    CHECK(set.holiday_count(0) == 1);
    CHECK(set.holiday_count(1) == 1);

    const std::vector<calendar_query> queries = {
        {0, year{2024} / July / 4}, // holiday for calendar 1, not calendar 0
        {1, year{2024} / July / 4},
        {0, year{2024} / January / 1},
        {1, year{2024} / January / 1},
    };
    const auto result = set.is_business_day_batch(queries);

    REQUIRE(result.size() == 4);
    CHECK(result[0]);       // calendar 0 has no holiday on July 4th
    CHECK_FALSE(result[1]); // calendar 1 does
    CHECK_FALSE(result[2]); // calendar 0 has a holiday on January 1st
    CHECK(result[3]);       // calendar 1 does not
}

TEST_CASE("a custom weekend_mask per calendar is honoured independently",
          "[business_day_calendar_set]") {
    const std::vector<calendar_date_row> rows = {};
    // Calendar 0: default Sat/Sun weekend. Calendar 1: Fri/Sat weekend.
    const auto set = business_day_calendar_set::from_rows(
        rows, 2, {std::bitset<7>{0b1000001}, std::bitset<7>{0b0100001}});

    // 2024-03-01 was a Friday.
    const std::vector<calendar_query> queries = {
        {0, year{2024} / March / 1},
        {1, year{2024} / March / 1},
    };
    const auto result = set.is_business_day_batch(queries);

    REQUIRE(result.size() == 2);
    CHECK(result[0]);       // ordinary business day under Sat/Sun weekend
    CHECK_FALSE(result[1]); // a weekend day under Fri/Sat weekend
}

TEST_CASE("empty rows and zero calendars produce an empty, well-formed set",
          "[business_day_calendar_set]") {
    const std::vector<calendar_date_row> rows = {};
    const auto set = business_day_calendar_set::from_rows(rows, 0, {});

    CHECK(set.calendar_count() == 0);
}
