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
#include "ores.marketdata.api/domain/tenor.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[tenor]");

}

TEST_CASE("tenor_parse_recognises_short_end_labels", tags) {
    using ores::marketdata::domain::tenor;
    using ores::marketdata::domain::tenor_unit;

    auto on = tenor::parse("O/N");
    CHECK(on.unit() == tenor_unit::days);
    CHECK(on.count() == 1);

    auto tn = tenor::parse("T/N");
    CHECK(tn.unit() == tenor_unit::days);
    CHECK(tn.count() == 2);

    auto sn = tenor::parse("S/N");
    CHECK(sn.unit() == tenor_unit::days);

    auto sw = tenor::parse("S/W");
    CHECK(sw.unit() == tenor_unit::days);
}

TEST_CASE("tenor_parse_recognises_period_labels", tags) {
    using ores::marketdata::domain::tenor;
    using ores::marketdata::domain::tenor_unit;

    auto oneWeek = tenor::parse("1W");
    CHECK(oneWeek.unit() == tenor_unit::weeks);
    CHECK(oneWeek.count() == 1);

    auto threeMonths = tenor::parse("3M");
    CHECK(threeMonths.unit() == tenor_unit::months);
    CHECK(threeMonths.count() == 3);

    auto twoYears = tenor::parse("2Y");
    CHECK(twoYears.unit() == tenor_unit::years);
    CHECK(twoYears.count() == 2);

    auto tenDays = tenor::parse("10D");
    CHECK(tenDays.unit() == tenor_unit::days);
    CHECK(tenDays.count() == 10);
}

TEST_CASE("tenor_parse_rejects_malformed_labels", tags) {
    using ores::marketdata::domain::tenor;

    CHECK_THROWS_AS(tenor::parse(""), std::invalid_argument);
    CHECK_THROWS_AS(tenor::parse("X"), std::invalid_argument);
    CHECK_THROWS_AS(tenor::parse("3Q"), std::invalid_argument);
    CHECK_THROWS_AS(tenor::parse("M3"), std::invalid_argument);
    CHECK_THROWS_AS(tenor::parse("-1Y"), std::invalid_argument);
    CHECK_THROWS_AS(tenor::parse("0M"), std::invalid_argument);
}

TEST_CASE("tenor_orders_by_approximate_length", tags) {
    using ores::marketdata::domain::tenor;

    CHECK(tenor::parse("O/N") < tenor::parse("1W"));
    CHECK(tenor::parse("1W") < tenor::parse("1M"));
    CHECK(tenor::parse("1M") < tenor::parse("1Y"));
    CHECK(tenor::parse("3M") < tenor::parse("6M"));
    CHECK(tenor::parse("12M") == tenor::parse("12M"));
}

TEST_CASE("tenor_end_date_adds_days_and_weeks", tags) {
    using namespace std::chrono;
    using ores::marketdata::domain::tenor;

    const year_month_day horizon{year(2026), month(1), day(15)};

    auto oneWeekEnd = tenor::parse("1W").end_date(horizon);
    CHECK(oneWeekEnd == year_month_day{year(2026), month(1), day(22)});

    auto tenDaysEnd = tenor::parse("10D").end_date(horizon);
    CHECK(tenDaysEnd == year_month_day{year(2026), month(1), day(25)});
}

TEST_CASE("tenor_end_date_adds_months_and_years", tags) {
    using namespace std::chrono;
    using ores::marketdata::domain::tenor;

    const year_month_day horizon{year(2026), month(1), day(15)};

    auto threeMonthsEnd = tenor::parse("3M").end_date(horizon);
    CHECK(threeMonthsEnd == year_month_day{year(2026), month(4), day(15)});

    auto oneYearEnd = tenor::parse("1Y").end_date(horizon);
    CHECK(oneYearEnd == year_month_day{year(2027), month(1), day(15)});
}

TEST_CASE("tenor_end_date_clamps_to_last_valid_day_of_month", tags) {
    using namespace std::chrono;
    using ores::marketdata::domain::tenor;

    // 31 Jan + 1M has no 31 Feb: must clamp to the last day of February.
    const year_month_day horizon{year(2026), month(1), day(31)};

    auto oneMonthEnd = tenor::parse("1M").end_date(horizon);
    CHECK(oneMonthEnd == year_month_day{year(2026), month(2), day(28)});
}

TEST_CASE("windows_overlap_detects_overlapping_windows", tags) {
    using namespace std::chrono;
    using ores::marketdata::domain::resolve_window;
    using ores::marketdata::domain::tenor;

    const year_month_day horizon{year(2026), month(1), day(1)};

    // A 3M deposit window and a 2M-3M FRA-like window overlap.
    auto deposit = resolve_window(horizon, tenor::parse("3M"));
    auto overlapping = resolve_window(horizon + months(2), tenor::parse("3M"));

    CHECK(ores::marketdata::domain::windows_overlap(deposit, overlapping));
}

TEST_CASE("windows_overlap_returns_false_for_disjoint_windows", tags) {
    using namespace std::chrono;
    using ores::marketdata::domain::resolve_window;
    using ores::marketdata::domain::tenor;

    const year_month_day horizon{year(2026), month(1), day(1)};

    auto first = resolve_window(horizon, tenor::parse("1M"));
    auto secondStart = horizon + months(2);
    auto second = resolve_window(secondStart, tenor::parse("1M"));

    CHECK_FALSE(ores::marketdata::domain::windows_overlap(first, second));
}

TEST_CASE("windows_overlap_treats_windows_as_half_open", tags) {
    using namespace std::chrono;
    using ores::marketdata::domain::resolve_window;
    using ores::marketdata::domain::tenor;

    const year_month_day horizon{year(2026), month(1), day(1)};

    // Second window starts exactly where the first ends: half-open windows
    // must not count that as an overlap.
    auto first = resolve_window(horizon, tenor::parse("1M"));
    auto second = resolve_window(first.end, tenor::parse("1M"));

    CHECK_FALSE(ores::marketdata::domain::windows_overlap(first, second));
}
