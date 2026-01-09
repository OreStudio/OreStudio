/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.platform/time/relative_time_formatter.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.platform.tests");
const std::string tags("[time][relative_time_formatter]");

using namespace std::chrono;
using namespace std::chrono_literals;
using ores::platform::time::relative_time_formatter;
using ores::platform::time::numeric_style;
using ores::platform::time::time_unit;

// Helper to create a time point at a specific offset from reference
system_clock::time_point offset_from(system_clock::time_point ref, seconds offset) {
    return ref + offset;
}

}

using namespace ores::logging;

// =============================================================================
// Tests for format(value, unit) with automatic style
// =============================================================================

TEST_CASE("format_value_unit_automatic_zero_values", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    BOOST_LOG_SEV(lg, info) << "Testing automatic style with zero values";

    CHECK(fmt.format(0, time_unit::second) == "now");
    CHECK(fmt.format(0, time_unit::minute) == "now");
    CHECK(fmt.format(0, time_unit::hour) == "this hour");
    CHECK(fmt.format(0, time_unit::day) == "today");
    CHECK(fmt.format(0, time_unit::week) == "this week");
    CHECK(fmt.format(0, time_unit::month) == "this month");
    CHECK(fmt.format(0, time_unit::quarter) == "this quarter");
    CHECK(fmt.format(0, time_unit::year) == "this year");
}

TEST_CASE("format_value_unit_automatic_past_singular", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    BOOST_LOG_SEV(lg, info) << "Testing automatic style with -1 values (past)";

    CHECK(fmt.format(-1, time_unit::second) == "1 second ago");
    CHECK(fmt.format(-1, time_unit::minute) == "1 minute ago");
    CHECK(fmt.format(-1, time_unit::hour) == "1 hour ago");
    CHECK(fmt.format(-1, time_unit::day) == "yesterday");
    CHECK(fmt.format(-1, time_unit::week) == "last week");
    CHECK(fmt.format(-1, time_unit::month) == "last month");
    CHECK(fmt.format(-1, time_unit::quarter) == "last quarter");
    CHECK(fmt.format(-1, time_unit::year) == "last year");
}

TEST_CASE("format_value_unit_automatic_future_singular", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    BOOST_LOG_SEV(lg, info) << "Testing automatic style with +1 values (future)";

    CHECK(fmt.format(1, time_unit::second) == "in 1 second");
    CHECK(fmt.format(1, time_unit::minute) == "in 1 minute");
    CHECK(fmt.format(1, time_unit::hour) == "in 1 hour");
    CHECK(fmt.format(1, time_unit::day) == "tomorrow");
    CHECK(fmt.format(1, time_unit::week) == "next week");
    CHECK(fmt.format(1, time_unit::month) == "next month");
    CHECK(fmt.format(1, time_unit::quarter) == "next quarter");
    CHECK(fmt.format(1, time_unit::year) == "next year");
}

TEST_CASE("format_value_unit_automatic_past_plural", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    BOOST_LOG_SEV(lg, info) << "Testing automatic style with negative plural values";

    CHECK(fmt.format(-5, time_unit::second) == "5 seconds ago");
    CHECK(fmt.format(-10, time_unit::minute) == "10 minutes ago");
    CHECK(fmt.format(-3, time_unit::hour) == "3 hours ago");
    CHECK(fmt.format(-2, time_unit::day) == "2 days ago");
    CHECK(fmt.format(-4, time_unit::week) == "4 weeks ago");
    CHECK(fmt.format(-6, time_unit::month) == "6 months ago");
    CHECK(fmt.format(-2, time_unit::quarter) == "2 quarters ago");
    CHECK(fmt.format(-3, time_unit::year) == "3 years ago");
}

TEST_CASE("format_value_unit_automatic_future_plural", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    BOOST_LOG_SEV(lg, info) << "Testing automatic style with positive plural values";

    CHECK(fmt.format(5, time_unit::second) == "in 5 seconds");
    CHECK(fmt.format(10, time_unit::minute) == "in 10 minutes");
    CHECK(fmt.format(3, time_unit::hour) == "in 3 hours");
    CHECK(fmt.format(2, time_unit::day) == "in 2 days");
    CHECK(fmt.format(4, time_unit::week) == "in 4 weeks");
    CHECK(fmt.format(6, time_unit::month) == "in 6 months");
    CHECK(fmt.format(2, time_unit::quarter) == "in 2 quarters");
    CHECK(fmt.format(3, time_unit::year) == "in 3 years");
}

// =============================================================================
// Tests for format(value, unit) with always style
// =============================================================================

TEST_CASE("format_value_unit_always_zero_values", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::always);

    BOOST_LOG_SEV(lg, info) << "Testing always style with zero values";

    CHECK(fmt.format(0, time_unit::second) == "now");
    CHECK(fmt.format(0, time_unit::minute) == "now");
    CHECK(fmt.format(0, time_unit::day) == "now");
}

TEST_CASE("format_value_unit_always_past_singular", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::always);

    BOOST_LOG_SEV(lg, info) << "Testing always style with -1 values (past)";

    CHECK(fmt.format(-1, time_unit::second) == "1 second ago");
    CHECK(fmt.format(-1, time_unit::minute) == "1 minute ago");
    CHECK(fmt.format(-1, time_unit::hour) == "1 hour ago");
    CHECK(fmt.format(-1, time_unit::day) == "1 day ago");
    CHECK(fmt.format(-1, time_unit::week) == "1 week ago");
    CHECK(fmt.format(-1, time_unit::month) == "1 month ago");
    CHECK(fmt.format(-1, time_unit::quarter) == "1 quarter ago");
    CHECK(fmt.format(-1, time_unit::year) == "1 year ago");
}

TEST_CASE("format_value_unit_always_future_singular", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::always);

    BOOST_LOG_SEV(lg, info) << "Testing always style with +1 values (future)";

    CHECK(fmt.format(1, time_unit::second) == "in 1 second");
    CHECK(fmt.format(1, time_unit::minute) == "in 1 minute");
    CHECK(fmt.format(1, time_unit::hour) == "in 1 hour");
    CHECK(fmt.format(1, time_unit::day) == "in 1 day");
    CHECK(fmt.format(1, time_unit::week) == "in 1 week");
    CHECK(fmt.format(1, time_unit::month) == "in 1 month");
    CHECK(fmt.format(1, time_unit::quarter) == "in 1 quarter");
    CHECK(fmt.format(1, time_unit::year) == "in 1 year");
}

// =============================================================================
// Tests for format(time_point, reference) - auto detection of units
// =============================================================================

TEST_CASE("format_time_point_just_now", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -5s);

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "5 seconds ago (automatic): " << result;

    CHECK(result == "just now");
}

TEST_CASE("format_time_point_seconds_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -30s);

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "30 seconds ago: " << result;

    CHECK(result == "30 seconds ago");
}

TEST_CASE("format_time_point_minutes_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -5min);

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "5 minutes ago: " << result;

    CHECK(result == "5 minutes ago");
}

TEST_CASE("format_time_point_one_minute_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -90s); // 1.5 minutes -> 1 minute

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "1 minute ago: " << result;

    CHECK(result == "1 minute ago");
}

TEST_CASE("format_time_point_hours_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -3h);

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "3 hours ago: " << result;

    CHECK(result == "3 hours ago");
}

TEST_CASE("format_time_point_one_hour_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -1h);

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "1 hour ago: " << result;

    CHECK(result == "1 hour ago");
}

TEST_CASE("format_time_point_yesterday", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -24h);

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "yesterday: " << result;

    CHECK(result == "yesterday");
}

TEST_CASE("format_time_point_days_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -72h); // 3 days

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "3 days ago: " << result;

    CHECK(result == "3 days ago");
}

TEST_CASE("format_time_point_last_week", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -168h); // 7 days = 1 week

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "last week: " << result;

    CHECK(result == "last week");
}

TEST_CASE("format_time_point_weeks_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -504h); // 21 days = 3 weeks

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "3 weeks ago: " << result;

    CHECK(result == "3 weeks ago");
}

TEST_CASE("format_time_point_last_month", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    // About 35 days, which should be 1 month
    auto past = offset_from(reference, -seconds(35 * 86400));

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "last month: " << result;

    CHECK(result == "last month");
}

TEST_CASE("format_time_point_months_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    // About 90 days = 2.96 months (using 30.44 day average month), truncates to 2
    auto past = offset_from(reference, -seconds(90 * 86400));

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "2 months ago: " << result;

    CHECK(result == "2 months ago");
}

TEST_CASE("format_time_point_last_year", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    // About 400 days = ~1 year
    auto past = offset_from(reference, -seconds(400 * 86400));

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "last year: " << result;

    CHECK(result == "last year");
}

TEST_CASE("format_time_point_years_ago", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    // 3 average years (365.24 days each) = 3 * 31556952 seconds
    auto past = offset_from(reference, -seconds(3 * 31556952));

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "3 years ago: " << result;

    CHECK(result == "3 years ago");
}

// =============================================================================
// Tests for future times
// =============================================================================

TEST_CASE("format_time_point_in_seconds", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto future = offset_from(reference, 30s);

    auto result = fmt.format(future, reference);

    BOOST_LOG_SEV(lg, info) << "in 30 seconds: " << result;

    CHECK(result == "in 30 seconds");
}

TEST_CASE("format_time_point_in_minutes", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto future = offset_from(reference, 10min);

    auto result = fmt.format(future, reference);

    BOOST_LOG_SEV(lg, info) << "in 10 minutes: " << result;

    CHECK(result == "in 10 minutes");
}

TEST_CASE("format_time_point_tomorrow", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto future = offset_from(reference, 24h);

    auto result = fmt.format(future, reference);

    BOOST_LOG_SEV(lg, info) << "tomorrow: " << result;

    CHECK(result == "tomorrow");
}

TEST_CASE("format_time_point_next_week", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto future = offset_from(reference, 168h); // 7 days

    auto result = fmt.format(future, reference);

    BOOST_LOG_SEV(lg, info) << "next week: " << result;

    CHECK(result == "next week");
}

TEST_CASE("format_time_point_next_month", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto future = offset_from(reference, seconds(35 * 86400));

    auto result = fmt.format(future, reference);

    BOOST_LOG_SEV(lg, info) << "next month: " << result;

    CHECK(result == "next month");
}

TEST_CASE("format_time_point_next_year", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::automatic);

    auto reference = system_clock::now();
    auto future = offset_from(reference, seconds(400 * 86400));

    auto result = fmt.format(future, reference);

    BOOST_LOG_SEV(lg, info) << "next year: " << result;

    CHECK(result == "next year");
}

// =============================================================================
// Tests for numeric style with time points
// =============================================================================

TEST_CASE("format_time_point_numeric_style_yesterday", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::always);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -24h);

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "1 day ago (always style): " << result;

    CHECK(result == "1 day ago");
}

TEST_CASE("format_time_point_numeric_style_tomorrow", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::always);

    auto reference = system_clock::now();
    auto future = offset_from(reference, 24h);

    auto result = fmt.format(future, reference);

    BOOST_LOG_SEV(lg, info) << "in 1 day (always style): " << result;

    CHECK(result == "in 1 day");
}

TEST_CASE("format_time_point_numeric_style_short_time", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt(numeric_style::always);

    auto reference = system_clock::now();
    auto past = offset_from(reference, -5s);

    auto result = fmt.format(past, reference);

    BOOST_LOG_SEV(lg, info) << "5 seconds ago (always style): " << result;

    // With always style, should show numeric even for small values
    CHECK(result == "5 seconds ago");
}

// =============================================================================
// Table-driven tests
// =============================================================================

TEST_CASE("format_value_unit_table_driven", tags) {
    auto lg(make_logger(test_suite));
    relative_time_formatter fmt_auto(numeric_style::automatic);
    relative_time_formatter fmt_always(numeric_style::always);

    struct test_case {
        long long value;
        time_unit unit;
        std::string expected_auto;
        std::string expected_always;
    };

    std::vector<test_case> tests = {
        {0, time_unit::day, "today", "now"},
        {-1, time_unit::day, "yesterday", "1 day ago"},
        {1, time_unit::day, "tomorrow", "in 1 day"},
        {-2, time_unit::day, "2 days ago", "2 days ago"},
        {2, time_unit::day, "in 2 days", "in 2 days"},
        {-1, time_unit::week, "last week", "1 week ago"},
        {1, time_unit::week, "next week", "in 1 week"},
        {-1, time_unit::month, "last month", "1 month ago"},
        {1, time_unit::month, "next month", "in 1 month"},
        {-1, time_unit::year, "last year", "1 year ago"},
        {1, time_unit::year, "next year", "in 1 year"},
    };

    for (const auto& tc : tests) {
        auto result_auto = fmt_auto.format(tc.value, tc.unit);
        auto result_always = fmt_always.format(tc.value, tc.unit);

        BOOST_LOG_SEV(lg, info) << "Value: " << tc.value << ", Unit: "
                                << static_cast<int>(tc.unit)
                                << ", Auto: " << result_auto
                                << ", Always: " << result_always;

        CHECK(result_auto == tc.expected_auto);
        CHECK(result_always == tc.expected_always);
    }
}
