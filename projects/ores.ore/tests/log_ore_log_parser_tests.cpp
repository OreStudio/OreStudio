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
#include "ores.ore/log/ore_log_parser.hpp"

#include <chrono>
#include <ctime>
#include <string>
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[ore][log][ore_log_parser]");

// Build a UTC time_point from explicit fields (no DST, no timezone conversion).
std::chrono::system_clock::time_point make_utc(
    int year, int mon, int mday,
    int hour, int min, int sec, long microseconds = 0)
{
    std::tm tm = {};
    tm.tm_year  = year - 1900;
    tm.tm_mon   = mon - 1;
    tm.tm_mday  = mday;
    tm.tm_hour  = hour;
    tm.tm_min   = min;
    tm.tm_sec   = sec;
    tm.tm_isdst = 0;
    const std::time_t t = timegm(&tm);
    return std::chrono::system_clock::from_time_t(t)
        + std::chrono::microseconds(microseconds);
}

} // namespace

using ores::ore::log::parse_ore_log_line;

// =============================================================================
// Valid lines — all five ORE levels
// =============================================================================

TEST_CASE("parse_data_level_maps_to_debug", tags) {
    const auto result = parse_ore_log_line(
        "DATA    [2024-Jan-15 09:12:34.000001]    (ore/oreapp.cpp:42) : some debug info");

    REQUIRE(result.has_value());
    CHECK(result->level   == "debug");
    CHECK(result->source  == "ore/oreapp.cpp:42");
    CHECK(result->message == "some debug info");
    CHECK(result->timestamp == make_utc(2024, 1, 15, 9, 12, 34, 1));
}

TEST_CASE("parse_notice_level_maps_to_info", tags) {
    const auto result = parse_ore_log_line(
        "NOTICE    [2025-Mar-01 00:00:00.000000]    (ored/main.cpp:10) : application started");

    REQUIRE(result.has_value());
    CHECK(result->level   == "info");
    CHECK(result->source  == "ored/main.cpp:10");
    CHECK(result->message == "application started");
    CHECK(result->timestamp == make_utc(2025, 3, 1, 0, 0, 0, 0));
}

TEST_CASE("parse_warning_level_maps_to_warn", tags) {
    const auto result = parse_ore_log_line(
        "WARNING    [2024-Dec-31 23:59:59.123456]    (risk/engine.cpp:99) : curve not found");

    REQUIRE(result.has_value());
    CHECK(result->level   == "warn");
    CHECK(result->source  == "risk/engine.cpp:99");
    CHECK(result->message == "curve not found");
    CHECK(result->timestamp == make_utc(2024, 12, 31, 23, 59, 59, 123456));
}

TEST_CASE("parse_error_level_maps_to_error", tags) {
    const auto result = parse_ore_log_line(
        "ERROR    [2024-Jun-15 12:00:00.500000]    (pricing/model.cpp:200) : pricing failed");

    REQUIRE(result.has_value());
    CHECK(result->level   == "error");
    CHECK(result->source  == "pricing/model.cpp:200");
    CHECK(result->message == "pricing failed");
    CHECK(result->timestamp == make_utc(2024, 6, 15, 12, 0, 0, 500000));
}

TEST_CASE("parse_alert_level_maps_to_error", tags) {
    const auto result = parse_ore_log_line(
        "ALERT    [2024-Sep-10 08:30:00.000000]    (monitor/alert.cpp:5) : critical threshold");

    REQUIRE(result.has_value());
    CHECK(result->level   == "error");
    CHECK(result->source  == "monitor/alert.cpp:5");
    CHECK(result->message == "critical threshold");
    CHECK(result->timestamp == make_utc(2024, 9, 10, 8, 30, 0, 0));
}

// =============================================================================
// Message field
// =============================================================================

TEST_CASE("parse_message_with_colon_inside_is_preserved", tags) {
    const auto result = parse_ore_log_line(
        "NOTICE    [2024-Feb-20 10:00:00.000000]    (a/b.cpp:1) : key: value: extra");

    REQUIRE(result.has_value());
    CHECK(result->message == "key: value: extra");
}

TEST_CASE("parse_message_with_leading_whitespace_after_colon", tags) {
    const auto result = parse_ore_log_line(
        "NOTICE    [2024-Feb-20 10:00:00.000000]    (a/b.cpp:1) :   spaced message");

    REQUIRE(result.has_value());
    CHECK(result->message == "  spaced message");
}

// =============================================================================
// Microsecond handling
// =============================================================================

TEST_CASE("parse_microseconds_fewer_than_six_digits_are_scaled_to_microseconds", tags) {
    // "123" → treated as 123000 µs (i.e., 0.123 seconds, right-padded with zeros)
    const auto result = parse_ore_log_line(
        "NOTICE    [2024-Jan-01 00:00:01.123]    (x.cpp:1) : msg");

    REQUIRE(result.has_value());
    CHECK(result->timestamp == make_utc(2024, 1, 1, 0, 0, 1, 123000));
}

TEST_CASE("parse_zero_microseconds_is_accepted", tags) {
    const auto result = parse_ore_log_line(
        "NOTICE    [2024-Jan-01 00:00:00.000000]    (x.cpp:1) : msg");

    REQUIRE(result.has_value());
    CHECK(result->timestamp == make_utc(2024, 1, 1, 0, 0, 0, 0));
}

// =============================================================================
// Malformed / empty lines
// =============================================================================

TEST_CASE("parse_empty_line_returns_nullopt", tags) {
    CHECK_FALSE(parse_ore_log_line("").has_value());
}

TEST_CASE("parse_whitespace_only_line_returns_nullopt", tags) {
    CHECK_FALSE(parse_ore_log_line("   \t  ").has_value());
}

TEST_CASE("parse_unknown_level_returns_nullopt", tags) {
    CHECK_FALSE(parse_ore_log_line(
        "DEBUG    [2024-Jan-01 00:00:00.000000]    (x.cpp:1) : msg").has_value());
}

TEST_CASE("parse_missing_timestamp_brackets_returns_nullopt", tags) {
    CHECK_FALSE(parse_ore_log_line(
        "NOTICE    2024-Jan-01 00:00:00.000000    (x.cpp:1) : msg").has_value());
}

TEST_CASE("parse_missing_source_parens_returns_nullopt", tags) {
    CHECK_FALSE(parse_ore_log_line(
        "NOTICE    [2024-Jan-01 00:00:00.000000]    x.cpp:1 : msg").has_value());
}

TEST_CASE("parse_missing_colon_separator_returns_nullopt", tags) {
    CHECK_FALSE(parse_ore_log_line(
        "NOTICE    [2024-Jan-01 00:00:00.000000]    (x.cpp:1) message without colon").has_value());
}

TEST_CASE("parse_invalid_timestamp_returns_nullopt", tags) {
    CHECK_FALSE(parse_ore_log_line(
        "NOTICE    [not-a-date]    (x.cpp:1) : msg").has_value());
}
