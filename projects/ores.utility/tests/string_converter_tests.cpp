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
#include "ores.utility/string/converter.hpp"

#include <limits>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/string/conversion_error.hpp"

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[string]");

}

using ores::utility::string::converter;
using ores::utility::string::conversion_error;
using namespace ores::utility::log;

TEST_CASE("string_to_int_valid_decimal", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("42");

    BOOST_LOG_SEV(lg, info) << "Converted '42' to: " << result;

    CHECK(result == 42);
}

TEST_CASE("string_to_int_negative_number", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("-123");

    BOOST_LOG_SEV(lg, info) << "Converted '-123' to: " << result;

    CHECK(result == -123);
}

TEST_CASE("string_to_int_zero", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("0");

    BOOST_LOG_SEV(lg, info) << "Converted '0' to: " << result;

    CHECK(result == 0);
}

TEST_CASE("string_to_int_hexadecimal", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("FF", 16);

    BOOST_LOG_SEV(lg, info) << "Converted 'FF' (base 16) to: " << result;

    CHECK(result == 255);
}

TEST_CASE("string_to_int_hex_lowercase", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("1abc", 16);

    BOOST_LOG_SEV(lg, info) << "Converted '1abc' (base 16) to: " << result;

    CHECK(result == 0x1ABC);
}

TEST_CASE("string_to_int_binary", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("1010", 2);

    BOOST_LOG_SEV(lg, info) << "Converted '1010' (base 2) to: " << result;

    CHECK(result == 10);
}

TEST_CASE("string_to_int_octal", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("777", 8);

    BOOST_LOG_SEV(lg, info) << "Converted '777' (base 8) to: " << result;

    CHECK(result == 511);
}

TEST_CASE("string_to_int_max_int", tags) {
    auto lg(make_logger(test_suite));

    auto max_str = std::to_string(std::numeric_limits<int>::max());
    auto result = converter::string_to_int(max_str);

    BOOST_LOG_SEV(lg, info) << "Converted max int: " << result;

    CHECK(result == std::numeric_limits<int>::max());
}

TEST_CASE("string_to_int_min_int", tags) {
    auto lg(make_logger(test_suite));

    auto min_str = std::to_string(std::numeric_limits<int>::min());
    auto result = converter::string_to_int(min_str);

    BOOST_LOG_SEV(lg, info) << "Converted min int: " << result;

    CHECK(result == std::numeric_limits<int>::min());
}

TEST_CASE("string_to_int_invalid_empty_string", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Attempting to convert empty string";

    CHECK_THROWS_AS(converter::string_to_int(""), conversion_error);
}

TEST_CASE("string_to_int_invalid_non_numeric", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Attempting to convert 'abc'";

    CHECK_THROWS_AS(converter::string_to_int("abc"), conversion_error);
}

TEST_CASE("string_to_int_partial_parse_stops_at_invalid", tags) {
    auto lg(make_logger(test_suite));

    // std::from_chars parses "123" and stops at 'abc'
    // This is valid behavior - it successfully parses the numeric prefix
    auto result = converter::string_to_int("123abc");

    BOOST_LOG_SEV(lg, info) << "Converted '123abc' to: " << result;

    CHECK(result == 123);
}

TEST_CASE("string_to_int_invalid_whitespace", tags) {
    auto lg(make_logger(test_suite));

    BOOST_LOG_SEV(lg, info) << "Attempting to convert ' 123'";

    CHECK_THROWS_AS(converter::string_to_int(" 123"), conversion_error);
}

TEST_CASE("string_to_int_out_of_range_overflow", tags) {
    auto lg(make_logger(test_suite));

    // Create a number larger than INT_MAX
    std::string huge_number = "9999999999999999999";

    BOOST_LOG_SEV(lg, info) << "Attempting to convert out-of-range: " << huge_number;

    CHECK_THROWS_AS(converter::string_to_int(huge_number), conversion_error);
}

TEST_CASE("string_to_int_out_of_range_underflow", tags) {
    auto lg(make_logger(test_suite));

    // Create a number smaller than INT_MIN
    std::string huge_negative = "-9999999999999999999";

    BOOST_LOG_SEV(lg, info) << "Attempting to convert out-of-range: " << huge_negative;

    CHECK_THROWS_AS(converter::string_to_int(huge_negative), conversion_error);
}

TEST_CASE("string_to_int_different_bases", tags) {
    auto lg(make_logger(test_suite));

    struct test_case {
        std::string input;
        int base;
        int expected;
    };

    auto placeholder = "100";
    std::vector<test_case> tests = {{.input = "100", .base = 10, .expected = 100},
                                    {.input=placeholder, .base=2, .expected=4},
                                    {.input="100", .base=8, .expected=64},
                                    {.input="100", .base=16, .expected=256},
                                    {.input="1A", .base=16, .expected=26},
                                    {.input="77", .base=8, .expected=63},
                                    {.input="11111111", .base=2, .expected=255}};

    for (const auto& tc : tests) {
        auto result = converter::string_to_int(tc.input, tc.base);

        BOOST_LOG_SEV(lg, info) << "Input: '" << tc.input
                                << "' (base " << tc.base << ") = " << result;

        CHECK(result == tc.expected);
    }
}

TEST_CASE("string_to_int_partial_parse_binary", tags) {
    auto lg(make_logger(test_suite));

    // '9' is not valid in binary, so from_chars parses "101" and stops
    auto result = converter::string_to_int("1019", 2);

    BOOST_LOG_SEV(lg, info) << "Converted '1019' (base 2) to: " << result;

    CHECK(result == 5); // Binary 101 = 5
}

TEST_CASE("string_to_int_partial_parse_hex", tags) {
    auto lg(make_logger(test_suite));

    // 'G' is not valid in hexadecimal, so from_chars parses "FFF" and stops
    auto result = converter::string_to_int("FFFG", 16);

    BOOST_LOG_SEV(lg, info) << "Converted 'FFFG' (base 16) to: " << result;

    CHECK(result == 0xFFF); // 4095
}

TEST_CASE("string_to_int_large_hex_value", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("7FFFFFFF", 16);

    BOOST_LOG_SEV(lg, info) << "Converted '7FFFFFFF' (base 16) to: " << result;

    CHECK(result == 0x7FFFFFFF);
}

TEST_CASE("string_to_int_negative_hex", tags) {
    auto lg(make_logger(test_suite));

    auto result = converter::string_to_int("-FF", 16);

    BOOST_LOG_SEV(lg, info) << "Converted '-FF' (base 16) to: " << result;

    CHECK(result == -255);
}
