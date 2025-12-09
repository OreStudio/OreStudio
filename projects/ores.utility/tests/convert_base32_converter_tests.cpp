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
#include "ores.utility/convert/base32_converter.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[convert]");

}

using ores::utility::converter::base32_converter;
using namespace ores::utility::log;

TEST_CASE("base32_encode_empty_input", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> input;
    auto result = base32_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Empty input encoded to: '" << result << "'";

    CHECK(result.empty());
}

TEST_CASE("base32_encode_single_byte", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> input = {0x48}; // 'H'
    auto result = base32_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Input: 0x48, Result: " << result;

    // 0x48 = 01001000 in binary
    // Split into 5-bit groups: 01001 000
    // 01001 = 9 -> 'J', 000 padded to 00000 = 0 -> 'A'
    CHECK(result == "JA");
}

TEST_CASE("base32_encode_hello", tags) {
    auto lg(make_logger(test_suite));

    std::string input_str = "Hello";
    std::vector<uint8_t> input(input_str.begin(), input_str.end());
    auto result = base32_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Input: '" << input_str << "', Result: " << result;

    // RFC 4648 test vector: "Hello" -> "JBSWY3DP"
    CHECK(result == "JBSWY3DP");
}

TEST_CASE("base32_encode_foobar", tags) {
    auto lg(make_logger(test_suite));

    std::string input_str = "foobar";
    std::vector<uint8_t> input(input_str.begin(), input_str.end());
    auto result = base32_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Input: '" << input_str << "', Result: " << result;

    // RFC 4648 test vector: "foobar" -> "MZXW6YTBOI"
    CHECK(result == "MZXW6YTBOI");
}

TEST_CASE("base32_encode_various_lengths", tags) {
    auto lg(make_logger(test_suite));

    // Test various input lengths to verify proper handling of padding
    struct test_case {
        std::string input;
        std::string expected;
    };

    std::vector<test_case> tests = {
        {.input="f", .expected="MY"},
        {.input="fo", .expected="MZXQ"},
        {.input="foo", .expected="MZXW6"},
        {.input="foob", .expected="MZXW6YQ"},
        {.input="fooba", .expected="MZXW6YTB"},
        {.input="foobar", .expected="MZXW6YTBOI"}
    };

    for (const auto& tc : tests) {
        std::vector<uint8_t> input(tc.input.begin(), tc.input.end());
        auto result = base32_converter::convert(input);

        BOOST_LOG_SEV(lg, info) << "Input: '" << tc.input
                                << "', Expected: " << tc.expected
                                << ", Got: " << result;

        CHECK(result == tc.expected);
    }
}

TEST_CASE("base32_encode_binary_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> input = {0x00, 0xFF, 0xAB, 0xCD, 0xEF};
    auto result = base32_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Binary input encoded to: " << result;

    // Verify result is valid Base32 (only contains valid alphabet characters)
    const std::string valid_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";
    for (char c : result) {
        CHECK(valid_chars.find(c) != std::string::npos);
    }

    // Check length is reasonable for input size
    CHECK(!result.empty());
}

TEST_CASE("base32_encode_all_zeros", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> input = {0x00, 0x00, 0x00, 0x00, 0x00};
    auto result = base32_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "All zeros encoded to: " << result;

    // 5 bytes of zeros should encode to 8 'A's (no padding in this implementation)
    CHECK(result == "AAAAAAAA");
}

TEST_CASE("base32_encode_alphabet_coverage", tags) {
    auto lg(make_logger(test_suite));

    // Test input that uses the full alphabet
    std::vector<uint8_t> input = {
        0x00, 0x44, 0x32, 0x14, 0xC7, 0x42, 0x54, 0xB6,
        0x35, 0xCF, 0x84, 0x65, 0x3A, 0x56, 0xD7, 0xC6,
        0x75, 0xBE, 0x77, 0xDF
    };

    auto result = base32_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Alphabet coverage result: " << result;

    // RFC 4648 test: This should produce "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
    CHECK(result == "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567");
}
