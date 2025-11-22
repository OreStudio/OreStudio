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
#include "ores.utility/convert/base64_converter.hpp"

#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace {

const std::string test_suite("ores.utility.tests");
const std::string tags("[convert]");

}

using ores::utility::converter::base64_converter;
using namespace ores::utility::log;

TEST_CASE("base64_encode_empty_input", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> input;
    auto result = base64_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Empty input encoded to: '" << result << "'";

    CHECK(result.empty());
}

TEST_CASE("base64_encode_hello", tags) {
    auto lg(make_logger(test_suite));

    std::string input_str = "Hello";
    std::vector<uint8_t> input(input_str.begin(), input_str.end());
    auto result = base64_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Input: '" << input_str << "', Result: " << result;

    // Standard Base64 encoding of "Hello" is "SGVsbG8="
    CHECK(result == "SGVsbG8=");
}

TEST_CASE("base64_encode_hello_world", tags) {
    auto lg(make_logger(test_suite));

    std::string input_str = "Hello, World!";
    std::vector<uint8_t> input(input_str.begin(), input_str.end());
    auto result = base64_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Input: '" << input_str << "', Result: " << result;

    CHECK(result == "SGVsbG8sIFdvcmxkIQ==");
}

TEST_CASE("base64_decode_hello", tags) {
    auto lg(make_logger(test_suite));

    std::string encoded = "SGVsbG8=";
    auto result = base64_converter::convert(encoded);

    std::string result_str(result.begin(), result.end());
    BOOST_LOG_SEV(lg, info) << "Encoded: '" << encoded
                            << "', Decoded: '" << result_str << "'";

    CHECK(result_str == "Hello");
}

TEST_CASE("base64_roundtrip_conversion", tags) {
    auto lg(make_logger(test_suite));

    std::string original = "The quick brown fox jumps over the lazy dog";
    std::vector<uint8_t> input(original.begin(), original.end());

    // Encode
    auto encoded = base64_converter::convert(input);
    BOOST_LOG_SEV(lg, info) << "Original: '" << original << "'";
    BOOST_LOG_SEV(lg, info) << "Encoded: '" << encoded << "'";

    // Decode
    auto decoded = base64_converter::convert(encoded);
    std::string decoded_str(decoded.begin(), decoded.end());
    BOOST_LOG_SEV(lg, info) << "Decoded: '" << decoded_str << "'";

    CHECK(decoded_str == original);
}

TEST_CASE("base64_encode_binary_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> input = {0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD};
    auto result = base64_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "Binary data encoded to: " << result;

    // Verify result is valid Base64
    const std::string valid_chars =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=";
    for (char c : result) {
        CHECK(valid_chars.find(c) != std::string::npos);
    }

    CHECK(!result.empty());
}

TEST_CASE("base64_roundtrip_binary_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> original = {
        0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
        0x88, 0x99, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF
    };

    // Encode
    auto encoded = base64_converter::convert(original);
    BOOST_LOG_SEV(lg, info) << "Binary data encoded to: " << encoded;

    // Decode
    auto decoded = base64_converter::convert(encoded);

    BOOST_LOG_SEV(lg, info) << "Original size: " << original.size()
                            << ", Decoded size: " << decoded.size();

    CHECK(decoded == original);
}

TEST_CASE("base64_decode_empty_throws", tags) {
    auto lg(make_logger(test_suite));

    std::string empty;

    BOOST_LOG_SEV(lg, info) << "Attempting to decode empty string";

    CHECK_THROWS_AS(base64_converter::convert(empty), std::runtime_error);
}

TEST_CASE("base64_encode_various_lengths", tags) {
    auto lg(make_logger(test_suite));

    // Test various input lengths to verify padding is correct
    std::vector<std::string> tests = {
        "a",      // 1 byte
        "ab",     // 2 bytes
        "abc",    // 3 bytes (no padding needed)
        "abcd",   // 4 bytes
        "abcde",  // 5 bytes
        "abcdef"  // 6 bytes (no padding needed)
    };

    for (const auto& test : tests) {
        std::vector<uint8_t> input(test.begin(), test.end());
        auto encoded = base64_converter::convert(input);
        auto decoded = base64_converter::convert(encoded);
        std::string decoded_str(decoded.begin(), decoded.end());

        BOOST_LOG_SEV(lg, info) << "Input: '" << test
                                << "', Encoded: '" << encoded
                                << "', Decoded: '" << decoded_str << "'";

        CHECK(decoded_str == test);
    }
}

TEST_CASE("base64_encode_long_text", tags) {
    auto lg(make_logger(test_suite));

    std::string original =
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit, "
        "sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
        "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris.";

    std::vector<uint8_t> input(original.begin(), original.end());

    auto encoded = base64_converter::convert(input);
    BOOST_LOG_SEV(lg, info) << "Long text encoded, length: " << encoded.length();

    auto decoded = base64_converter::convert(encoded);
    std::string decoded_str(decoded.begin(), decoded.end());

    CHECK(decoded_str == original);
}

TEST_CASE("base64_encode_all_zeros", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> input(100, 0x00);
    auto encoded = base64_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "100 zeros encoded to length: " << encoded.length();

    auto decoded = base64_converter::convert(encoded);

    CHECK(decoded == input);
    CHECK(decoded.size() == 100);
}

TEST_CASE("base64_encode_all_ones", tags) {
    auto lg(make_logger(test_suite));

    std::vector<uint8_t> input(50, 0xFF);
    auto encoded = base64_converter::convert(input);

    BOOST_LOG_SEV(lg, info) << "50 0xFF bytes encoded to: " << encoded;

    auto decoded = base64_converter::convert(encoded);

    CHECK(decoded == input);
}
