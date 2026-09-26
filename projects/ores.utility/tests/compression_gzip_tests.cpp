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
#include "ores.logging/make_logger.hpp"
#include "ores.utility/compression/gzip.hpp"
#include <catch2/catch_test_macros.hpp>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[compression]");

std::string to_string(const std::vector<char>& bytes) {
    return {bytes.begin(), bytes.end()};
}

}

using ores::utility::compression::gzip_compress;
using ores::utility::compression::gzip_decompress;
using namespace ores::logging;

TEST_CASE("gzip_roundtrip_preserves_payload", tags) {
    auto lg(make_logger(test_suite));

    const std::string payload("hello world");
    const auto compressed = gzip_compress(payload);
    const auto restored = gzip_decompress(compressed);

    BOOST_LOG_SEV(lg, info) << "Payload " << payload.size() << " bytes compressed to "
                            << compressed.size() << " bytes";

    CHECK(to_string(restored) == payload);
    CHECK(to_string(compressed) != payload);
}

TEST_CASE("gzip_compress_emits_the_gzip_container_header", tags) {
    auto lg(make_logger(test_suite));

    const auto compressed = gzip_compress(std::string("hello world"));

    REQUIRE(compressed.size() >= 3);
    CHECK(static_cast<unsigned char>(compressed[0]) == 0x1f);
    CHECK(static_cast<unsigned char>(compressed[1]) == 0x8b);
    CHECK(static_cast<unsigned char>(compressed[2]) == 0x08);

    BOOST_LOG_SEV(lg, info) << "Container header: " << std::hex
                            << static_cast<int>(static_cast<unsigned char>(compressed[0])) << " "
                            << static_cast<int>(static_cast<unsigned char>(compressed[1])) << " "
                            << static_cast<int>(static_cast<unsigned char>(compressed[2]));
}

TEST_CASE("gzip_decompress_reads_a_stream_from_another_producer", tags) {
    auto lg(make_logger(test_suite));

    // Produced by GNU gzip -n -9 for "The quick brown fox jumps over the lazy dog".
    const std::vector<unsigned char> stream{
        0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x03, 0x0b, 0xc9, 0x48,
        0x55, 0x28, 0x2c, 0xcd, 0x4c, 0xce, 0x56, 0x48, 0x2a, 0xca, 0x2f, 0xcf, 0x53,
        0x48, 0xcb, 0xaf, 0x50, 0xc8, 0x2a, 0xcd, 0x2d, 0x28, 0x56, 0xc8, 0x2f, 0x4b,
        0x2d, 0x52, 0x28, 0x01, 0x4a, 0xe7, 0x24, 0x56, 0x55, 0x2a, 0xa4, 0xe4, 0xa7,
        0x03, 0x00, 0x39, 0xa3, 0x4f, 0x41, 0x2b, 0x00, 0x00, 0x00};
    const std::string compressed(stream.begin(), stream.end());

    const auto restored = gzip_decompress(compressed);

    BOOST_LOG_SEV(lg, info) << "Restored " << restored.size() << " bytes";

    CHECK(to_string(restored) == "The quick brown fox jumps over the lazy dog");
}

TEST_CASE("gzip_roundtrip_of_empty_input_returns_no_bytes", tags) {
    auto lg(make_logger(test_suite));

    const auto compressed = gzip_compress(std::string());
    const auto restored = gzip_decompress(compressed);

    BOOST_LOG_SEV(lg, info) << "Empty payload compressed to " << compressed.size() << " bytes";

    CHECK(!compressed.empty());
    CHECK(restored.empty());
}

TEST_CASE("gzip_compress_shrinks_a_repetitive_payload", tags) {
    auto lg(make_logger(test_suite));

    const std::string payload(10000, 'a');
    const auto compressed = gzip_compress(payload);

    BOOST_LOG_SEV(lg, info) << "10000 bytes compressed to " << compressed.size() << " bytes";

    CHECK(compressed.size() < payload.size());
    CHECK(to_string(gzip_decompress(compressed)) == payload);
}

TEST_CASE("gzip_roundtrip_preserves_embedded_nul_bytes", tags) {
    auto lg(make_logger(test_suite));

    const std::string payload("a\0b", 3);
    const auto restored = gzip_decompress(gzip_compress(payload));

    CHECK(restored.size() == 3);
    CHECK(to_string(restored) == payload);
}

TEST_CASE("gzip_decompress_rejects_input_that_is_not_gzip", tags) {
    auto lg(make_logger(test_suite));

    const std::string not_gzip("this is not a gzip stream");

    BOOST_LOG_SEV(lg, info) << "Rejecting " << not_gzip.size() << " bytes of plain text";

    CHECK_THROWS_AS(gzip_decompress(not_gzip), std::runtime_error);
}
