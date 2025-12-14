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
#include "ores.comms/messaging/compression.hpp"

#include <span>
#include <cstdint>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.comms.tests");
const std::string tags("[compression]");

}

using namespace ores::utility::log;
using ores::comms::messaging::compression_type;
using ores::comms::messaging::compress;
using ores::comms::messaging::decompress;
using ores::comms::messaging::is_compression_supported;

TEST_CASE("test_is_compression_supported", tags) {
    auto lg(make_logger(test_suite));

    CHECK(is_compression_supported(compression_type::none));
    CHECK(is_compression_supported(compression_type::zlib));
    CHECK(is_compression_supported(compression_type::gzip));
    CHECK(is_compression_supported(compression_type::bzip2));

    BOOST_LOG_SEV(lg, debug) << "All compression types are supported";
}

TEST_CASE("test_compress_decompress_zlib", tags) {
    auto lg(make_logger(test_suite));

    // Create test data
    std::vector<std::byte> data(1000);
    for (size_t i = 0; i < data.size(); ++i) {
        data[i] = static_cast<std::byte>('A' + (i % 26));
    }

    BOOST_LOG_SEV(lg, debug) << "Testing zlib compression with " << data.size() << " bytes";

    // Compress
    auto compressed = compress(std::span<const std::byte>(data), compression_type::zlib);
    REQUIRE(compressed.has_value());
    CHECK(compressed->size() < data.size());

    BOOST_LOG_SEV(lg, debug) << "Compressed to " << compressed->size() << " bytes";

    // Decompress
    auto decompressed = decompress(std::span<const std::byte>(*compressed), compression_type::zlib);
    REQUIRE(decompressed.has_value());
    CHECK(decompressed->size() == data.size());
    CHECK(std::equal(data.begin(), data.end(), decompressed->begin(), decompressed->end()));

    BOOST_LOG_SEV(lg, debug) << "Zlib roundtrip successful";
}

TEST_CASE("test_compress_decompress_gzip", tags) {
    auto lg(make_logger(test_suite));

    // Create test data
    std::vector<std::byte> data(1000);
    for (size_t i = 0; i < data.size(); ++i) {
        data[i] = static_cast<std::byte>('a' + (i % 26));
    }

    BOOST_LOG_SEV(lg, debug) << "Testing gzip compression with " << data.size() << " bytes";

    // Compress
    auto compressed = compress(std::span<const std::byte>(data), compression_type::gzip);
    REQUIRE(compressed.has_value());
    CHECK(compressed->size() < data.size());

    BOOST_LOG_SEV(lg, debug) << "Compressed to " << compressed->size() << " bytes";

    // Decompress
    auto decompressed = decompress(std::span<const std::byte>(*compressed), compression_type::gzip);
    REQUIRE(decompressed.has_value());
    CHECK(decompressed->size() == data.size());
    CHECK(std::equal(data.begin(), data.end(), decompressed->begin(), decompressed->end()));

    BOOST_LOG_SEV(lg, debug) << "Gzip roundtrip successful";
}

TEST_CASE("test_compress_decompress_bzip2", tags) {
    auto lg(make_logger(test_suite));

    // Create test data
    std::vector<std::byte> data(1000);
    for (size_t i = 0; i < data.size(); ++i) {
        data[i] = static_cast<std::byte>('0' + (i % 10));
    }

    BOOST_LOG_SEV(lg, debug) << "Testing bzip2 compression with " << data.size() << " bytes";

    // Compress
    auto compressed = compress(std::span<const std::byte>(data), compression_type::bzip2);
    REQUIRE(compressed.has_value());
    CHECK(compressed->size() < data.size());

    BOOST_LOG_SEV(lg, debug) << "Compressed to " << compressed->size() << " bytes";

    // Decompress
    auto decompressed = decompress(std::span<const std::byte>(*compressed), compression_type::bzip2);
    REQUIRE(decompressed.has_value());
    CHECK(decompressed->size() == data.size());
    CHECK(std::equal(data.begin(), data.end(), decompressed->begin(), decompressed->end()));

    BOOST_LOG_SEV(lg, debug) << "Bzip2 roundtrip successful";
}

TEST_CASE("test_compress_decompress_none", tags) {
    auto lg(make_logger(test_suite));

    // Create test data
    std::vector<std::byte> data = {
        std::byte{0x01},
        std::byte{0x02},
        std::byte{0x03},
        std::byte{0x04}
    };

    BOOST_LOG_SEV(lg, debug) << "Testing no compression";

    // "Compress" with none should return same data
    auto compressed = compress(std::span<const std::byte>(data), compression_type::none);
    REQUIRE(compressed.has_value());
    CHECK(compressed->size() == data.size());
    CHECK(std::equal(data.begin(), data.end(), compressed->begin(), compressed->end()));

    // "Decompress" with none should return same data
    auto decompressed = decompress(std::span<const std::byte>(*compressed), compression_type::none);
    REQUIRE(decompressed.has_value());
    CHECK(decompressed->size() == data.size());
    CHECK(std::equal(data.begin(), data.end(), decompressed->begin(), decompressed->end()));

    BOOST_LOG_SEV(lg, debug) << "No compression roundtrip successful";
}

TEST_CASE("test_compress_empty_data", tags) {
    auto lg(make_logger(test_suite));

    std::vector<std::byte> empty_data;

    BOOST_LOG_SEV(lg, debug) << "Testing compression of empty data";

    // Compress empty data
    auto compressed = compress(std::span<const std::byte>(empty_data), compression_type::zlib);
    REQUIRE(compressed.has_value());

    // Decompress
    auto decompressed = decompress(std::span<const std::byte>(*compressed), compression_type::zlib);
    REQUIRE(decompressed.has_value());
    CHECK(decompressed->empty());

    BOOST_LOG_SEV(lg, debug) << "Empty data compression roundtrip successful";
}

TEST_CASE("test_decompress_invalid_data", tags) {
    auto lg(make_logger(test_suite));

    // Create invalid compressed data
    std::vector<std::byte> invalid_data = {
        std::byte{0xFF},
        std::byte{0xFE},
        std::byte{0xFD},
        std::byte{0xFC}
    };

    BOOST_LOG_SEV(lg, debug) << "Testing decompression of invalid data";

    // Try to decompress invalid data
    auto result = decompress(std::span<const std::byte>(invalid_data), compression_type::zlib);
    CHECK(!result.has_value());
    CHECK(result.error() == ores::comms::messaging::error_code::decompression_failed);

    BOOST_LOG_SEV(lg, debug) << "Invalid data decompression correctly failed";
}

TEST_CASE("test_compression_ratio", tags) {
    auto lg(make_logger(test_suite));

    // Create highly compressible data (repeated pattern)
    std::vector<std::byte> data(10000);
    for (size_t i = 0; i < data.size(); ++i) {
        data[i] = static_cast<std::byte>('X');
    }

    BOOST_LOG_SEV(lg, debug) << "Testing compression ratio with highly compressible data";

    // Test all compression types
    auto zlib_compressed = compress(std::span<const std::byte>(data), compression_type::zlib);
    REQUIRE(zlib_compressed.has_value());
    BOOST_LOG_SEV(lg, debug) << "Zlib: " << data.size() << " -> " << zlib_compressed->size()
                             << " (" << (100.0 * zlib_compressed->size() / data.size()) << "%)";

    auto gzip_compressed = compress(std::span<const std::byte>(data), compression_type::gzip);
    REQUIRE(gzip_compressed.has_value());
    BOOST_LOG_SEV(lg, debug) << "Gzip: " << data.size() << " -> " << gzip_compressed->size()
                             << " (" << (100.0 * gzip_compressed->size() / data.size()) << "%)";

    auto bzip2_compressed = compress(std::span<const std::byte>(data), compression_type::bzip2);
    REQUIRE(bzip2_compressed.has_value());
    BOOST_LOG_SEV(lg, debug) << "Bzip2: " << data.size() << " -> " << bzip2_compressed->size()
                             << " (" << (100.0 * bzip2_compressed->size() / data.size()) << "%)";

    // Verify all are significantly smaller
    CHECK(zlib_compressed->size() < data.size() / 10);
    CHECK(gzip_compressed->size() < data.size() / 10);
    CHECK(bzip2_compressed->size() < data.size() / 10);
}

// Handshake compression negotiation tests
#include "ores.comms/messaging/handshake_protocol.hpp"

using ores::comms::messaging::select_compression;
using ores::comms::messaging::COMPRESSION_SUPPORT_ZLIB;
using ores::comms::messaging::COMPRESSION_SUPPORT_GZIP;
using ores::comms::messaging::COMPRESSION_SUPPORT_BZIP2;
using ores::comms::messaging::COMPRESSION_SUPPORT_ALL;

TEST_CASE("test_select_compression_no_support", "[compression][handshake]") {
    auto lg(make_logger(test_suite));

    // Client doesn't support compression
    auto selected = select_compression(0x00);
    CHECK(selected == compression_type::none);

    BOOST_LOG_SEV(lg, debug) << "No compression support returns none";
}

TEST_CASE("test_select_compression_zlib_only", "[compression][handshake]") {
    auto lg(make_logger(test_suite));

    auto selected = select_compression(COMPRESSION_SUPPORT_ZLIB);
    CHECK(selected == compression_type::zlib);

    BOOST_LOG_SEV(lg, debug) << "Zlib only returns zlib";
}

TEST_CASE("test_select_compression_gzip_only", "[compression][handshake]") {
    auto lg(make_logger(test_suite));

    auto selected = select_compression(COMPRESSION_SUPPORT_GZIP);
    CHECK(selected == compression_type::gzip);

    BOOST_LOG_SEV(lg, debug) << "Gzip only returns gzip";
}

TEST_CASE("test_select_compression_bzip2_only", "[compression][handshake]") {
    auto lg(make_logger(test_suite));

    auto selected = select_compression(COMPRESSION_SUPPORT_BZIP2);
    CHECK(selected == compression_type::bzip2);

    BOOST_LOG_SEV(lg, debug) << "Bzip2 only returns bzip2";
}

TEST_CASE("test_select_compression_all_supported", "[compression][handshake]") {
    auto lg(make_logger(test_suite));

    // With all supported, should return preferred (default: zlib)
    auto selected = select_compression(COMPRESSION_SUPPORT_ALL);
    CHECK(selected == compression_type::zlib);

    BOOST_LOG_SEV(lg, debug) << "All supported with default preference returns zlib";
}

TEST_CASE("test_select_compression_preferred_gzip", "[compression][handshake]") {
    auto lg(make_logger(test_suite));

    // Server prefers gzip, client supports all
    auto selected = select_compression(COMPRESSION_SUPPORT_ALL, compression_type::gzip);
    CHECK(selected == compression_type::gzip);

    BOOST_LOG_SEV(lg, debug) << "Server preference for gzip honored";
}

TEST_CASE("test_select_compression_preferred_not_supported", "[compression][handshake]") {
    auto lg(make_logger(test_suite));

    // Server prefers bzip2, but client only supports zlib
    auto selected = select_compression(COMPRESSION_SUPPORT_ZLIB, compression_type::bzip2);
    CHECK(selected == compression_type::zlib);

    BOOST_LOG_SEV(lg, debug) << "Falls back to supported type when preferred not available";
}

TEST_CASE("test_select_compression_gzip_bzip2", "[compression][handshake]") {
    auto lg(make_logger(test_suite));

    // Client supports gzip and bzip2, but not zlib
    auto selected = select_compression(COMPRESSION_SUPPORT_GZIP | COMPRESSION_SUPPORT_BZIP2);
    CHECK(selected == compression_type::gzip);

    BOOST_LOG_SEV(lg, debug) << "Falls back in order: zlib > gzip > bzip2";
}
