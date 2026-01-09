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
#include "ores.utility/uuid/uuid_v7_generator.hpp"

#include <set>
#include <thread>
#include <bitset>
#include <iomanip>
#include <catch2/catch_test_macros.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.logging/make_logger.hpp"

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[uuid]");

}

using ores::utility::uuid::uuid_v7_generator;
using namespace ores::logging;

TEST_CASE("generate_uuid_v7_is_not_nil", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;
    auto uuid = generator();

    BOOST_LOG_SEV(lg, info) << "Generated UUID: " << uuid;

    CHECK(!uuid.is_nil());
}

TEST_CASE("generate_multiple_unique_uuids", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;
    std::set<boost::uuids::uuid> uuids;

    const size_t count = 1000;
    for (size_t i = 0; i < count; ++i) {
        auto uuid = generator();
        uuids.insert(uuid);
    }

    BOOST_LOG_SEV(lg, info) << "Generated " << count << " UUIDs, "
                            << uuids.size() << " unique";

    CHECK(uuids.size() == count);
}

TEST_CASE("uuid_v7_has_correct_version", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;
    auto uuid = generator();

    BOOST_LOG_SEV(lg, info) << "UUID: " << uuid;

    // Version is in the most significant 4 bits of byte 6
    // For v7, this should be 0x7
    const unsigned char version_byte = uuid.data[6];
    const unsigned char version = (version_byte >> 4) & 0x0F;

    BOOST_LOG_SEV(lg, info) << "Version byte: 0x" << std::hex
                            << static_cast<int>(version_byte);
    BOOST_LOG_SEV(lg, info) << "Version: " << static_cast<int>(version);

    CHECK(version == 0x7);
}

TEST_CASE("uuid_v7_has_correct_variant", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;
    auto uuid = generator();

    BOOST_LOG_SEV(lg, info) << "UUID: " << uuid;

    // Variant is in the most significant 2 bits of byte 8
    // For RFC 4122, this should be 0b10 (0x80 when checking top 2 bits)
    const unsigned char variant_byte = uuid.data[8];
    const unsigned char variant = (variant_byte >> 6) & 0x03;

    BOOST_LOG_SEV(lg, info) << "Variant byte: 0x" << std::hex
                            << static_cast<int>(variant_byte);
    BOOST_LOG_SEV(lg, info) << "Variant: 0b" << std::bitset<2>(variant);

    CHECK(variant == 0x02); // 0b10
}

TEST_CASE("uuid_v7_preserves_time_ordering", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;

    auto uuid1 = generator();
    BOOST_LOG_SEV(lg, info) << "First UUID: " << uuid1;

    // Small delay to ensure different timestamp
    std::this_thread::sleep_for(std::chrono::milliseconds(2));

    auto uuid2 = generator();
    BOOST_LOG_SEV(lg, info) << "Second UUID: " << uuid2;

    // UUID v7 is designed to be sortable by time
    // The first 6 bytes contain the timestamp, so uuid1 should be less than uuid2
    CHECK(uuid1 < uuid2);
}

TEST_CASE("uuid_v7_string_format_is_valid", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;
    auto uuid = generator();

    std::string uuid_str = boost::uuids::to_string(uuid);
    BOOST_LOG_SEV(lg, info) << "UUID string: " << uuid_str;

    // Standard UUID format: 8-4-4-4-12 (36 characters including hyphens)
    CHECK(uuid_str.length() == 36);
    CHECK(uuid_str[8] == '-');
    CHECK(uuid_str[13] == '-');
    CHECK(uuid_str[18] == '-');
    CHECK(uuid_str[23] == '-');
}

TEST_CASE("uuid_v7_timestamp_extraction", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;
    auto uuid = generator();

    BOOST_LOG_SEV(lg, info) << "UUID: " << uuid;

    // Extract the 48-bit timestamp from the first 6 bytes
    uint64_t timestamp =
        (static_cast<uint64_t>(uuid.data[0]) << 40) |
        (static_cast<uint64_t>(uuid.data[1]) << 32) |
        (static_cast<uint64_t>(uuid.data[2]) << 24) |
        (static_cast<uint64_t>(uuid.data[3]) << 16) |
        (static_cast<uint64_t>(uuid.data[4]) << 8) |
        (static_cast<uint64_t>(uuid.data[5]));

    BOOST_LOG_SEV(lg, info) << "Extracted timestamp: " << timestamp << " ms";

    // Get current time for comparison
    auto now = std::chrono::system_clock::now();
    auto now_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        now.time_since_epoch()).count();

    BOOST_LOG_SEV(lg, info) << "Current timestamp: " << now_ms << " ms";

    // The extracted timestamp should be very close to current time
    // Allow for up to 1 second difference to account for test execution time
    const uint64_t tolerance_ms = 1000;
    CHECK(std::abs(static_cast<int64_t>(timestamp - now_ms)) <=
          static_cast<int64_t>(tolerance_ms));
}

TEST_CASE("generate_batch_of_time_ordered_uuids", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;
    std::vector<boost::uuids::uuid> uuids;

    // Generate UUIDs with guaranteed different timestamps
    // UUID v7 only guarantees ordering across different timestamps,
    // not within the same millisecond where random bits determine order
    const size_t count = 20;
    for (size_t i = 0; i < count; ++i) {
        uuids.push_back(generator());
        // Ensure each UUID has a different timestamp (2ms is safe margin)
        std::this_thread::sleep_for(std::chrono::milliseconds(2));
    }

    BOOST_LOG_SEV(lg, info) << "Generated " << count << " UUIDs with 2ms delays";
    BOOST_LOG_SEV(lg, info) << "First UUID: " << uuids.front();
    BOOST_LOG_SEV(lg, info) << "Last UUID: " << uuids.back();

    // Verify that UUIDs are in strictly ascending order
    // Since timestamps differ, later UUIDs must be greater
    for (size_t i = 1; i < uuids.size(); ++i) {
        CHECK(uuids[i - 1] < uuids[i]);
    }
}

TEST_CASE("uuid_v7_format_components", tags) {
    auto lg(make_logger(test_suite));

    uuid_v7_generator generator;
    auto uuid = generator();

    BOOST_LOG_SEV(lg, info) << "UUID: " << uuid;

    // Log individual components for verification
    BOOST_LOG_SEV(lg, info) << "Timestamp bytes [0-5]: "
                            << std::hex << std::setfill('0')
                            << std::setw(2) << static_cast<int>(uuid.data[0]) << " "
                            << std::setw(2) << static_cast<int>(uuid.data[1]) << " "
                            << std::setw(2) << static_cast<int>(uuid.data[2]) << " "
                            << std::setw(2) << static_cast<int>(uuid.data[3]) << " "
                            << std::setw(2) << static_cast<int>(uuid.data[4]) << " "
                            << std::setw(2) << static_cast<int>(uuid.data[5]);

    BOOST_LOG_SEV(lg, info) << "Version byte [6]: 0x"
                            << std::setw(2) << static_cast<int>(uuid.data[6])
                            << " (should be 0x7X)";

    BOOST_LOG_SEV(lg, info) << "Variant byte [8]: 0x"
                            << std::setw(2) << static_cast<int>(uuid.data[8])
                            << " (should be 0x8X or 0x9X or 0xAX or 0xBX)";

    // Version byte should have 0x7 in upper nibble
    CHECK((uuid.data[6] & 0xF0) == 0x70);

    // Variant byte should have 0b10 in upper 2 bits (0x80, 0x90, 0xA0, or 0xB0)
    CHECK((uuid.data[8] & 0xC0) == 0x80);
}
