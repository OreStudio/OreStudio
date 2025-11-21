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
#include "ores.comms/protocol/frame.hpp"

#include <span>
#include <cstdint>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.utility/streaming/std_optional.hpp" // IWYU pragma: keep
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep
#include "ores.comms/protocol/message_types.hpp"
#include <boost/asio/prefer.hpp>

namespace {

const std::string test_suite("ores.comms.tests");
const std::string tags("[messaging]");

int message_type_as_int(ores::comms::protocol::message_type mt) {
    return static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(mt);
}

// Helper function to deserialize a complete frame (header + payload)
std::expected<ores::comms::protocol::frame, ores::comms::protocol::error_code>
deserialize_frame(std::span<const std::byte> data) {
    // First deserialize the header
    auto header_result = ores::comms::protocol::frame::deserialize_header(data);
    if (!header_result) {
        return std::unexpected(header_result.error());
    }

    // Then deserialize the complete frame
    return ores::comms::protocol::frame::deserialize(*header_result, data);
}

}

using namespace ores::utility::log;
using ores::comms::protocol::message_type;

TEST_CASE("test_frame_serialization", tags) {
    auto lg(make_logger(test_suite));

    // Create a frame with some test data
    std::vector<std::byte> payload = {
        std::byte{0x01},
        std::byte{0x02},
        std::byte{0x03},
        std::byte{0x04}
    };
    ores::comms::protocol::frame frame(
        message_type::handshake_request,
        123, // sequence number
        payload
    );

    BOOST_LOG_SEV(lg, debug) << "Created frame with type: handshake_request, "
                             << "sequence: 123, payload size: " << payload.size();

    // Serialize the frame
    auto serialized = frame.serialize();

    BOOST_LOG_SEV(lg, debug) << "Serialized frame size: " << serialized.size();

    // Verify that we got some data
    REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = deserialize_frame(
        std::span<const std::byte>(serialized.data(), serialized.size())
    );

    REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    BOOST_LOG_SEV(lg, debug) << "Deserialized frame - sequence: "
                             << deserialized_frame.header().sequence
                             << ", payload size: "
                             << deserialized_frame.payload().size();

    // Verify that the deserialized frame matches the original
    // Compare underlying integer values to avoid printing issues
    CHECK(message_type_as_int(frame.header().type) ==
        message_type_as_int(deserialized_frame.header().type));
    CHECK(frame.header().sequence == deserialized_frame.header().sequence);
    CHECK(frame.header().payload_size == deserialized_frame.header().payload_size);
    CHECK(frame.payload().size() == deserialized_frame.payload().size());
    CHECK(std::equal(frame.payload().begin(), frame.payload().end(),
                     deserialized_frame.payload().begin(), deserialized_frame.payload().end()));

    BOOST_LOG_SEV(lg, debug) << "Frame serialization roundtrip successful";
}

TEST_CASE("test_frame_serialization_empty_payload", tags) {
    auto lg(make_logger(test_suite));

    // Create a frame with empty payload
    std::vector<std::byte> empty_payload = {};
    ores::comms::protocol::frame frame(
        message_type::handshake_response,
        456, // sequence number
        empty_payload
    );

    BOOST_LOG_SEV(lg, debug) << "Created frame with empty payload - type: handshake_response, sequence: 456";

    // Serialize the frame
    auto serialized = frame.serialize();

    BOOST_LOG_SEV(lg, debug) << "Serialized frame size (header only): " << serialized.size();

    // Verify that we got some data (at least the header)
    REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = deserialize_frame(
        std::span<const std::byte>(serialized.data(), serialized.size())
    );

    REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    BOOST_LOG_SEV(lg, debug) << "Deserialized empty payload frame successfully";

    // Verify that the deserialized frame matches the original
    CHECK(message_type_as_int(frame.header().type) ==
        message_type_as_int(deserialized_frame.header().type));
    CHECK(frame.header().sequence == deserialized_frame.header().sequence);
    CHECK(frame.header().payload_size == deserialized_frame.header().payload_size);
    CHECK(frame.payload().size() == deserialized_frame.payload().size());
    CHECK(deserialized_frame.payload().size() == 0);
}

TEST_CASE("test_frame_serialization_large_payload", tags) {
    auto lg(make_logger(test_suite));

    // Create a frame with a larger payload
    std::vector<std::byte> large_payload(1000);
    for (size_t i = 0; i < large_payload.size(); ++i) {
        large_payload[i] = static_cast<std::byte>(i % 256);
    }

    BOOST_LOG_SEV(lg, debug) << "Created large payload of size: " << large_payload.size();

    ores::comms::protocol::frame frame(
        message_type::handshake_ack,
        789, // sequence number
        large_payload
    );

    BOOST_LOG_SEV(lg, debug) << "Created frame with type: handshake_ack, sequence: 789";

    // Serialize the frame
    auto serialized = frame.serialize();

    BOOST_LOG_SEV(lg, debug) << "Serialized large frame size: " << serialized.size();

    // Verify that we got some data
    REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = deserialize_frame(
        std::span<const std::byte>(serialized.data(), serialized.size())
    );

    REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    BOOST_LOG_SEV(lg, debug) << "Deserialized large frame - payload size: "
                            << deserialized_frame.payload().size();

    // Verify that the deserialized frame matches the original
    CHECK(message_type_as_int(frame.header().type) ==
        message_type_as_int(deserialized_frame.header().type));
    CHECK(frame.header().sequence == deserialized_frame.header().sequence);
    CHECK(frame.header().payload_size == deserialized_frame.header().payload_size);
    CHECK(frame.payload().size() == deserialized_frame.payload().size());
    CHECK(std::equal(frame.payload().begin(), frame.payload().end(),
                     deserialized_frame.payload().begin(), deserialized_frame.payload().end()));

    BOOST_LOG_SEV(lg, debug) << "Large frame roundtrip successful";
}

TEST_CASE("test_frame_deserialization_invalid_data", tags) {
    auto lg(make_logger(test_suite));

    // Try to deserialize invalid data (too short)
    std::vector<std::byte> invalid_data = { std::byte{0x01}, std::byte{0x02} };

    BOOST_LOG_SEV(lg, debug) << "Attempting to deserialize invalid data of size: "
                            << invalid_data.size();

    auto result = deserialize_frame(
        std::span<const std::byte>(invalid_data.data(), invalid_data.size())
    );

    // Should fail with an error
    CHECK(!result.has_value());
    if (!result.has_value()) {
        BOOST_LOG_SEV(lg, debug) << "Deserialization failed as expected with error code: invalid_message_type";
        CHECK(result.error() == ores::comms::protocol::error_code::invalid_message_type);
    }
}

TEST_CASE("test_frame_deserialization_corrupted_data", tags) {
    auto lg(make_logger(test_suite));

    // Create a valid frame and serialize it
    std::vector<std::byte> payload = {
        std::byte{0x01},
        std::byte{0x02},
        std::byte{0x03},
        std::byte{0x04}
    };
    ores::comms::protocol::frame frame(
        message_type::handshake_request,
        101, // sequence number
        payload
    );

    BOOST_LOG_SEV(lg, debug) << "Created frame for corruption test";

    auto serialized = frame.serialize();

    BOOST_LOG_SEV(lg, debug) << "Serialized frame size: " << serialized.size();

    // Corrupt the magic number in the serialized data
    if (serialized.size() > 0) {
        serialized[0] ^= std::byte(0xFF); // Flip some bits to corrupt the data

        BOOST_LOG_SEV(lg, debug) << "Corrupted first byte of serialized data";

        auto result = deserialize_frame(
            std::span<const std::byte>(serialized.data(), serialized.size())
        );

        // Should fail with an error
        CHECK(!result.has_value());
        if (!result.has_value()) {
            BOOST_LOG_SEV(lg, debug) << "Deserialization of corrupted data failed as expected";
            CHECK(result.error() == ores::comms::protocol::error_code::invalid_message_type);
        }
    }
}

TEST_CASE("test_frame_roundtrip_multiple_message_types", tags) {
    auto lg(make_logger(test_suite));

    // Test serialization/deserialization with different message types
    std::vector<std::byte> payload = {
        std::byte{0xDE},
        std::byte{0xAD},
        std::byte{0xBE},
        std::byte{0xEF}
    };

    std::vector<message_type> message_types = {
        message_type::handshake_request,
        message_type::handshake_response,
        message_type::handshake_ack,
        message_type::error_response
    };

    BOOST_LOG_SEV(lg, debug) << "Testing roundtrip for " << message_types.size()
                            << " different message types";

    for (auto msg_type : message_types) {
        ores::comms::protocol::frame original_frame(msg_type, 999, payload);

        BOOST_LOG_SEV(lg, debug) << "Testing message type: "
                                << static_cast<int>(msg_type);

        // Serialize
        auto serialized = original_frame.serialize();
        REQUIRE(!serialized.empty());

        BOOST_LOG_SEV(lg, debug) << "Serialized size: " << serialized.size();

        // Deserialize
        auto deserialized_result = deserialize_frame(
            std::span<const std::byte>(serialized.data(), serialized.size())
        );

        REQUIRE(deserialized_result.has_value());

        auto deserialized_frame = deserialized_result.value();

        // Verify the roundtrip worked correctly
        CHECK(message_type_as_int(original_frame.header().type) ==
            message_type_as_int(deserialized_frame.header().type));
        CHECK(original_frame.header().sequence == deserialized_frame.header().sequence);
        CHECK(original_frame.header().payload_size == deserialized_frame.header().payload_size);
        CHECK(std::equal(original_frame.payload().begin(), original_frame.payload().end(),
                         deserialized_frame.payload().begin(), deserialized_frame.payload().end()));

        BOOST_LOG_SEV(lg, debug) << "Roundtrip successful for message type: "
                                << static_cast<int>(msg_type);
    }

    BOOST_LOG_SEV(lg, debug) << "All message types tested successfully";
}

TEST_CASE("test_frame_version_mismatch_strict_mode", tags) {
    auto lg(make_logger(test_suite));

    // Create a frame with current protocol version
    std::vector<std::byte> payload = {
        std::byte{0x01},
        std::byte{0x02},
        std::byte{0x03}
    };

    ores::comms::protocol::frame frame(
        message_type::handshake_request,
        1,
        payload
    );

    // Serialize it
    auto serialized = frame.serialize();
    REQUIRE(!serialized.empty());

    BOOST_LOG_SEV(lg, debug) << "Created frame with current protocol version";

    // Corrupt the version in the serialized header
    // Version major is at offset 4-5 (after 4-byte magic)
    // Let's change it to something different
    std::uint16_t wrong_version = 99;
    serialized[4] = static_cast<std::byte>((wrong_version >> 8) & 0xFF);
    serialized[5] = static_cast<std::byte>(wrong_version & 0xFF);

    BOOST_LOG_SEV(lg, debug) << "Modified version to " << wrong_version;

    // Try to deserialize with strict version checking (default)
    auto header_result = ores::comms::protocol::frame::deserialize_header(
        std::span<const std::byte>(serialized));

    // Should fail with version_mismatch error
    REQUIRE(!header_result.has_value());
    CHECK(header_result.error() == ores::comms::protocol::error_code::version_mismatch);

    BOOST_LOG_SEV(lg, debug) << "Version mismatch correctly detected in strict mode";
}

TEST_CASE("test_frame_version_mismatch_lenient_mode", tags) {
    auto lg(make_logger(test_suite));

    // Create a frame with current protocol version
    std::vector<std::byte> payload = {
        std::byte{0xAA},
        std::byte{0xBB},
        std::byte{0xCC}
    };

    ores::comms::protocol::frame frame(
        message_type::handshake_request,
        42,
        payload
    );

    // Serialize it
    auto serialized = frame.serialize();
    REQUIRE(!serialized.empty());

    BOOST_LOG_SEV(lg, debug) << "Created frame with current protocol version";

    // Corrupt the version in the serialized header
    // Version major is at offset 4-5 (after 4-byte magic)
    std::uint16_t wrong_version = 1; // Different from current version
    serialized[4] = static_cast<std::byte>((wrong_version >> 8) & 0xFF);
    serialized[5] = static_cast<std::byte>(wrong_version & 0xFF);

    BOOST_LOG_SEV(lg, debug) << "Modified version to " << wrong_version;

    // Try to deserialize with lenient version checking (skip_version_check=true)
    auto header_result = ores::comms::protocol::frame::deserialize_header(
        std::span<const std::byte>(serialized), true);

    // Should succeed even with mismatched version
    REQUIRE(header_result.has_value());
    CHECK(header_result->version_major == wrong_version);
    CHECK(header_result->type == message_type::handshake_request);
    CHECK(header_result->sequence == 42);

    BOOST_LOG_SEV(lg, debug) << "Successfully read header with mismatched version in lenient mode";

    // Now deserialize the complete frame using the header we just read
    // Note: We need to fix the CRC since we modified the version
    // For this test, let's just verify the header was read correctly
}

TEST_CASE("test_frame_version_mismatch_handshake_scenario", tags) {
    auto lg(make_logger(test_suite));

    // This simulates the handshake scenario:
    // 1. Client with v1 sends handshake_request
    // 2. Server with v2 needs to read it (lenient mode)
    // 3. Server sends handshake_response with version info

    // Create a handshake request from a "v1 client"
    std::vector<std::byte> payload = {
        std::byte{0x11},
        std::byte{0x22}
    };

    ores::comms::protocol::frame client_frame(
        message_type::handshake_request,
        1,
        payload
    );

    auto serialized = client_frame.serialize();
    REQUIRE(!serialized.empty());

    BOOST_LOG_SEV(lg, debug) << "Created handshake request from v1 client";

    // Modify version to simulate v1 (assuming current is v2)
    std::uint16_t client_version = 1;
    serialized[4] = static_cast<std::byte>((client_version >> 8) & 0xFF);
    serialized[5] = static_cast<std::byte>(client_version & 0xFF);

    BOOST_LOG_SEV(lg, debug) << "Modified to client version " << client_version;

    // Server tries to read with strict mode - should fail
    auto strict_result = ores::comms::protocol::frame::deserialize_header(
        std::span<const std::byte>(serialized), false);

    CHECK(!strict_result.has_value());
    if (!strict_result.has_value()) {
        BOOST_LOG_SEV(lg, debug) << "Strict mode correctly rejected mismatched version";
    }

    // Server tries to read with lenient mode - should succeed
    auto lenient_result = ores::comms::protocol::frame::deserialize_header(
        std::span<const std::byte>(serialized), true);

    REQUIRE(lenient_result.has_value());
    CHECK(lenient_result->version_major == client_version);
    CHECK(lenient_result->type == message_type::handshake_request);

    BOOST_LOG_SEV(lg, debug) << "Lenient mode successfully read handshake from v"
                            << client_version << " client";
    BOOST_LOG_SEV(lg, debug) << "Server can now send handshake_response with version details";
}
