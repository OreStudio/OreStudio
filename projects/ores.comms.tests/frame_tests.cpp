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
#include <span>
#include <vector>
#include <cstdint>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include "ores.utility/streaming/std_optional.hpp" // IWYU pragma: keep
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep
#include "ores.comms/protocol/frame.hpp"
#include "ores.comms/protocol/message_types.hpp"

namespace {

// Helper function to deserialize a complete frame (header + payload)
std::expected<ores::comms::protocol::frame, ores::comms::protocol::error_code>
deserialize_frame(std::span<const std::uint8_t> data) {
    // First deserialize the header
    auto header_result = ores::comms::protocol::frame::deserialize_header(data);
    if (!header_result) {
        return std::unexpected(header_result.error());
    }

    // Then deserialize the complete frame
    return ores::comms::protocol::frame::deserialize(*header_result, data);
}

}

TEST_CASE("test_frame_serialization", "[frame_tests]") {
    // Create a frame with some test data
    std::vector<std::uint8_t> payload = {0x01, 0x02, 0x03, 0x04};
    ores::comms::protocol::frame frame(
        ores::comms::protocol::message_type::handshake_request,
        123, // sequence number
        payload
    );

    // Serialize the frame
    auto serialized = frame.serialize();

    // Verify that we got some data
    REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = deserialize_frame(
        std::span<const std::uint8_t>(serialized.data(), serialized.size())
    );

    REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    // Verify that the deserialized frame matches the original
    // Compare underlying integer values to avoid printing issues
    CHECK(static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(frame.header().type) ==
        static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(deserialized_frame.header().type));
    CHECK(frame.header().sequence == deserialized_frame.header().sequence);
    CHECK(frame.header().payload_size == deserialized_frame.header().payload_size);
    CHECK(frame.payload().size() == deserialized_frame.payload().size());
    CHECK(std::equal(frame.payload().begin(), frame.payload().end(),
                     deserialized_frame.payload().begin(), deserialized_frame.payload().end()));
}

TEST_CASE("test_frame_serialization_empty_payload", "[frame_tests]") {
    // Create a frame with empty payload
    std::vector<std::uint8_t> empty_payload = {};
    ores::comms::protocol::frame frame(
        ores::comms::protocol::message_type::handshake_response,
        456, // sequence number
        empty_payload
    );

    // Serialize the frame
    auto serialized = frame.serialize();

    // Verify that we got some data (at least the header)
    REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = deserialize_frame(
        std::span<const std::uint8_t>(serialized.data(), serialized.size())
    );

    REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    // Verify that the deserialized frame matches the original
    CHECK(static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(frame.header().type) ==
        static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(deserialized_frame.header().type));
    CHECK(frame.header().sequence == deserialized_frame.header().sequence);
    CHECK(frame.header().payload_size == deserialized_frame.header().payload_size);
    CHECK(frame.payload().size() == deserialized_frame.payload().size());
    CHECK(deserialized_frame.payload().size() == 0);
}

TEST_CASE("test_frame_serialization_large_payload", "[frame_tests]") {
    // Create a frame with a larger payload
    std::vector<std::uint8_t> large_payload(1000);
    for (size_t i = 0; i < large_payload.size(); ++i) {
        large_payload[i] = static_cast<std::uint8_t>(i % 256);
    }

    ores::comms::protocol::frame frame(
        ores::comms::protocol::message_type::handshake_ack,
        789, // sequence number
        large_payload
    );

    // Serialize the frame
    auto serialized = frame.serialize();

    // Verify that we got some data
    REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = deserialize_frame(
        std::span<const std::uint8_t>(serialized.data(), serialized.size())
    );

    REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    // Verify that the deserialized frame matches the original
    CHECK(static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(frame.header().type) ==
        static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(deserialized_frame.header().type));
    CHECK(frame.header().sequence == deserialized_frame.header().sequence);
    CHECK(frame.header().payload_size == deserialized_frame.header().payload_size);
    CHECK(frame.payload().size() == deserialized_frame.payload().size());
    CHECK(std::equal(frame.payload().begin(), frame.payload().end(),
                     deserialized_frame.payload().begin(), deserialized_frame.payload().end()));
}

TEST_CASE("test_frame_deserialization_invalid_data", "[frame_tests]") {
    // Try to deserialize invalid data (too short)
    std::vector<std::uint8_t> invalid_data = {0x01, 0x02};
    auto result = deserialize_frame(
        std::span<const std::uint8_t>(invalid_data.data(), invalid_data.size())
    );

    // Should fail with an error
    CHECK(!result.has_value());
    if (!result.has_value()) {
        CHECK(result.error() == ores::comms::protocol::error_code::invalid_message_type);
    }
}

TEST_CASE("test_frame_deserialization_corrupted_data", "[frame_tests]") {
    // Create a valid frame and serialize it
    std::vector<std::uint8_t> payload = {0x01, 0x02, 0x03, 0x04};
    ores::comms::protocol::frame frame(
        ores::comms::protocol::message_type::handshake_request,
        101, // sequence number
        payload
    );

    auto serialized = frame.serialize();

    // Corrupt the magic number in the serialized data
    if (serialized.size() > 0) {
        serialized[0] ^= 0xFF; // Flip some bits to corrupt the data

        auto result = deserialize_frame(
            std::span<const std::uint8_t>(serialized.data(), serialized.size())
        );

        // Should fail with an error
        CHECK(!result.has_value());
        if (!result.has_value()) {
            CHECK(result.error() == ores::comms::protocol::error_code::invalid_message_type);
        }
    }
}

TEST_CASE("test_frame_roundtrip_multiple_message_types", "[frame_tests]") {
    // Test serialization/deserialization with different message types
    std::vector<std::uint8_t> payload = {0xDE, 0xAD, 0xBE, 0xEF};

    std::vector<ores::comms::protocol::message_type> message_types = {
        ores::comms::protocol::message_type::handshake_request,
        ores::comms::protocol::message_type::handshake_response,
        ores::comms::protocol::message_type::handshake_ack,
        ores::comms::protocol::message_type::error_response
    };

    for (auto msg_type : message_types) {
        ores::comms::protocol::frame original_frame(msg_type, 999, payload);

        // Serialize
        auto serialized = original_frame.serialize();
        REQUIRE(!serialized.empty());

        // Deserialize
        auto deserialized_result = deserialize_frame(
            std::span<const std::uint8_t>(serialized.data(), serialized.size())
        );

        REQUIRE(deserialized_result.has_value());

        auto deserialized_frame = deserialized_result.value();

        // Verify the roundtrip worked correctly
        CHECK(static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(original_frame.header().type) ==
            static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(deserialized_frame.header().type));
        CHECK(original_frame.header().sequence == deserialized_frame.header().sequence);
        CHECK(original_frame.header().payload_size == deserialized_frame.header().payload_size);
        CHECK(std::equal(original_frame.payload().begin(), original_frame.payload().end(),
                         deserialized_frame.payload().begin(), deserialized_frame.payload().end()));
    }
}
