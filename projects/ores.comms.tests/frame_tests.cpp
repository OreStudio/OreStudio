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

#include <boost/test/unit_test.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include "ores.utility/test/logging.hpp"
#include "ores.utility/streaming/std_optional.hpp" // IWYU pragma: keep
#include "ores.utility/streaming/std_vector.hpp" // IWYU pragma: keep
#include "ores.comms/protocol/frame.hpp"
#include "ores.comms/protocol/message_types.hpp"
#include <vector>
#include <span>
#include <cstdint>

namespace {

const std::string test_module("ores.comms.tests");
const std::string test_suite("frame_tests");

}

BOOST_AUTO_TEST_SUITE(frame_tests)

BOOST_AUTO_TEST_CASE(test_frame_serialization) {
    SETUP_TEST_LOG_SOURCE_DEBUG("test_frame_serialization")

    // Create a frame with some test data
    std::vector<uint8_t> payload = {0x01, 0x02, 0x03, 0x04};
    ores::comms::protocol::frame frame(
        ores::comms::protocol::message_type::handshake_request,
        123, // sequence number
        payload
    );

    // Serialize the frame
    auto serialized = frame.serialize();

    // Verify that we got some data
    BOOST_REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = ores::comms::protocol::frame::deserialize(
        std::span<const uint8_t>(serialized.data(), serialized.size())
    );

    BOOST_REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    // Verify that the deserialized frame matches the original
    // Compare underlying integer values to avoid Boost.Test printing issues
    BOOST_CHECK_EQUAL(static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(frame.header().type),
        static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(deserialized_frame.header().type));
    BOOST_CHECK_EQUAL(frame.header().sequence, deserialized_frame.header().sequence);
    BOOST_CHECK_EQUAL(frame.header().payload_size, deserialized_frame.header().payload_size);
    BOOST_CHECK_EQUAL(frame.payload().size(), deserialized_frame.payload().size());
    BOOST_CHECK_EQUAL_COLLECTIONS(
        frame.payload().begin(), frame.payload().end(),
        deserialized_frame.payload().begin(), deserialized_frame.payload().end()
    );
}

BOOST_AUTO_TEST_CASE(test_frame_serialization_empty_payload) {
    SETUP_TEST_LOG_SOURCE_DEBUG("test_frame_serialization_empty_payload")

    // Create a frame with empty payload
    std::vector<uint8_t> empty_payload = {};
    ores::comms::protocol::frame frame(
        ores::comms::protocol::message_type::handshake_response,
        456, // sequence number
        empty_payload
    );

    // Serialize the frame
    auto serialized = frame.serialize();

    // Verify that we got some data (at least the header)
    BOOST_REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = ores::comms::protocol::frame::deserialize(
        std::span<const uint8_t>(serialized.data(), serialized.size())
    );

    BOOST_REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    // Verify that the deserialized frame matches the original
    BOOST_CHECK_EQUAL(static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(frame.header().type),
        static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(deserialized_frame.header().type));
    BOOST_CHECK_EQUAL(frame.header().sequence, deserialized_frame.header().sequence);
    BOOST_CHECK_EQUAL(frame.header().payload_size, deserialized_frame.header().payload_size);
    BOOST_CHECK_EQUAL(frame.payload().size(), deserialized_frame.payload().size());
    BOOST_CHECK_EQUAL(deserialized_frame.payload().size(), 0);
}

BOOST_AUTO_TEST_CASE(test_frame_serialization_large_payload) {
    SETUP_TEST_LOG_SOURCE_DEBUG("test_frame_serialization_large_payload")

    // Create a frame with a larger payload
    std::vector<uint8_t> large_payload(1000);
    for (size_t i = 0; i < large_payload.size(); ++i) {
        large_payload[i] = static_cast<uint8_t>(i % 256);
    }

    ores::comms::protocol::frame frame(
        ores::comms::protocol::message_type::handshake_ack,
        789, // sequence number
        large_payload
    );

    // Serialize the frame
    auto serialized = frame.serialize();

    // Verify that we got some data
    BOOST_REQUIRE(!serialized.empty());

    // Deserialize it back
    auto deserialized_result = ores::comms::protocol::frame::deserialize(
        std::span<const uint8_t>(serialized.data(), serialized.size())
    );

    BOOST_REQUIRE(deserialized_result.has_value());

    auto deserialized_frame = deserialized_result.value();

    // Verify that the deserialized frame matches the original
    BOOST_CHECK_EQUAL(static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(frame.header().type),
        static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(deserialized_frame.header().type));
    BOOST_CHECK_EQUAL(frame.header().sequence, deserialized_frame.header().sequence);
    BOOST_CHECK_EQUAL(frame.header().payload_size, deserialized_frame.header().payload_size);
    BOOST_CHECK_EQUAL(frame.payload().size(), deserialized_frame.payload().size());
    BOOST_CHECK_EQUAL_COLLECTIONS(
        frame.payload().begin(), frame.payload().end(),
        deserialized_frame.payload().begin(), deserialized_frame.payload().end()
    );
}

BOOST_AUTO_TEST_CASE(test_frame_deserialization_invalid_data) {
    SETUP_TEST_LOG_SOURCE_DEBUG("test_frame_deserialization_invalid_data")

    // Try to deserialize invalid data (too short)
    std::vector<uint8_t> invalid_data = {0x01, 0x02};
    auto result = ores::comms::protocol::frame::deserialize(
        std::span<const uint8_t>(invalid_data.data(), invalid_data.size())
    );

    // Should fail with an error
    BOOST_CHECK(!result.has_value());
    if (!result.has_value()) {
        BOOST_CHECK(result.error() == ores::comms::protocol::error_code::invalid_message_type);
    }
}

BOOST_AUTO_TEST_CASE(test_frame_deserialization_corrupted_data) {
    SETUP_TEST_LOG_SOURCE_DEBUG("test_frame_deserialization_corrupted_data")

    // Create a valid frame and serialize it
    std::vector<uint8_t> payload = {0x01, 0x02, 0x03, 0x04};
    ores::comms::protocol::frame frame(
        ores::comms::protocol::message_type::handshake_request,
        101, // sequence number
        payload
    );

    auto serialized = frame.serialize();

    // Corrupt the magic number in the serialized data
    if (serialized.size() > 0) {
        serialized[0] ^= 0xFF; // Flip some bits to corrupt the data

        auto result = ores::comms::protocol::frame::deserialize(
            std::span<const uint8_t>(serialized.data(), serialized.size())
        );

        // Should fail with an error
        BOOST_CHECK(!result.has_value());
        if (!result.has_value()) {
            BOOST_CHECK(result.error() == ores::comms::protocol::error_code::invalid_message_type);
        }
    }
}

BOOST_AUTO_TEST_CASE(test_frame_roundtrip_multiple_message_types) {
    SETUP_TEST_LOG_SOURCE_DEBUG("test_frame_roundtrip_multiple_message_types")

    // Test serialization/deserialization with different message types
    std::vector<uint8_t> payload = {0xDE, 0xAD, 0xBE, 0xEF};

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
        BOOST_REQUIRE(!serialized.empty());

        // Deserialize
        auto deserialized_result = ores::comms::protocol::frame::deserialize(
            std::span<const uint8_t>(serialized.data(), serialized.size())
        );

        BOOST_REQUIRE(deserialized_result.has_value());

        auto deserialized_frame = deserialized_result.value();

        // Verify the roundtrip worked correctly
        BOOST_CHECK_EQUAL(static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(original_frame.header().type),
            static_cast<std::underlying_type_t<ores::comms::protocol::message_type>>(deserialized_frame.header().type));
        BOOST_CHECK_EQUAL(original_frame.header().sequence, deserialized_frame.header().sequence);
        BOOST_CHECK_EQUAL(original_frame.header().payload_size, deserialized_frame.header().payload_size);
        BOOST_CHECK_EQUAL_COLLECTIONS(
            original_frame.payload().begin(), original_frame.payload().end(),
            deserialized_frame.payload().begin(), deserialized_frame.payload().end()
        );
    }
}

BOOST_AUTO_TEST_SUITE_END()
