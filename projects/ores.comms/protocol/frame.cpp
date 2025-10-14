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
#include "ores.comms/protocol/crc.hpp"
#include <rfl.hpp>
#include <rfl/bson.hpp>
#include "ores.utility/log/logger.hpp"

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.comms.protocol.frame"));

}

namespace ores::comms::protocol {

frame::frame() : header_{}, payload_{} {
    header_.magic = PROTOCOL_MAGIC;
    header_.version_major = PROTOCOL_VERSION_MAJOR;
    header_.version_minor = PROTOCOL_VERSION_MINOR;
    header_.type = message_type::handshake_request;
    header_.reserved1 = 0;
    header_.payload_size = 0;
    header_.sequence = 0;
    header_.crc = 0;
    header_.reserved2.fill(0);
}

frame::frame(message_type type, uint32_t sequence, std::vector<uint8_t> payload)
    : header_{}, payload_(std::move(payload)) {
    header_.magic = PROTOCOL_MAGIC;
    header_.version_major = PROTOCOL_VERSION_MAJOR;
    header_.version_minor = PROTOCOL_VERSION_MINOR;
    header_.type = type;
    header_.reserved1 = 0;
    header_.payload_size = static_cast<uint32_t>(payload_.size());
    header_.sequence = sequence;
    header_.crc = 0; // Will be calculated during serialization
    header_.reserved2.fill(0);
}

uint32_t frame::calculate_crc() const {
    crc32 calc;

    // Create temporary header with CRC set to 0
    frame_header temp_header = header_;
    temp_header.crc = 0;

    // Serialize header using reflect-cpp BSON and update CRC
    auto header_bytes = rfl::bson::write(temp_header);
    // Convert char vector to uint8_t span
    calc.update(std::span<const uint8_t>(
        reinterpret_cast<const uint8_t*>(header_bytes.data()),
        header_bytes.size()));

    // Update CRC with payload
    if (!payload_.empty()) {
        calc.update(std::span<const uint8_t>(payload_.data(), payload_.size()));
    }

    return calc.finalize();
}

std::vector<uint8_t> frame::serialize() const {
    // Calculate CRC before serialization
    frame_header header_with_crc = header_;
    header_with_crc.crc = calculate_crc();

    // Serialize header using reflect-cpp BSON
    auto header_bytes = rfl::bson::write(header_with_crc);
    BOOST_LOG_SEV(lg, debug) << "Serialized header to " << header_bytes.size() << " bytes";

    // Combine header and payload
    std::vector<uint8_t> result;
    result.reserve(header_bytes.size() + payload_.size());
    result.insert(result.end(),
                  reinterpret_cast<const uint8_t*>(header_bytes.data()),
                  reinterpret_cast<const uint8_t*>(header_bytes.data()) + header_bytes.size());
    result.insert(result.end(), payload_.begin(), payload_.end());

    BOOST_LOG_SEV(lg, debug) << "Total serialized frame size: " << result.size() 
                              << " (header: " << header_bytes.size() << ", payload: " << payload_.size() << ")";
    BOOST_LOG_SEV(lg, debug) << "Header magic: 0x" << std::hex << header_with_crc.magic << std::dec
                              << ", type: " << static_cast<int>(header_with_crc.type)
                              << ", payload_size: " << header_with_crc.payload_size;

    return result;
}

std::expected<frame, error_code> frame::deserialize(std::span<const uint8_t> data) {
    BOOST_LOG_SEV(lg, debug) << "Starting frame deserialization, input size: " << data.size();
    
    // BSON documents start with a 32-bit little-endian length
    if (data.size() < 4) {
        BOOST_LOG_SEV(lg, error) << "Data too short for BSON length, size: " << data.size();
        return std::unexpected(error_code::invalid_message_type);
    }

    // Read the BSON document length (first 4 bytes, little-endian)
    uint32_t bson_length = 0;
    bson_length |= static_cast<uint32_t>(data[0]);
    bson_length |= static_cast<uint32_t>(data[1]) << 8;
    bson_length |= static_cast<uint32_t>(data[2]) << 16;
    bson_length |= static_cast<uint32_t>(data[3]) << 24;

    BOOST_LOG_SEV(lg, debug) << "Parsed BSON length: " << bson_length;
    
    // The length includes the 4 bytes for the length itself
    if (bson_length < 4 || data.size() < bson_length) {
        BOOST_LOG_SEV(lg, error) << "Invalid BSON length (" << bson_length << ") or insufficient data (" << data.size() << ")";
        return std::unexpected(error_code::invalid_message_type);
    }

    // Extract the header part (the BSON document)
    std::span<const uint8_t> header_span(data.data(), bson_length);
    
    BOOST_LOG_SEV(lg, debug) << "Attempting to deserialize BSON header of size: " << bson_length;
    
    // Deserialize the header - use the same signature as in serialization
    auto header_result = rfl::bson::read<frame_header>(header_span.data(), header_span.size());
    
    if (!header_result) {
        BOOST_LOG_SEV(lg, error) << "Failed to deserialize BSON header";
        return std::unexpected(error_code::invalid_message_type);
    }

    frame_header header = header_result.value();
    BOOST_LOG_SEV(lg, debug) << "Successfully deserialized header, payload_size: " << header.payload_size 
                              << ", magic: 0x" << std::hex << header.magic << std::dec
                              << ", type: " << static_cast<int>(header.type);
    
    // Check magic number
    if (header.magic != PROTOCOL_MAGIC) {
        BOOST_LOG_SEV(lg, error) << "Invalid magic number, expected: 0x" << std::hex << PROTOCOL_MAGIC 
                                  << ", got: 0x" << header.magic << std::dec;
        return std::unexpected(error_code::invalid_message_type);
    }

    // Check if we have enough data for the payload
    if (data.size() < bson_length + header.payload_size) {
        BOOST_LOG_SEV(lg, error) << "Insufficient data for payload, need: " << (bson_length + header.payload_size) 
                                  << ", have: " << data.size();
        return std::unexpected(error_code::invalid_message_type);
    }

    // Extract payload
    std::vector<uint8_t> payload;
    if (header.payload_size > 0) {
        auto payload_start = data.begin() + bson_length;
        auto payload_end = payload_start + header.payload_size;
        payload.assign(payload_start, payload_end);
        BOOST_LOG_SEV(lg, debug) << "Extracted payload of size: " << payload.size();
    }

    // Create frame and validate
    frame f;
    f.header_ = header;
    f.payload_ = std::move(payload);

    auto validation = f.validate();
    if (!validation) {
        BOOST_LOG_SEV(lg, error) << "Frame validation failed with error: " << static_cast<int>(validation.error());
        return std::unexpected(validation.error());
    }

    BOOST_LOG_SEV(lg, debug) << "Successfully deserialized frame, type: " << static_cast<int>(f.header().type);
    return f;
}

std::expected<void, error_code> frame::validate() const {
    // Check magic number
    if (header_.magic != PROTOCOL_MAGIC) {
        return std::unexpected(error_code::invalid_message_type);
    }

    // Check version compatibility (major version must match)
    if (header_.version_major != PROTOCOL_VERSION_MAJOR) {
        return std::unexpected(error_code::version_mismatch);
    }

    // Check payload size
    if (header_.payload_size != payload_.size()) {
        return std::unexpected(error_code::invalid_message_type);
    }

    // Validate CRC
    uint32_t calculated_crc = calculate_crc();
    if (header_.crc != calculated_crc) {
        return std::unexpected(error_code::crc_validation_failed);
    }

    return {};
}

}
