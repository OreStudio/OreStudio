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
#include "ores.utility/log/logger.hpp"
#include <cstring> // for memcpy
#include <algorithm> // for std::fill

namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.comms.protocol.frame"));

// Helper functions for cross-platform binary serialization
uint32_t host_to_network_32(uint32_t val) {
#ifdef __GNUC__
    return __builtin_bswap32(val);
#elif defined(_MSC_VER)
    return _byteswap_ulong(val);
#else
    return ((val & 0xFF000000) >> 24) |
           ((val & 0x00FF0000) >> 8)  |
           ((val & 0x0000FF00) << 8)  |
           ((val & 0x000000FF) << 24);
#endif
}

uint16_t host_to_network_16(uint16_t val) {
#ifdef __GNUC__
    return __builtin_bswap16(val);
#elif defined(_MSC_VER)
    return _byteswap_ushort(val);
#else
    return ((val & 0xFF00) >> 8) |
           ((val & 0x00FF) << 8);
#endif
}

uint32_t network_to_host_32(uint32_t val) {
    return host_to_network_32(val); // BSwap is symmetric
}

uint16_t network_to_host_16(uint16_t val) {
    return host_to_network_16(val); // BSwap is symmetric
}

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

    // Create temporary header with CRC set to 0 for calculation
    frame_header temp_header = header_;
    temp_header.crc = 0;

    // Serialize header to fixed 32-byte binary format for CRC calculation
    std::array<uint8_t, frame_header::size> header_bytes;
    size_t offset = 0;
    
    // Serialize each field in network byte order (for CRC calculation)
    uint32_t temp32 = host_to_network_32(temp_header.magic);
    std::memcpy(&header_bytes[offset], &temp32, sizeof(temp32));
    offset += sizeof(temp32);
    
    uint16_t temp16 = host_to_network_16(temp_header.version_major);
    std::memcpy(&header_bytes[offset], &temp16, sizeof(temp16));
    offset += sizeof(temp16);
    
    temp16 = host_to_network_16(temp_header.version_minor);
    std::memcpy(&header_bytes[offset], &temp16, sizeof(temp16));
    offset += sizeof(temp16);
    
    temp16 = host_to_network_16(static_cast<uint16_t>(temp_header.type));
    std::memcpy(&header_bytes[offset], &temp16, sizeof(temp16));
    offset += sizeof(temp16);
    
    temp16 = host_to_network_16(temp_header.reserved1);
    std::memcpy(&header_bytes[offset], &temp16, sizeof(temp16));
    offset += sizeof(temp16);
    
    temp32 = host_to_network_32(temp_header.payload_size);
    std::memcpy(&header_bytes[offset], &temp32, sizeof(temp32));
    offset += sizeof(temp32);
    
    temp32 = host_to_network_32(temp_header.sequence);
    std::memcpy(&header_bytes[offset], &temp32, sizeof(temp32));
    offset += sizeof(temp32);
    
    temp32 = host_to_network_32(0); // CRC is 0 for calculation
    std::memcpy(&header_bytes[offset], &temp32, sizeof(temp32));
    offset += sizeof(temp32);
    
    // Copy reserved2 array (8 bytes)
    std::memcpy(&header_bytes[offset], temp_header.reserved2.data(), temp_header.reserved2.size());
    offset += temp_header.reserved2.size();

    // Update CRC with the header bytes
    calc.update(std::span<const uint8_t>(header_bytes.data(), header_bytes.size()));

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

    // Serialize header to fixed 32-byte binary format
    std::array<uint8_t, frame_header::size> header_bytes;
    size_t offset = 0;
    
    // Serialize each field in network byte order
    uint32_t temp32 = host_to_network_32(header_with_crc.magic);
    std::memcpy(&header_bytes[offset], &temp32, sizeof(temp32));
    offset += sizeof(temp32);
    
    uint16_t temp16 = host_to_network_16(header_with_crc.version_major);
    std::memcpy(&header_bytes[offset], &temp16, sizeof(temp16));
    offset += sizeof(temp16);
    
    temp16 = host_to_network_16(header_with_crc.version_minor);
    std::memcpy(&header_bytes[offset], &temp16, sizeof(temp16));
    offset += sizeof(temp16);
    
    temp16 = host_to_network_16(static_cast<uint16_t>(header_with_crc.type));
    std::memcpy(&header_bytes[offset], &temp16, sizeof(temp16));
    offset += sizeof(temp16);
    
    temp16 = host_to_network_16(header_with_crc.reserved1);
    std::memcpy(&header_bytes[offset], &temp16, sizeof(temp16));
    offset += sizeof(temp16);
    
    temp32 = host_to_network_32(header_with_crc.payload_size);
    std::memcpy(&header_bytes[offset], &temp32, sizeof(temp32));
    offset += sizeof(temp32);
    
    temp32 = host_to_network_32(header_with_crc.sequence);
    std::memcpy(&header_bytes[offset], &temp32, sizeof(temp32));
    offset += sizeof(temp32);
    
    temp32 = host_to_network_32(header_with_crc.crc);
    std::memcpy(&header_bytes[offset], &temp32, sizeof(temp32));
    offset += sizeof(temp32);
    
    // Copy reserved2 array (8 bytes)
    std::memcpy(&header_bytes[offset], header_with_crc.reserved2.data(), header_with_crc.reserved2.size());
    offset += header_with_crc.reserved2.size();

    // Verify we filled exactly 32 bytes
    if (offset != frame_header::size) {
        BOOST_LOG_SEV(lg, error) << "Header serialization error: wrong size " << offset;
    }

    // Combine header and payload
    std::vector<uint8_t> result;
    result.reserve(frame_header::size + payload_.size());
    result.insert(result.end(), header_bytes.begin(), header_bytes.end());
    result.insert(result.end(), payload_.begin(), payload_.end());

    BOOST_LOG_SEV(lg, debug) << "Total serialized frame size: " << result.size() 
                              << " (header: " << frame_header::size << ", payload: " << payload_.size() << ")";
    BOOST_LOG_SEV(lg, debug) << "Header magic: 0x" << std::hex << header_with_crc.magic << std::dec
                              << ", type: " << static_cast<int>(header_with_crc.type)
                              << ", payload_size: " << header_with_crc.payload_size;

    return result;
}

std::expected<frame, error_code> frame::deserialize(std::span<const uint8_t> data) {
    BOOST_LOG_SEV(lg, debug) << "Starting frame deserialization, input size: " << data.size();
    
    // Check for minimum frame size (header only)
    if (data.size() < frame_header::size) {
        BOOST_LOG_SEV(lg, error) << "Data too short for frame header, size: " << data.size() 
                                  << ", need at least: " << frame_header::size;
        return std::unexpected(error_code::invalid_message_type);
    }

    // Deserialize the 32-byte header
    frame_header header{};
    size_t offset = 0;
    
    // Read each field in network byte order
    uint32_t temp32;
    std::memcpy(&temp32, &data[offset], sizeof(temp32));
    header.magic = network_to_host_32(temp32);
    offset += sizeof(temp32);
    
    uint16_t temp16;
    std::memcpy(&temp16, &data[offset], sizeof(temp16));
    header.version_major = network_to_host_16(temp16);
    offset += sizeof(temp16);
    
    std::memcpy(&temp16, &data[offset], sizeof(temp16));
    header.version_minor = network_to_host_16(temp16);
    offset += sizeof(temp16);
    
    std::memcpy(&temp16, &data[offset], sizeof(temp16));
    header.type = static_cast<message_type>(network_to_host_16(temp16));
    offset += sizeof(temp16);
    
    std::memcpy(&temp16, &data[offset], sizeof(temp16));
    header.reserved1 = network_to_host_16(temp16);
    offset += sizeof(temp16);
    
    std::memcpy(&temp32, &data[offset], sizeof(temp32));
    header.payload_size = network_to_host_32(temp32);
    offset += sizeof(temp32);
    
    std::memcpy(&temp32, &data[offset], sizeof(temp32));
    header.sequence = network_to_host_32(temp32);
    offset += sizeof(temp32);
    
    std::memcpy(&temp32, &data[offset], sizeof(temp32));
    header.crc = network_to_host_32(temp32);
    offset += sizeof(temp32);
    
    // Read reserved2 array (8 bytes)
    std::memcpy(header.reserved2.data(), &data[offset], header.reserved2.size());
    offset += header.reserved2.size();

    // Verify we read exactly 32 bytes for the header
    if (offset != frame_header::size) {
        BOOST_LOG_SEV(lg, error) << "Header deserialization error: read wrong size " << offset;
        return std::unexpected(error_code::invalid_message_type);
    }

    // Check magic number
    if (header.magic != PROTOCOL_MAGIC) {
        BOOST_LOG_SEV(lg, error) << "Invalid magic number, expected: 0x" << std::hex << PROTOCOL_MAGIC 
                                  << ", got: 0x" << header.magic << std::dec;
        return std::unexpected(error_code::invalid_message_type);
    }

    BOOST_LOG_SEV(lg, debug) << "Successfully parsed header, payload_size: " << header.payload_size 
                              << ", magic: 0x" << std::hex << header.magic << std::dec
                              << ", type: " << static_cast<int>(header.type);

    // Check if we have enough data for the payload
    if (data.size() < frame_header::size + header.payload_size) {
        BOOST_LOG_SEV(lg, error) << "Insufficient data for payload, need: " << (frame_header::size + header.payload_size) 
                                  << ", have: " << data.size();
        return std::unexpected(error_code::invalid_message_type);
    }

    // Extract payload
    std::vector<uint8_t> payload;
    if (header.payload_size > 0) {
        auto payload_start = data.begin() + frame_header::size;
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
