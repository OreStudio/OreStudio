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
#include <ostream>
#include <cstring>
#include <rfl.hpp>
#include <rfl/json.hpp>
#include "ores.comms/protocol/frame.hpp"
#include "ores.comms/protocol/crc.hpp"
#include "ores.utility/log/logger.hpp"


namespace {

using namespace ores::utility::log;
auto lg(logger_factory("ores.comms.protocol.frame"));

uint32_t host_to_network_32(uint32_t val) {
    if constexpr (std::endian::native == std::endian::little)
        return std::byteswap(val);
    return val;
}

uint16_t host_to_network_16(uint16_t val) {
    if constexpr (std::endian::native == std::endian::little)
        return std::byteswap(val);
    return val;
}

uint32_t network_to_host_32(uint32_t val) { return host_to_network_32(val); }
uint16_t network_to_host_16(uint16_t val) { return host_to_network_16(val); }

bool is_valid_message_type(uint16_t type) {
    using ores::comms::protocol::message_type;
    return type >= static_cast<uint16_t>(message_type::handshake_request) &&
        type <= static_cast<uint16_t>(message_type::last_value);
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

void frame::serialize_header(frame_header header, std::span<uint8_t> buffer) const {
    if (buffer.size() < frame_header::size) {
        BOOST_LOG_SEV(lg, error) << "Buffer too small for header: " << buffer.size();
        throw std::runtime_error("Invalid buffer size");
    }

    size_t offset = 0;
    auto write32 = [&](uint32_t val) {
        val = host_to_network_32(val);
        std::memcpy(buffer.data() + offset, &val, sizeof(val));
        offset += sizeof(val);
    };
    auto write16 = [&](uint16_t val) {
        val = host_to_network_16(val);
        std::memcpy(buffer.data() + offset, &val, sizeof(val));
        offset += sizeof(val);
    };

    write32(header.magic);
    write16(header.version_major);
    write16(header.version_minor);
    write16(static_cast<uint16_t>(header.type));
    write16(header.reserved1);
    write32(header.payload_size);
    write32(header.sequence);
    write32(header.crc);
    std::memcpy(buffer.data() + offset, header.reserved2.data(), header.reserved2.size());
}

uint32_t frame::calculate_crc() const {
    crc32 calc;
    std::array<uint8_t, frame_header::size> header_bytes;
    frame_header temp_header = header_;
    temp_header.crc = 0;
    serialize_header(temp_header, header_bytes);
    calc.update(header_bytes);
    if (!payload_.empty()) {
        calc.update(payload_);
    }
    return calc.finalize();
}

std::vector<uint8_t> frame::serialize() const {
    frame_header header_with_crc = header_;
    header_with_crc.crc = calculate_crc();

    std::vector<uint8_t> result;
    result.resize(frame_header::size + payload_.size());
    serialize_header(header_with_crc, result);
    if (!payload_.empty()) {
        std::memcpy(result.data() + frame_header::size, payload_.data(), payload_.size());
    }

    BOOST_LOG_SEV(lg, debug) << "Serialised frame, type: " << static_cast<int>(header_with_crc.type)
                             << ", size: " << result.size();
    return result;
}

std::expected<frame_header, error_code> frame::read_header(std::span<const uint8_t> data) {
    BOOST_LOG_SEV(lg, debug) << "Reading frame header from data of size: " << data.size();

    if (data.size() < frame_header::size) {
        BOOST_LOG_SEV(lg, error) << "Data too short for header: " << data.size();
        return std::unexpected(error_code::invalid_message_type);
    }

    frame_header header{};
    size_t offset = 0;
    auto read32 = [&]() {
        uint32_t val;
        std::memcpy(&val, data.data() + offset, sizeof(val));
        offset += sizeof(val);
        return network_to_host_32(val);
    };
    auto read16 = [&]() {
        uint16_t val;
        std::memcpy(&val, data.data() + offset, sizeof(val));
        offset += sizeof(val);
        return network_to_host_16(val);
    };

    header.magic = read32();
    header.version_major = read16();
    header.version_minor = read16();
    uint16_t raw_type = read16();
    if (!is_valid_message_type(raw_type)) {
        BOOST_LOG_SEV(lg, error) << "Invalid message type: " << raw_type;
        return std::unexpected(error_code::invalid_message_type);
    }
    header.type = static_cast<message_type>(raw_type);
    header.reserved1 = read16();
    header.payload_size = read32();
    header.sequence = read32();
    header.crc = read32();

    std::memcpy(header.reserved2.data(), data.data() + offset, header.reserved2.size());
    offset += header.reserved2.size();

    if (header.magic != PROTOCOL_MAGIC || header.reserved1 != 0 ||
        std::ranges::any_of(header.reserved2, [](uint8_t v) {
            return v != 0; })) {
        BOOST_LOG_SEV(lg, error) << "Invalid header fields";
        return std::unexpected(error_code::invalid_message_type);
    }
    if (header.payload_size > MAX_PAYLOAD_SIZE) {
        BOOST_LOG_SEV(lg, error) << "Payload size too large: " << header.payload_size;
        return std::unexpected(error_code::payload_too_large);
    }

    BOOST_LOG_SEV(lg, debug) << "Read frame header: " << header;
    return header;
}

std::expected<frame, error_code> frame::deserialize(std::span<const uint8_t> data) {
    BOOST_LOG_SEV(lg, debug) << "Received data. Size: " << data.size();

    // First, read and validate the header
    auto header_result = read_header(data);
    if (!header_result) {
        return std::unexpected(header_result.error());
    }
    const auto& header = *header_result;

    // Check we have enough data for the complete frame
    const auto expected_size = frame_header::size + header.payload_size;
    if (data.size() < expected_size) {
        BOOST_LOG_SEV(lg, error) << "Insufficient payload data. Got:"
                                 << data.size()
                                 << " Expected: " << expected_size;
        return std::unexpected(error_code::invalid_message_type);
    }

    // Build the frame with payload
    frame f;
    f.header_ = header;
    if (header.payload_size > 0) {
        f.payload_.assign(data.begin() + frame_header::size,
            data.begin() + frame_header::size + header.payload_size);
    }

    // Validate the complete frame (including CRC)
    auto validation = f.validate();
    if (!validation) {
        BOOST_LOG_SEV(lg, error) << "Validation failed: " << static_cast<int>(validation.error());
        return std::unexpected(validation.error());
    }

    return f;
}

std::expected<void, error_code> frame::validate() const {
    // Check magic number
    if (header_.magic != PROTOCOL_MAGIC) {
        BOOST_LOG_SEV(lg, warn) << "Invalid protocol magic: " << header_.magic;
        return std::unexpected(error_code::invalid_message_type);
    }

    // Check version compatibility (major version must match)
    if (header_.version_major != PROTOCOL_VERSION_MAJOR) {
        BOOST_LOG_SEV(lg, warn) << "Invalid major version: "
                                << header_.version_major;
        return std::unexpected(error_code::version_mismatch);
    }

    // Check payload size
    if (header_.payload_size != payload_.size()) {
        BOOST_LOG_SEV(lg, warn) << "Payload size does not match message. Expected: "
                                << header_.payload_size
                                << " but got: " << payload_.size();
        return std::unexpected(error_code::invalid_message_type);
    }

    if (header_.payload_size > MAX_PAYLOAD_SIZE) {
        BOOST_LOG_SEV(lg, warn) << "Payload size too large. Size: "
                                << header_.payload_size
                                << " Maximum: " << MAX_PAYLOAD_SIZE;
        return std::unexpected(error_code::invalid_message_type);
    }

    // Validate CRC
    uint32_t calculated_crc = calculate_crc();
    if (header_.crc != calculated_crc) {
        return std::unexpected(error_code::crc_validation_failed);
    }

    return {};
}

std::ostream& operator<<(std::ostream& s, const frame_header& v) {
    rfl::json::write(v, s);
    return(s);
}

}
