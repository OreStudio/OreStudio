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

    // Combine header and payload
    std::vector<uint8_t> result;
    result.reserve(header_bytes.size() + payload_.size());
    result.insert(result.end(),
                  reinterpret_cast<const uint8_t*>(header_bytes.data()),
                  reinterpret_cast<const uint8_t*>(header_bytes.data()) + header_bytes.size());
    result.insert(result.end(), payload_.begin(), payload_.end());

    return result;
}

std::expected<frame, error_code> frame::deserialize(std::span<const uint8_t> data) {
    // First, we need to deserialize the header to know the payload size
    // Try to read the header - we'll use a reasonable minimum size
    if (data.size() < 20) { // Minimum CBOR encoded size estimate
        return std::unexpected(error_code::invalid_message_type);
    }

    // Deserialize header using reflect-cpp BSON
    // We need to find where the header ends and payload begins
    // For BSON, we'll deserialize the header and then use the payload_size field
    auto header_result = rfl::bson::read<frame_header>(data.data(), data.size());

    if (!header_result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    frame_header header = header_result.value();

    // Check magic number
    if (header.magic != PROTOCOL_MAGIC) {
        return std::unexpected(error_code::invalid_message_type);
    }

    // To properly extract the payload, we need to know where the BSON header ends
    // We'll serialize the header again to determine its size
    auto header_bytes = rfl::bson::write(header);
    size_t header_size = header_bytes.size();

    // Check if we have enough data for the payload
    if (data.size() < header_size + header.payload_size) {
        return std::unexpected(error_code::invalid_message_type);
    }

    // Extract payload
    std::vector<uint8_t> payload;
    if (header.payload_size > 0) {
        auto payload_start = data.begin() + header_size;
        auto payload_end = payload_start + header.payload_size;
        payload.assign(payload_start, payload_end);
    }

    // Create frame and validate
    frame f;
    f.header_ = header;
    f.payload_ = std::move(payload);

    auto validation = f.validate();
    if (!validation) {
        return std::unexpected(validation.error());
    }

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
