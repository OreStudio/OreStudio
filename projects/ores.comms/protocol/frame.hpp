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
#ifndef ORES_COMMS_PROTOCOL_FRAME_HPP
#define ORES_COMMS_PROTOCOL_FRAME_HPP

#include <span>
#include <array>
#include <vector>
#include <iosfwd>
#include <cstdint>
#include <expected>
#include "ores.comms/protocol/message_types.hpp"

namespace ores::comms::protocol {

constexpr size_t MAX_PAYLOAD_SIZE = 1'000'000; // Example limit

/**
 * @brief Frame header structure for the ORES protocol.
 *
 * Frame layout (32 bytes header):
 * +0:  magic (4 bytes)       - Protocol identifier (0x4F524553 = "ORES")
 * +4:  version_major (2 bytes) - Protocol major version
 * +6:  version_minor (2 bytes) - Protocol minor version
 * +8:  type (2 bytes)        - Message type
 * +10: reserved1 (2 bytes)   - Reserved for future use
 * +12: payload_size (4 bytes) - Size of payload in bytes
 * +16: sequence (4 bytes)    - Sequence number for ordering
 * +20: crc (4 bytes)         - CRC32 checksum of header + payload
 * +24: reserved2 (8 bytes)   - Reserved for future use
 * +32: payload (N bytes)     - Message payload
 */
struct frame_header final {
    std::uint32_t magic;
    std::uint16_t version_major;
    std::uint16_t version_minor;
    message_type type;
    std::uint16_t reserved1;
    std::uint32_t payload_size;
    std::uint32_t sequence;
    std::uint32_t crc;
    std::array<std::uint8_t, 8> reserved2;

    static constexpr size_t size = 32;
};

/**
 * @brief Complete frame with header and payload.
 */
class frame final {
public:
    frame();
    frame(message_type type, std::uint32_t sequence, std::vector<std::uint8_t> payload);

    /**
     * @brief Get the frame header.
     */
    const frame_header& header() const { return header_; }

    /**
     * @brief Get the payload.
     */
    const std::vector<std::uint8_t>& payload() const { return payload_; }

    /**
     * @brief Serialize frame to bytes.
     *
     * Calculates CRC32 over header (excluding CRC field) and payload,
     * then serializes to network byte order.
     */
    std::vector<std::uint8_t> serialize() const;

    /**
     * @brief Deserialize and validate header from bytes.
     *
     * Validates magic number, version, message type, reserved fields, and payload size.
     * Does NOT validate CRC as that requires the full frame.
     * Returns the validated header which can be used to determine how much payload to read.
     */
    static std::expected<frame_header, error_code> deserialize_header(std::span<const std::uint8_t> data);

    /**
     * @brief Deserialize complete frame using a pre-parsed header.
     *
     * Takes the header from deserialize_header() and the complete buffer (header + payload).
     * Validates CRC32 checksum over the entire frame.
     * Returns error if validation fails.
     */
    static std::expected<frame, error_code> deserialize(const frame_header& header, std::span<const std::uint8_t> data);

    /**
     * @brief Validate frame integrity.
     *
     * Checks magic number, version compatibility, and CRC.
     */
    std::expected<void, error_code> validate() const;

private:
    frame_header header_;
    std::vector<std::uint8_t> payload_;

    /**
     * @brief Calculate CRC32 for the frame.
     *
     * CRC is calculated over header (with CRC field set to 0) and payload.
     */
    std::uint32_t calculate_crc() const;

    /**
     * @brief Serialize header to bytes in network byte order.
     */
    void serialize_header(frame_header header, std::span<std::uint8_t> buffer) const;
};

std::ostream& operator<<(std::ostream& s, const frame_header& v);

}

#endif
