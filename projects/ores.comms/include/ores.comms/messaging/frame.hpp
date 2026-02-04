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
#ifndef ORES_COMMS_MESSAGING_FRAME_HPP
#define ORES_COMMS_MESSAGING_FRAME_HPP

#include <span>
#include <array>
#include <vector>
#include <iosfwd>
#include <cstdint>
#include <expected>
#include "ores.logging/make_logger.hpp"
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/compression_type.hpp"
#include "ores.comms/messaging/message_type.hpp"

namespace ores::comms::messaging {

constexpr size_t MAX_PAYLOAD_SIZE = 1'000'000;

/**
 * @brief Frame header structure for the ORES protocol.
 *
 * Frame layout (32 bytes header):
 * +0:  magic (4 bytes)          - Protocol identifier (0x4F524553 = "ORES")
 * +4:  version_major (2 bytes)  - Protocol major version
 * +6:  version_minor (2 bytes)  - Protocol minor version
 * +8:  type (2 bytes)           - Message type
 * +10: compression (1 byte)     - Compression algorithm (compression_type enum)
 * +11: compression_flags (1 byte) - Reserved compression flags (must be 0)
 * +12: payload_size (4 bytes)   - Size of payload in bytes (compressed size if compressed)
 * +16: sequence (4 bytes)       - Sequence number for ordering
 * +20: crc (4 bytes)            - CRC32 checksum of header + compressed payload
 * +24: correlation_id (4 bytes) - Matches requests to responses
 * +28: reserved2 (4 bytes)      - Reserved for future use
 * +32: payload (N bytes)        - Message payload (compressed if compression != none)
 *
 * When compression is enabled (compression != none):
 * - payload_size contains the compressed size
 * - The payload is the compressed data
 * - CRC is calculated over header + compressed payload
 */
struct frame_header final {
    std::uint32_t magic;
    std::uint16_t version_major;
    std::uint16_t version_minor;
    message_type type;
    compression_type compression;
    std::uint8_t compression_flags;
    std::uint32_t payload_size;
    std::uint32_t sequence;
    std::uint32_t crc;
    std::uint32_t correlation_id;
    std::array<std::uint8_t, 4> reserved2;

    static constexpr size_t size = 32;
};

/**
 * @brief Complete frame with header and payload.
 */
class frame final {
private:
    inline static std::string_view logger_name = "ores.comms.messaging.frame";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    frame();
    frame(message_type type, std::uint32_t sequence, std::vector<std::byte> payload,
        compression_type compression = compression_type::none);
    frame(message_type type, std::uint32_t sequence, std::uint32_t correlation_id,
        std::vector<std::byte> payload,
        compression_type compression = compression_type::none);

    /**
     * @brief Get the correlation ID for request/response matching.
     */
    std::uint32_t correlation_id() const { return header_.correlation_id; }

    /**
     * @brief Get the frame header.
     */
    const frame_header& header() const { return header_; }

    /**
     * @brief Get the raw payload (compressed if compression is enabled).
     */
    const std::vector<std::byte>& payload() const { return payload_; }

    /**
     * @brief Get the compression type used for the payload.
     */
    compression_type compression() const { return header_.compression; }

    /**
     * @brief Decompress and return the payload.
     *
     * If no compression was used, returns a copy of the raw payload.
     * @return The decompressed payload, or error_code on failure
     */
    std::expected<std::vector<std::byte>, ores::utility::serialization::error_code> decompressed_payload() const;

    /**
     * @brief Serialize frame to bytes.
     *
     * Calculates CRC32 over header (excluding CRC field) and payload,
     * then serializes to network byte order.
     */
    std::vector<std::byte> serialize() const;

    /**
     * @brief Deserialize and validate header from bytes.
     *
     * Validates magic number, version, message type, reserved fields, and payload size.
     * Does NOT validate CRC as that requires the full frame.
     * Returns the validated header which can be used to determine how much payload to read.
     *
     * @param data The data buffer containing the header
     * @param skip_version_check If true, skips protocol version validation.
     *        This is useful during handshake to allow the server to send a proper
     *        version mismatch response instead of rejecting the frame immediately.
     */
    static std::expected<frame_header, ores::utility::serialization::error_code> deserialize_header(
        std::span<const std::byte> data, bool skip_version_check = false);

    /**
     * @brief Deserialize complete frame using a pre-parsed header.
     *
     * Takes the header from deserialize_header() and the complete buffer (header + payload).
     * Validates CRC32 checksum over the entire frame.
     * Returns error if validation fails.
     */
    static std::expected<frame, ores::utility::serialization::error_code>
    deserialize(const frame_header& header, std::span<const std::byte> data);

    /**
     * @brief Validate frame integrity.
     *
     * Checks magic number, version compatibility, and CRC.
     */
    std::expected<void, ores::utility::serialization::error_code> validate() const;

private:
    frame_header header_;
    std::vector<std::byte> payload_;

    /**
     * @brief Calculate CRC32 for the frame.
     *
     * CRC is calculated over header (with CRC field set to 0) and payload.
     */
    std::uint32_t calculate_crc() const;

    /**
     * @brief Serialize header to bytes in network byte order.
     */
    void serialize_header(frame_header header, std::span<std::byte> buffer) const;

    /**
     * @brief Initialize payload with optional compression.
     *
     * Compresses the payload if compression is enabled and payload is non-empty.
     * Falls back to uncompressed if compression fails.
     * Updates header_.compression and header_.payload_size accordingly.
     */
    void init_payload(std::vector<std::byte> payload, compression_type compression);
};

std::ostream& operator<<(std::ostream& s, const frame_header& v);

}

#endif
