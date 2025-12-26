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
#ifndef ORES_COMMS_RECORDING_SESSION_FILE_HPP
#define ORES_COMMS_RECORDING_SESSION_FILE_HPP

#include <array>
#include <chrono>
#include <cstdint>
#include <expected>
#include <string>
#include <boost/uuid/uuid.hpp>
#include "ores.comms/messaging/message_types.hpp"

namespace ores::comms::recording {

/**
 * @brief Magic bytes identifying an ORES session recording file.
 *
 * "ORES-REC" in ASCII = 0x4F5245532D524543
 */
constexpr std::array<std::uint8_t, 8> SESSION_FILE_MAGIC = {
    0x4F, 0x52, 0x45, 0x53, 0x2D, 0x52, 0x45, 0x43
};

/**
 * @brief Current version of the session file format.
 */
constexpr std::uint16_t SESSION_FILE_VERSION = 1;

/**
 * @brief Direction of a recorded frame.
 */
enum class frame_direction : std::uint8_t {
    sent = 0x00,     ///< Frame was sent by the client
    received = 0x01  ///< Frame was received by the client
};

/**
 * @brief File header for session recording files.
 *
 * Layout (64 bytes):
 * +0:  magic (8 bytes)           - "ORES-REC" identifier
 * +8:  version (2 bytes)         - File format version
 * +10: reserved1 (2 bytes)       - Reserved for future use
 * +12: protocol_version_major (2 bytes) - ORES protocol version used
 * +14: protocol_version_minor (2 bytes) - ORES protocol minor version
 * +16: session_id (16 bytes)     - UUID v7 for this recording session
 * +32: start_timestamp (8 bytes) - Session start time (microseconds since epoch)
 * +40: server_address_length (2 bytes) - Length of server address string
 * +42: compression (1 byte)      - Negotiated compression type
 * +43: reserved2 (21 bytes)      - Reserved for future use
 *
 * Following the header:
 * - Server address string (variable length, up to 255 bytes)
 * - Frame records
 */
struct session_file_header final {
    std::array<std::uint8_t, 8> magic;
    std::uint16_t version;
    std::uint16_t reserved1;
    std::uint16_t protocol_version_major;
    std::uint16_t protocol_version_minor;
    boost::uuids::uuid session_id;
    std::int64_t start_timestamp_us;
    std::uint16_t server_address_length;
    messaging::compression_type compression;
    std::array<std::uint8_t, 21> reserved2;

    static constexpr size_t size = 64;
};

static_assert(sizeof(session_file_header) == session_file_header::size,
    "session_file_header must be exactly 64 bytes");

/**
 * @brief Header for each recorded frame in the session file.
 *
 * Layout (16 bytes):
 * +0:  timestamp_offset_us (8 bytes) - Microseconds since session start
 * +4:  frame_size (4 bytes)          - Size of the frame data (header + payload)
 * +8:  direction (1 byte)            - Sent or received
 * +9:  reserved (3 bytes)            - Reserved for future use
 *
 * Following the record header:
 * - Frame data (frame_size bytes) - The raw serialized frame
 */
struct frame_record_header final {
    std::int64_t timestamp_offset_us;
    std::uint32_t frame_size;
    frame_direction direction;
    std::array<std::uint8_t, 3> reserved;

    static constexpr size_t size = 16;
};

static_assert(sizeof(frame_record_header) == frame_record_header::size,
    "frame_record_header must be exactly 16 bytes");

/**
 * @brief Error codes for session file operations.
 */
enum class session_file_error {
    none = 0,
    file_open_failed,
    file_write_failed,
    file_read_failed,
    invalid_magic,
    unsupported_version,
    corrupt_file,
    unexpected_eof
};

/**
 * @brief Generate a session filename with timestamp and UUID.
 *
 * Format: session-YYYYMMDD-HHMMSS-{short-uuid}.ores
 *
 * @param session_id The session UUID
 * @param start_time The session start time
 * @return The generated filename
 */
std::string generate_session_filename(
    const boost::uuids::uuid& session_id,
    std::chrono::system_clock::time_point start_time);

}

#endif
