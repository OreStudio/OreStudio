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
#include "ores.comms.analyser/domain/session_reader.hpp"

#include <span>
#include <cstring>
#include "ores.utility/serialization/reader.hpp"

namespace ores::comms::analyser::domain {

using namespace ores::logging;
using namespace ores::comms::recording;
using ores::utility::serialization::reader;
using ores::comms::messaging::compression_type;
using ores::comms::messaging::frame;

std::expected<session_data, session_file_error>
session_reader::read(const std::filesystem::path& file_path) {
    std::ifstream file(file_path, std::ios::binary);
    if (!file.is_open()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to open session file: " << file_path;
        return std::unexpected(session_file_error::file_open_failed);
    }

    // Read header
    auto header_result = read_header(file);
    if (!header_result) {
        return std::unexpected(header_result.error());
    }

    auto [metadata, server_addr_len] = *header_result;

    // Read server address string
    std::string server_address(server_addr_len, '\0');
    file.read(server_address.data(), server_addr_len);
    if (!file.good() && !file.eof()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to read server address";
        return std::unexpected(session_file_error::file_read_failed);
    }
    metadata.server_address = server_address;

    session_data data;
    data.metadata = metadata;

    // Read all frame records
    while (true) {
        auto frame_result = read_frame_record(file);
        if (!frame_result) {
            if (frame_result.error() == session_file_error::end_of_file) {
                // Clean end of file reached, this is normal
                break;
            }
            return std::unexpected(frame_result.error());
        }
        data.frames.push_back(std::move(*frame_result));
    }

    BOOST_LOG_SEV(lg(), info) << "Read session file with " << data.frames.size()
                              << " frames";

    return data;
}

std::expected<session_metadata, session_file_error>
session_reader::read_metadata(const std::filesystem::path& file_path) {
    std::ifstream file(file_path, std::ios::binary);
    if (!file.is_open()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to open session file: " << file_path;
        return std::unexpected(session_file_error::file_open_failed);
    }

    auto header_result = read_header(file);
    if (!header_result) {
        return std::unexpected(header_result.error());
    }

    auto [metadata, server_addr_len] = *header_result;

    // Read server address string
    std::string server_address(server_addr_len, '\0');
    file.read(server_address.data(), server_addr_len);
    if (!file.good() && !file.eof()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to read server address";
        return std::unexpected(session_file_error::file_read_failed);
    }
    metadata.server_address = server_address;

    return metadata;
}

std::expected<std::pair<session_metadata, std::uint16_t>, session_file_error>
session_reader::read_header(std::ifstream& file) {
    // Read header bytes
    std::vector<std::byte> buffer(session_file_header::size);
    file.read(reinterpret_cast<char*>(buffer.data()), buffer.size());
    if (!file.good()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to read session file header";
        return std::unexpected(session_file_error::file_read_failed);
    }

    std::span<const std::byte> data(buffer);

    // Validate magic
    for (size_t i = 0; i < SESSION_FILE_MAGIC.size(); ++i) {
        auto byte_result = reader::read_uint8(data);
        if (!byte_result || *byte_result != SESSION_FILE_MAGIC[i]) {
            BOOST_LOG_SEV(lg(), error) << "Invalid session file magic";
            return std::unexpected(session_file_error::invalid_magic);
        }
    }

    // Read version
    auto version_result = reader::read_uint16(data);
    if (!version_result) {
        return std::unexpected(session_file_error::corrupt_file);
    }
    if (*version_result != SESSION_FILE_VERSION) {
        BOOST_LOG_SEV(lg(), error) << "Unsupported session file version: " << *version_result;
        return std::unexpected(session_file_error::unsupported_version);
    }

    // Read reserved1
    (void)reader::read_uint16(data); // skip reserved1

    // Read protocol version
    auto protocol_major_result = reader::read_uint16(data);
    auto protocol_minor_result = reader::read_uint16(data);
    if (!protocol_major_result || !protocol_minor_result) {
        return std::unexpected(session_file_error::corrupt_file);
    }

    // Read session UUID (16 bytes)
    auto session_id_result = reader::read_uuid(data);
    if (!session_id_result) {
        return std::unexpected(session_file_error::corrupt_file);
    }

    // Read start timestamp
    auto start_timestamp_result = reader::read_int64(data);
    if (!start_timestamp_result) {
        return std::unexpected(session_file_error::corrupt_file);
    }

    // Read server address length
    auto server_addr_len_result = reader::read_uint16(data);
    if (!server_addr_len_result) {
        return std::unexpected(session_file_error::corrupt_file);
    }

    // Read compression type
    auto compression_result = reader::read_uint8(data);
    if (!compression_result) {
        return std::unexpected(session_file_error::corrupt_file);
    }
    auto compression = static_cast<compression_type>(*compression_result);

    // Skip reserved2 (21 bytes) - already at correct offset

    // Build metadata
    session_metadata metadata;
    metadata.session_id = *session_id_result;
    metadata.start_time = std::chrono::system_clock::time_point(
        std::chrono::microseconds(*start_timestamp_result));
    metadata.protocol_version_major = *protocol_major_result;
    metadata.protocol_version_minor = *protocol_minor_result;
    metadata.compression = compression;

    return std::make_pair(metadata, *server_addr_len_result);
}

std::expected<recorded_frame, session_file_error>
session_reader::read_frame_record(std::ifstream& file) {
    // Read frame record header
    std::vector<std::byte> header_buffer(frame_record_header::size);
    file.read(reinterpret_cast<char*>(header_buffer.data()), header_buffer.size());
    const auto bytes_read = file.gcount();
    if (bytes_read == 0 && file.eof()) {
        // Clean EOF at frame boundary
        return std::unexpected(session_file_error::end_of_file);
    }
    if (bytes_read < static_cast<std::streamsize>(header_buffer.size())) {
        // Partial read - truncated file
        BOOST_LOG_SEV(lg(), error) << "Truncated frame record header: read "
                                   << bytes_read << " of " << header_buffer.size() << " bytes";
        return std::unexpected(session_file_error::unexpected_eof);
    }
    if (!file.good() && !file.eof()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to read frame record header";
        return std::unexpected(session_file_error::file_read_failed);
    }

    std::span<const std::byte> header_data(header_buffer);

    auto timestamp_result = reader::read_int64(header_data);
    auto frame_size_result = reader::read_uint32(header_data);
    if (!timestamp_result || !frame_size_result) {
        return std::unexpected(session_file_error::corrupt_file);
    }

    auto direction_result = reader::read_uint8(header_data);
    if (!direction_result) {
        return std::unexpected(session_file_error::corrupt_file);
    }
    auto direction = static_cast<frame_direction>(*direction_result);
    // Skip reserved (3 bytes)

    // Read frame data
    std::vector<std::byte> frame_data(*frame_size_result);
    file.read(reinterpret_cast<char*>(frame_data.data()), *frame_size_result);
    const auto frame_bytes_read = file.gcount();
    if (frame_bytes_read < static_cast<std::streamsize>(*frame_size_result)) {
        // Partial read - truncated file
        BOOST_LOG_SEV(lg(), error) << "Truncated frame data: read "
                                   << frame_bytes_read << " of " << *frame_size_result << " bytes";
        return std::unexpected(session_file_error::unexpected_eof);
    }
    if (!file.good() && !file.eof()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to read frame data";
        return std::unexpected(session_file_error::file_read_failed);
    }

    // Parse the frame header first
    auto frame_header_result = frame::deserialize_header(frame_data, true);
    if (!frame_header_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize frame header";
        return std::unexpected(session_file_error::corrupt_file);
    }

    // Deserialize the full frame
    auto frame_result = frame::deserialize(*frame_header_result, frame_data);
    if (!frame_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize frame";
        return std::unexpected(session_file_error::corrupt_file);
    }

    recorded_frame record;
    record.timestamp_offset_us = *timestamp_result;
    record.direction = direction;
    record.frame = std::move(*frame_result);

    return record;
}

}
