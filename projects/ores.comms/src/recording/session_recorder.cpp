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
#include "ores.comms/recording/session_recorder.hpp"

#include <cstring>
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.utility/serialization/writer.hpp"

namespace ores::comms::recording {

using ores::utility::serialization::writer;

using namespace ores::telemetry::log;

session_recorder::~session_recorder() {
    stop();
}

std::expected<std::filesystem::path, session_file_error> session_recorder::start(
    const std::filesystem::path& output_directory,
    const std::string& server_address,
    messaging::compression_type compression) {

    std::lock_guard lock(mutex_);

    if (recording_) {
        BOOST_LOG_SEV(lg(), warn) << "Recording already in progress, stopping first";
        file_.close();
        recording_ = false;
    }

    // Generate session ID and filename
    utility::uuid::uuid_v7_generator uuid_gen;
    session_id_ = uuid_gen();
    const auto now = std::chrono::system_clock::now();
    const auto filename = generate_session_filename(session_id_, now);

    // Ensure output directory exists
    std::error_code ec;
    std::filesystem::create_directories(output_directory, ec);
    if (ec) {
        BOOST_LOG_SEV(lg(), error) << "Failed to create output directory: " << ec.message();
        return std::unexpected(session_file_error::file_open_failed);
    }

    file_path_ = output_directory / filename;

    // Open file for binary writing
    file_.open(file_path_, std::ios::binary | std::ios::trunc);
    if (!file_.is_open()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to open session file: " << file_path_;
        return std::unexpected(session_file_error::file_open_failed);
    }

    // Record start time for relative timestamps
    start_time_ = std::chrono::steady_clock::now();
    frame_count_ = 0;

    // Write file header
    auto write_result = write_header(server_address, compression);
    if (write_result != session_file_error::none) {
        file_.close();
        return std::unexpected(write_result);
    }

    recording_ = true;
    BOOST_LOG_SEV(lg(), info) << "Started recording session to: " << file_path_;

    return file_path_;
}

void session_recorder::stop() {
    std::lock_guard lock(mutex_);

    if (!recording_) {
        return;
    }

    file_.flush();
    file_.close();
    recording_ = false;

    BOOST_LOG_SEV(lg(), info) << "Stopped recording session. Frames recorded: "
                              << frame_count_.load();
}

bool session_recorder::is_recording() const {
    return recording_.load();
}

void session_recorder::record_sent(const messaging::frame& f) {
    record_frame(f, frame_direction::sent);
}

void session_recorder::record_received(const messaging::frame& f) {
    record_frame(f, frame_direction::received);
}

boost::uuids::uuid session_recorder::session_id() const {
    std::lock_guard lock(mutex_);
    return session_id_;
}

std::filesystem::path session_recorder::file_path() const {
    std::lock_guard lock(mutex_);
    return file_path_;
}

std::uint64_t session_recorder::frame_count() const {
    return frame_count_.load();
}

session_file_error session_recorder::write_header(
    const std::string& server_address,
    messaging::compression_type compression) {

    // Serialize header to buffer using network byte order
    std::vector<std::byte> buffer;
    buffer.reserve(session_file_header::size);

    // Write magic (8 bytes)
    for (const auto& byte : SESSION_FILE_MAGIC) {
        writer::write_uint8(buffer, byte);
    }

    // Write version and reserved
    writer::write_uint16(buffer, SESSION_FILE_VERSION);
    writer::write_uint16(buffer, 0); // reserved1

    // Write protocol version
    writer::write_uint16(buffer, messaging::PROTOCOL_VERSION_MAJOR);
    writer::write_uint16(buffer, messaging::PROTOCOL_VERSION_MINOR);

    // Write session UUID (16 bytes)
    writer::write_uuid(buffer, session_id_);

    // Write start timestamp
    const auto now = std::chrono::system_clock::now();
    const auto us_since_epoch = std::chrono::duration_cast<std::chrono::microseconds>(
        now.time_since_epoch()).count();
    writer::write_int64(buffer, us_since_epoch);

    // Write server address length
    const auto addr_length = static_cast<std::uint16_t>(
        std::min(server_address.size(), static_cast<size_t>(255)));
    writer::write_uint16(buffer, addr_length);

    // Write compression type
    writer::write_uint8(buffer, static_cast<std::uint8_t>(compression));

    // Write reserved2 (21 bytes of zeros)
    for (int i = 0; i < 21; ++i) {
        writer::write_uint8(buffer, 0);
    }

    // Write header to file
    file_.write(reinterpret_cast<const char*>(buffer.data()), buffer.size());
    if (!file_.good()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to write session file header";
        return session_file_error::file_write_failed;
    }

    // Write server address string
    file_.write(server_address.data(), addr_length);
    if (!file_.good()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to write server address";
        return session_file_error::file_write_failed;
    }

    return session_file_error::none;
}

void session_recorder::record_frame(const messaging::frame& f, frame_direction direction) {
    if (!recording_.load()) {
        return;
    }

    std::lock_guard lock(mutex_);

    if (!recording_.load() || !file_.is_open()) {
        return;
    }

    // Serialize the frame
    const auto frame_data = f.serialize();

    // Calculate timestamp offset from session start
    const auto now = std::chrono::steady_clock::now();
    const auto offset = std::chrono::duration_cast<std::chrono::microseconds>(
        now - start_time_).count();

    // Serialize frame record header
    std::vector<std::byte> header_buffer;
    header_buffer.reserve(frame_record_header::size);

    writer::write_int64(header_buffer, offset);
    writer::write_uint32(header_buffer, static_cast<std::uint32_t>(frame_data.size()));
    writer::write_uint8(header_buffer, static_cast<std::uint8_t>(direction));

    // Write reserved (3 bytes of zeros)
    for (int i = 0; i < 3; ++i) {
        writer::write_uint8(header_buffer, 0);
    }

    // Write record header
    file_.write(reinterpret_cast<const char*>(header_buffer.data()), header_buffer.size());

    // Write frame data
    file_.write(reinterpret_cast<const char*>(frame_data.data()), frame_data.size());

    if (!file_.good()) {
        BOOST_LOG_SEV(lg(), error) << "Failed to write frame record";
        return;
    }

    ++frame_count_;

    // Flush periodically to avoid data loss
    if (frame_count_ % 100 == 0) {
        file_.flush();
    }
}

}
