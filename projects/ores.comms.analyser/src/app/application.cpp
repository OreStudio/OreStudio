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
#include "ores.comms.analyser/app/application.hpp"

#include <iomanip>
#include <iostream>
#include <sstream>
#include <magic_enum/magic_enum.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.platform/time/datetime.hpp"

namespace ores::comms::analyser::app {

using namespace ores::telemetry::log;
using namespace ores::comms::recording;
using namespace ores::comms::messaging;

application::application(config::options opts)
    : opts_(std::move(opts)) {
}

int application::run() {
    switch (opts_.cmd) {
    case config::command::read:
        return read_session();
    case config::command::info:
        return show_info();
    default:
        std::cerr << "Unknown command\n";
        return 1;
    }
}

int application::read_session() {
    auto result = domain::session_reader::read(opts_.input_file);
    if (!result) {
        std::cerr << "Error reading session file: "
                  << magic_enum::enum_name(result.error()) << "\n";
        return 1;
    }

    print_header(result->metadata);
    print_frames(*result);

    return 0;
}

int application::show_info() {
    auto result = domain::session_reader::read_metadata(opts_.input_file);
    if (!result) {
        std::cerr << "Error reading session file: "
                  << magic_enum::enum_name(result.error()) << "\n";
        return 1;
    }

    print_header(*result);

    return 0;
}

void application::print_header(const domain::session_metadata& metadata) {
    using namespace ores::platform::time;

    std::cout << "ores.comms.analyser:\n";
    std::cout << "  File: \"" << opts_.input_file.filename().string() << "\"\n";
    std::cout << "  Session ID: " << metadata.session_id << "\n";
    std::cout << "  Start time: "
              << datetime::format_time_point(metadata.start_time, "%Y-%m-%d %H:%M:%S")
              << "\n";
    std::cout << "  Server: " << metadata.server_address << "\n";
    std::cout << "  Protocol: v" << metadata.protocol_version_major << "."
              << metadata.protocol_version_minor << "\n";
    std::cout << "  Compression: " << magic_enum::enum_name(metadata.compression) << "\n";
    std::cout << "\n";
}

void application::print_frames(const domain::session_data& data) {
    if (data.frames.empty()) {
        std::cout << "No frames captured\n";
        return;
    }

    // Calculate the last timestamp for duration
    const auto last_offset = data.frames.back().timestamp_offset_us;
    const auto duration_s = last_offset / 1'000'000.0;

    std::cout << "Duration: " << std::fixed << std::setprecision(3)
              << duration_s << "s\n";
    std::cout << "Frames: " << data.frames.size() << "\n\n";

    // Print header row
    std::cout << std::setw(5) << std::right << "No."
              << "  "
              << std::setw(14) << std::left << "Time"
              << " "
              << std::setw(4) << "Dir"
              << "  "
              << std::setw(32) << std::left << "Type"
              << " "
              << std::setw(6) << std::right << "Length"
              << "  "
              << "Info"
              << "\n";

    // Print separator
    std::cout << std::string(100, '-') << "\n";

    // Print frames
    std::uint64_t frame_num = 1;
    for (const auto& record : data.frames) {
        const auto& hdr = record.frame.header();

        // Direction arrow
        const char* dir_str = (record.direction == frame_direction::sent) ? "->" : "<-";

        // Message type name
        auto type_name = std::string(magic_enum::enum_name(hdr.type));

        // Frame size (header + payload)
        const auto frame_size = frame_header::size + hdr.payload_size;

        // Info string
        auto info = get_message_info(record.frame);

        std::cout << std::setw(5) << std::right << frame_num
                  << "  "
                  << std::setw(14) << std::left << format_timestamp(record.timestamp_offset_us)
                  << " "
                  << std::setw(4) << dir_str
                  << "  "
                  << std::setw(32) << std::left << type_name
                  << " "
                  << std::setw(6) << std::right << frame_size
                  << "  "
                  << info
                  << "\n";

        ++frame_num;
    }

    std::cout << "\n" << data.frames.size() << " frames captured\n";
}

std::string application::format_timestamp(std::int64_t offset_us) {
    // Format as seconds with microsecond precision
    const auto seconds = offset_us / 1'000'000;
    const auto microseconds = offset_us % 1'000'000;

    std::ostringstream oss;
    oss << seconds << "." << std::setfill('0') << std::setw(6) << microseconds;
    return oss.str();
}

std::string application::get_message_info(const comms::messaging::frame& f) {
    const auto& hdr = f.header();

    std::ostringstream oss;

    // Add correlation ID if non-zero
    if (hdr.correlation_id != 0) {
        oss << "corr=" << hdr.correlation_id;
    }

    // Add sequence number
    if (!oss.str().empty()) {
        oss << " ";
    }
    oss << "seq=" << hdr.sequence;

    // Add compression info for frames that have payloads
    if (hdr.payload_size > 0 && hdr.compression != compression_type::none) {
        oss << " " << magic_enum::enum_name(hdr.compression);
    }

    return oss.str();
}

}
