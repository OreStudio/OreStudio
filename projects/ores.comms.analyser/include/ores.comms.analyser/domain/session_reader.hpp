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
#ifndef ORES_COMMS_ANALYSER_DOMAIN_SESSION_READER_HPP
#define ORES_COMMS_ANALYSER_DOMAIN_SESSION_READER_HPP

#include <vector>
#include <string>
#include <chrono>
#include <fstream>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/recording/session_file.hpp"

namespace ores::comms::analyser::domain {

/**
 * @brief Metadata about a recorded session.
 */
struct session_metadata {
    boost::uuids::uuid session_id;
    std::chrono::system_clock::time_point start_time;
    std::string server_address;
    std::uint16_t protocol_version_major;
    std::uint16_t protocol_version_minor;
    comms::messaging::compression_type compression;
};

/**
 * @brief A single recorded frame with metadata.
 */
struct recorded_frame {
    std::int64_t timestamp_offset_us;
    comms::recording::frame_direction direction;
    comms::messaging::frame frame;
};

/**
 * @brief Complete session data including metadata and frames.
 */
struct session_data {
    session_metadata metadata;
    std::vector<recorded_frame> frames;
};

/**
 * @brief Reads and parses ORES session recording files.
 *
 * This class provides functionality to read session recording files
 * created by the session_recorder and extract the contained frames
 * and metadata for analysis.
 */
class session_reader final {
private:
    inline static std::string_view logger_name = "ores.comms.analyser.domain.session_reader";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Read a complete session file.
     *
     * @param file_path Path to the session recording file
     * @return Expected containing session data, or error
     */
    static std::expected<session_data, comms::recording::session_file_error>
    read(const std::filesystem::path& file_path);

    /**
     * @brief Read only the session metadata (header).
     *
     * Useful for quickly inspecting a file without loading all frames.
     *
     * @param file_path Path to the session recording file
     * @return Expected containing session metadata, or error
     */
    static std::expected<session_metadata, comms::recording::session_file_error>
    read_metadata(const std::filesystem::path& file_path);

private:
    /**
     * @brief Read and parse the file header.
     */
    static std::expected<std::pair<session_metadata, std::uint16_t>, comms::recording::session_file_error>
    read_header(std::ifstream& file);

    /**
     * @brief Read a single frame record.
     */
    static std::expected<recorded_frame, comms::recording::session_file_error>
    read_frame_record(std::ifstream& file);
};

}

#endif
