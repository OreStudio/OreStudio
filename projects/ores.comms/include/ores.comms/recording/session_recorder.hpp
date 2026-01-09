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
#ifndef ORES_COMMS_RECORDING_SESSION_RECORDER_HPP
#define ORES_COMMS_RECORDING_SESSION_RECORDER_HPP

#include <mutex>
#include <atomic>
#include <chrono>
#include <fstream>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.comms/messaging/frame.hpp"
#include "ores.comms/recording/session_file.hpp"

namespace ores::comms::recording {

/**
 * @brief Records communication frames to a session file.
 *
 * This class provides thread-safe recording of frames sent and received
 * during a client session. The recorded data can later be analyzed using
 * the ores.comms.analyser tool.
 *
 * Usage:
 * @code
 * session_recorder recorder;
 * auto result = recorder.start("/path/to/output", "server:8443", compression);
 * if (result) {
 *     // Recording is active, frames will be written to:
 *     // /path/to/output/session-20250115-143205-abc123.ores
 * }
 *
 * // Record frames as they are sent/received
 * recorder.record_sent(frame);
 * recorder.record_received(frame);
 *
 * recorder.stop();
 * @endcode
 */
class session_recorder final {
private:
    inline static std::string_view logger_name = "ores.comms.recording.session_recorder";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    session_recorder() = default;
    ~session_recorder();

    // Non-copyable
    session_recorder(const session_recorder&) = delete;
    session_recorder& operator=(const session_recorder&) = delete;

    // Non-movable (due to mutex)
    session_recorder(session_recorder&&) = delete;
    session_recorder& operator=(session_recorder&&) = delete;

    /**
     * @brief Start recording to a new session file.
     *
     * Creates a new session file with a unique name in the specified directory.
     * The filename format is: session-YYYYMMDD-HHMMSS-{short-uuid}.ores
     *
     * @param output_directory Directory where the session file will be created
     * @param server_address The server address being connected to
     * @param compression The negotiated compression type for the session
     * @return Expected containing the full path to the created file, or error
     */
    std::expected<std::filesystem::path, session_file_error> start(
        const std::filesystem::path& output_directory,
        const std::string& server_address,
        messaging::compression_type compression = messaging::compression_type::none);

    /**
     * @brief Stop recording and close the session file.
     *
     * Flushes any buffered data and closes the file. Safe to call multiple
     * times or when not recording.
     */
    void stop();

    /**
     * @brief Check if recording is currently active.
     */
    bool is_recording() const;

    /**
     * @brief Record a frame that was sent by the client.
     *
     * Thread-safe. If recording is not active, does nothing.
     *
     * @param f The frame that was sent
     */
    void record_sent(const messaging::frame& f);

    /**
     * @brief Record a frame that was received by the client.
     *
     * Thread-safe. If recording is not active, does nothing.
     *
     * @param f The frame that was received
     */
    void record_received(const messaging::frame& f);

    /**
     * @brief Get the session ID for the current recording.
     *
     * @return The session UUID, or nil UUID if not recording
     */
    boost::uuids::uuid session_id() const;

    /**
     * @brief Get the path to the current recording file.
     *
     * @return The file path, or empty path if not recording
     */
    std::filesystem::path file_path() const;

    /**
     * @brief Get the number of frames recorded so far.
     */
    std::uint64_t frame_count() const;

private:
    /**
     * @brief Write the file header.
     */
    session_file_error write_header(
        const std::string& server_address,
        messaging::compression_type compression);

    /**
     * @brief Record a frame with the specified direction.
     */
    void record_frame(const messaging::frame& f, frame_direction direction);

    mutable std::mutex mutex_;
    std::ofstream file_;
    std::filesystem::path file_path_;
    boost::uuids::uuid session_id_;
    std::chrono::steady_clock::time_point start_time_;
    std::atomic<bool> recording_{false};
    std::atomic<std::uint64_t> frame_count_{0};
};

}

#endif
