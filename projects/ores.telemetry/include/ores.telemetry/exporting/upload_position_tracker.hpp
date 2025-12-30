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
#ifndef ORES_TELEMETRY_EXPORTING_UPLOAD_POSITION_TRACKER_HPP
#define ORES_TELEMETRY_EXPORTING_UPLOAD_POSITION_TRACKER_HPP

#include <cstdint>
#include <filesystem>
#include <mutex>

namespace ores::telemetry::exporting {

/**
 * @brief Tracks the upload position for telemetry log files.
 *
 * This class maintains state about which log records have been successfully
 * uploaded to the server. It uses a companion marker file (.uploaded) stored
 * alongside the log file to persist the position across restarts.
 *
 * The marker file contains a single 64-bit integer representing the byte
 * offset in the log file up to which records have been successfully uploaded.
 * When reconnecting after a disconnect, this position is used to resume
 * uploading from where we left off.
 *
 * Thread-safe: all public methods are protected by a mutex.
 */
class upload_position_tracker final {
public:
    /**
     * @brief Constructs a tracker for the specified log file.
     *
     * @param log_file_path Path to the log file being tracked.
     *
     * The marker file will be created at log_file_path with ".uploaded"
     * extension added (e.g., "telemetry.jsonl" -> "telemetry.jsonl.uploaded").
     */
    explicit upload_position_tracker(std::filesystem::path log_file_path);

    /**
     * @brief Gets the current upload position.
     *
     * @return The byte offset in the log file up to which records have been
     *         successfully uploaded. Returns 0 if no records have been uploaded.
     */
    std::uint64_t get_position() const;

    /**
     * @brief Updates the upload position after a successful upload.
     *
     * This atomically updates both the in-memory position and the marker file.
     * The marker file is synced to disk to ensure durability.
     *
     * @param position The new byte offset representing the uploaded position.
     */
    void set_position(std::uint64_t position);

    /**
     * @brief Gets the path to the marker file.
     *
     * @return Path to the .uploaded marker file.
     */
    const std::filesystem::path& marker_file_path() const;

    /**
     * @brief Resets the tracker to position 0 and deletes the marker file.
     *
     * Use this when starting a new log file or when the log file has been
     * truncated/rotated.
     */
    void reset();

private:
    /**
     * @brief Loads the position from the marker file.
     *
     * @return The stored position, or 0 if the marker file doesn't exist.
     */
    std::uint64_t load_position_from_file() const;

    /**
     * @brief Saves the position to the marker file.
     *
     * @param position The position to save.
     */
    void save_position_to_file(std::uint64_t position) const;

    std::filesystem::path log_file_path_;
    std::filesystem::path marker_file_path_;
    mutable std::mutex mutex_;
    std::uint64_t position_;
};

}

#endif
