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
#ifndef ORES_TELEMETRY_EXPORT_FILE_LOG_EXPORTER_HPP
#define ORES_TELEMETRY_EXPORT_FILE_LOG_EXPORTER_HPP

#include <mutex>
#include <fstream>
#include <filesystem>
#include "ores.telemetry/export/log_exporter.hpp"

namespace ores::telemetry::exp {

/**
 * @brief Exports log records to a JSON Lines file.
 *
 * Each log record is written as a single JSON object on its own line,
 * following the JSON Lines format (https://jsonlines.org/). This format
 * is ideal for log shipping tools like Filebeat or Fluent Bit.
 *
 * Example output:
 * @code
 * {"timestamp":"2025-01-15T10:30:45.123Z","severity":"INFO","body":"Starting..."}
 * {"timestamp":"2025-01-15T10:30:45.456Z","severity":"DEBUG","body":"Connected"}
 * @endcode
 *
 * Thread-safety: This class is thread-safe. Multiple threads can call
 * export_record() concurrently.
 */
class file_log_exporter final : public log_exporter {
public:
    /**
     * @brief Constructs a file log exporter.
     *
     * @param path Path to the output file. Will be created if it doesn't exist.
     * @throws std::runtime_error if the file cannot be opened.
     */
    explicit file_log_exporter(const std::filesystem::path& path);

    ~file_log_exporter() override;

    /**
     * @brief Exports a log record as a JSON line.
     *
     * The record is immediately written to the file (no internal buffering
     * beyond OS-level buffering).
     *
     * @param record The log record to export.
     */
    void export_record(domain::log_record record) override;

    /**
     * @brief Flushes the file stream.
     */
    void flush() override;

    /**
     * @brief Closes the file.
     */
    void shutdown() override;

private:
    std::mutex mutex_;
    std::ofstream file_;
    bool is_open_;
};

}

#endif
