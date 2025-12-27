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
#ifndef ORES_TELEMETRY_EXPORT_TELEMETRY_OPTIONS_HPP
#define ORES_TELEMETRY_EXPORT_TELEMETRY_OPTIONS_HPP

#include <chrono>
#include <cstdint>
#include <iosfwd>
#include <string>
#include <filesystem>

namespace ores::telemetry::exp {

/**
 * @brief Options related to telemetry export.
 *
 * These options control the export of log records to external systems for
 * centralized log aggregation and analysis. When enabled, all log records
 * are captured by a telemetry sink and exported to the configured destination.
 */
struct telemetry_options final {
    /**
     * @brief Name of the service producing telemetry.
     *
     * This is used to identify the service in exported log records and follows
     * OpenTelemetry semantic conventions for service.name.
     */
    std::string service_name;

    /**
     * @brief Version of the service producing telemetry.
     *
     * This is used for service.version in the resource attributes.
     */
    std::string service_version;

    /**
     * @brief Path to the output file for telemetry export.
     *
     * Log records are written in JSON Lines format (.jsonl). Each record is
     * a single JSON object on its own line.
     */
    std::filesystem::path output_file;

    /**
     * @brief Directory in which to place the telemetry output file.
     *
     * If output_file is a relative path, it is resolved relative to this
     * directory. If empty, the current working directory is used.
     */
    std::filesystem::path output_directory;

    /**
     * @brief Enable streaming of log records to the server.
     *
     * When enabled, log records are batched and sent to the server in addition
     * to being written to the local file. If the connection is not available,
     * records are buffered locally and sent when the connection is restored.
     *
     * Default: false (file-only export).
     */
    bool streaming_enabled = false;

    /**
     * @brief Number of records to batch before sending to the server.
     *
     * Records are accumulated until this threshold is reached, at which point
     * they are sent as a single batch. Larger batch sizes improve network
     * efficiency at the cost of higher memory usage and potential data loss
     * on crash.
     *
     * Default: 50 records.
     */
    std::uint32_t batch_size = 50;

    /**
     * @brief Maximum time to wait before flushing a partial batch.
     *
     * Even if batch_size hasn't been reached, records will be sent after this
     * interval to ensure timely delivery. This balances network efficiency
     * with latency requirements.
     *
     * Default: 5 seconds.
     */
    std::chrono::seconds flush_interval{5};
};

std::ostream& operator<<(std::ostream& s, const telemetry_options& v);

}

#endif
