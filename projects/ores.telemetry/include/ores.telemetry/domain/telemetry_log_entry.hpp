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
#ifndef ORES_TELEMETRY_DOMAIN_TELEMETRY_LOG_ENTRY_HPP
#define ORES_TELEMETRY_DOMAIN_TELEMETRY_LOG_ENTRY_HPP

#include <chrono>
#include <string>
#include <optional>
#include <boost/uuid/uuid.hpp>
#include "ores.telemetry/domain/telemetry_source.hpp"

namespace ores::telemetry::domain {

/**
 * @brief A persisted telemetry log entry.
 *
 * This structure represents a log entry as stored in the telemetry_logs
 * database table. It is designed for efficient storage and querying in
 * a TimescaleDB hypertable.
 *
 * The structure is intentionally simpler than the OpenTelemetry log_record
 * structure, focusing on the fields needed for practical log analysis.
 */
struct telemetry_log_entry final {
    /**
     * @brief Unique identifier for this log entry.
     */
    boost::uuids::uuid id;

    /**
     * @brief When the log was emitted by the source.
     *
     * This is the partitioning key for the TimescaleDB hypertable.
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief Source type (client or server).
     */
    telemetry_source source = telemetry_source::client;

    /**
     * @brief Name of the source application.
     *
     * Examples: "ores.qt", "ores.comms.shell", "ores.comms.service"
     */
    std::string source_name;

    /**
     * @brief Session ID for client logs.
     *
     * Empty for server logs or pre-login client logs.
     * Can be joined with sessions table for client version info.
     */
    std::optional<boost::uuids::uuid> session_id;

    /**
     * @brief Account ID for authenticated logs.
     *
     * Empty for server logs or pre-login client logs.
     */
    std::optional<boost::uuids::uuid> account_id;

    /**
     * @brief Log severity level.
     *
     * Valid values: "trace", "debug", "info", "warn", "error"
     */
    std::string level = "info";

    /**
     * @brief Logger/component name that emitted this log.
     *
     * Examples: "ores.qt.main_window", "ores.comms.client"
     */
    std::string component;

    /**
     * @brief The actual log message.
     */
    std::string message;

    /**
     * @brief Optional tag for filtering.
     *
     * Can be used for categorization (e.g., "security", "performance").
     */
    std::string tag;

    /**
     * @brief Server receipt timestamp.
     *
     * Set by the server when the log is received/persisted.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
