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
#ifndef ORES_TELEMETRY_DOMAIN_LOG_RECORD_HPP
#define ORES_TELEMETRY_DOMAIN_LOG_RECORD_HPP

#include <chrono>
#include <memory>
#include <optional>
#include <string>
#include "ores.telemetry/domain/trace_id.hpp"
#include "ores.telemetry/domain/span_id.hpp"
#include "ores.logging/severity_level.hpp"
#include "ores.telemetry/domain/attribute_value.hpp"
#include "ores.telemetry/domain/resource.hpp"

namespace ores::telemetry::domain {

using logging::severity_level;

/**
 * @brief A log record with trace correlation.
 *
 * This represents a single log entry that can be correlated with a trace
 * and span. By including trace_id and span_id, logs can be filtered and
 * associated with specific operations in the distributed trace.
 *
 * This structure follows the OpenTelemetry log data model.
 */
struct log_record final {
    /**
     * @brief When the log was emitted.
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief When the event that the log describes occurred.
     *
     * This may differ from timestamp if the log is emitted after the fact.
     */
    std::optional<std::chrono::system_clock::time_point> observed_timestamp;

    /**
     * @brief The severity of the log message.
     */
    severity_level severity = severity_level::info;

    /**
     * @brief The log message body.
     */
    std::string body;

    /**
     * @brief The trace this log belongs to, if any.
     */
    std::optional<trace_id> trace;

    /**
     * @brief The span this log was emitted from, if any.
     */
    std::optional<span_id> span;

    /**
     * @brief The name of the logger/component that emitted this log.
     */
    std::string logger_name;

    /**
     * @brief Additional attributes providing context.
     */
    attributes attrs;

    /**
     * @brief The resource that produced this log.
     *
     * Shared across all logs from the same source.
     */
    std::shared_ptr<resource> source_resource;
};

}

#endif
