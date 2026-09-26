/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_TELEMETRY_CORE_MESSAGING_LOGS_PROTOCOL_HPP
#define ORES_TELEMETRY_CORE_MESSAGING_LOGS_PROTOCOL_HPP

#include "ores.telemetry.core/domain/telemetry_source.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::telemetry::messaging {

/**
 * @brief A persisted telemetry log entry.
 */
struct telemetry_log_entry {
    /**
     * @brief Unique identifier for this log entry.
     */
    boost::uuids::uuid id;
    /**
     * @brief When the log was emitted by the source.
     */
    std::chrono::system_clock::time_point timestamp;
    /**
     * @brief Source type, client or server.
     */
    ores::telemetry::domain::telemetry_source source;
    /**
     * @brief Name of the source application, for example @c ores.qt.
     */
    std::string source_name;
    /**
     * @brief Session identifier for client logs.
     */
    std::optional<boost::uuids::uuid> session_id;
    /**
     * @brief Account identifier for authenticated logs.
     */
    std::optional<boost::uuids::uuid> account_id;
    /**
     * @brief Log severity level.
     */
    std::string level;
    /**
     * @brief Logger or component that emitted this log.
     */
    std::string component;
    /**
     * @brief The log message.
     */
    std::string message;
    /**
     * @brief Optional tag for filtering.
     */
    std::string tag;
    /**
     * @brief Server receipt timestamp.
     */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Query parameters for retrieving telemetry logs.
 *
 * All filter fields are optional, and multiple filters combine with AND
 * logic.
 */
struct telemetry_query {
    /**
     * @brief Start of the time range, inclusive.
     */
    std::chrono::system_clock::time_point start_time;
    /**
     * @brief End of the time range, exclusive.
     */
    std::chrono::system_clock::time_point end_time;
    /**
     * @brief Filter by source type, client or server.
     */
    std::optional<ores::telemetry::domain::telemetry_source> source;
    /**
     * @brief Filter by source application name.
     */
    std::optional<std::string> source_name;
    /**
     * @brief Filter by session identifier.
     */
    std::optional<boost::uuids::uuid> session_id;
    /**
     * @brief Filter by account identifier.
     */
    std::optional<boost::uuids::uuid> account_id;
    /**
     * @brief Filter by log level.
     */
    std::optional<std::string> level;
    /**
     * @brief Filter by minimum log level.
     */
    std::optional<std::string> min_level;
    /**
     * @brief Filter by component name.
     */
    std::optional<std::string> component;
    /**
     * @brief Filter by tag.
     */
    std::optional<std::string> tag;
    /**
     * @brief Search text in the message body.
     */
    std::optional<std::string> message_contains;
    /**
     * @brief Maximum number of results to return.
     */
    std::uint32_t limit = 1000;
    /**
     * @brief Number of results to skip, for pagination.
     */
    std::uint32_t offset = 0;
};

/**
 * @brief Asks for the log entries a filter selects.
 */
struct get_telemetry_logs_request {
    using response_type = struct get_telemetry_logs_response;
    static constexpr std::string_view nats_subject = "telemetry.v1.logs.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The time range and filters the read applies.
     */
    telemetry_query query;
};

/**
 * @brief The log entries the filter selected.
 */
struct get_telemetry_logs_response {
    /**
     * @brief Whether the entries were read.
     */
    bool success = false;
    /**
     * @brief Why they were not, when they were not.
     */
    std::string message;
    /**
     * @brief The entries the read returned.
     */
    std::vector<telemetry_log_entry> entries;
    /**
     * @brief How many entries the filter matches in total.
     */
    std::uint64_t total_count;
};

/**
 * @brief A single log entry for the fire-and-forget publish protocol.
 *
 * It uses primitive types only, to avoid rfl serialisation issues with
 * boost::uuids::uuid and std::chrono::time_point.
 */
struct publish_log_entry_item {
    /**
     * @brief Log severity level.
     */
    std::string level;
    /**
     * @brief When the log was emitted, in milliseconds since the epoch.
     */
    std::int64_t timestamp_ms = 0;
    /**
     * @brief Logger or component that emitted this log.
     */
    std::string component;
    /**
     * @brief The log message.
     */
    std::string message;
};

/**
 * @brief Publishes a batch of log entries, fire-and-forget.
 *
 * A wrapper node publishes it to ingest engine logs into the telemetry
 * store. It carries no response, so the publisher does not wait for one.
 */
struct publish_log_entries_request {
    static constexpr std::string_view nats_subject = "telemetry.v1.logs.publish";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Name of the source application that emitted the entries.
     */
    std::string source_name;
    /**
     * @brief Tag applied to every entry in the batch.
     */
    std::string tag;
    /**
     * @brief The entries the batch carries.
     */
    std::vector<publish_log_entry_item> entries;
};

}

#endif
