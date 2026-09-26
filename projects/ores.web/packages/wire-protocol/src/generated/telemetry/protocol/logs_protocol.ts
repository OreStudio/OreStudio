/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief A persisted telemetry log entry.
 */
export interface TelemetryLogEntry {
    /**
     * @brief Unique identifier for this log entry.
     */
    id: string;
    /**
     * @brief When the log was emitted by the source.
     */
    timestamp: string;
    /**
     * @brief Source type, client or server.
     */
    source: string;
    /**
     * @brief Name of the source application, for example @c ores.qt.
     */
    source_name: string;
    /**
     * @brief Session identifier for client logs.
     */
    session_id: string | null;
    /**
     * @brief Account identifier for authenticated logs.
     */
    account_id: string | null;
    /**
     * @brief Log severity level.
     */
    level: string;
    /**
     * @brief Logger or component that emitted this log.
     */
    component: string;
    /**
     * @brief The log message.
     */
    message: string;
    /**
     * @brief Optional tag for filtering.
     */
    tag: string;
    /**
     * @brief Server receipt timestamp.
     */
    recorded_at: string;
}

/**
 * @brief Query parameters for retrieving telemetry logs.
 *
 * All filter fields are optional, and multiple filters combine with AND
 * logic.
 */
export interface TelemetryQuery {
    /**
     * @brief Start of the time range, inclusive.
     */
    start_time: string;
    /**
     * @brief End of the time range, exclusive.
     */
    end_time: string;
    /**
     * @brief Filter by source type, client or server.
     */
    source: string | null;
    /**
     * @brief Filter by source application name.
     */
    source_name: string | null;
    /**
     * @brief Filter by session identifier.
     */
    session_id: string | null;
    /**
     * @brief Filter by account identifier.
     */
    account_id: string | null;
    /**
     * @brief Filter by log level.
     */
    level: string | null;
    /**
     * @brief Filter by minimum log level.
     */
    min_level: string | null;
    /**
     * @brief Filter by component name.
     */
    component: string | null;
    /**
     * @brief Filter by tag.
     */
    tag: string | null;
    /**
     * @brief Search text in the message body.
     */
    message_contains: string | null;
    /**
     * @brief Maximum number of results to return.
     */
    limit: number;
    /**
     * @brief Number of results to skip, for pagination.
     */
    offset: number;
}

/**
 * @brief Asks for the log entries a filter selects.
 */
export interface GetTelemetryLogsRequest {
    /**
     * @brief The time range and filters the read applies.
     */
    query: TelemetryQuery;
}

/**
 * @brief The log entries the filter selected.
 */
export interface GetTelemetryLogsResponse {
    /**
     * @brief Whether the entries were read.
     */
    success: boolean;
    /**
     * @brief Why they were not, when they were not.
     */
    message: string;
    /**
     * @brief The entries the read returned.
     */
    entries: TelemetryLogEntry[];
    /**
     * @brief How many entries the filter matches in total.
     */
    total_count: number;
}

/**
 * @brief A single log entry for the fire-and-forget publish protocol.
 *
 * It uses primitive types only, to avoid rfl serialisation issues with
 * boost::uuids::uuid and std::chrono::time_point.
 */
export interface PublishLogEntryItem {
    /**
     * @brief Log severity level.
     */
    level: string;
    /**
     * @brief When the log was emitted, in milliseconds since the epoch.
     */
    timestamp_ms: number;
    /**
     * @brief Logger or component that emitted this log.
     */
    component: string;
    /**
     * @brief The log message.
     */
    message: string;
}

/**
 * @brief Publishes a batch of log entries, fire-and-forget.
 *
 * A wrapper node publishes it to ingest engine logs into the telemetry
 * store. It carries no response, so the publisher does not wait for one.
 */
export interface PublishLogEntriesRequest {
    /**
     * @brief Name of the source application that emitted the entries.
     */
    source_name: string;
    /**
     * @brief Tag applied to every entry in the batch.
     */
    tag: string;
    /**
     * @brief The entries the batch carries.
     */
    entries: PublishLogEntryItem[];
}

export const subjects = {
    get_telemetry_logs_request: "telemetry.v1.logs.list",
    publish_log_entries_request: "telemetry.v1.logs.publish",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_telemetry_logs_request: true,
    publish_log_entries_request: true,
} as const;
