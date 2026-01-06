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
#ifndef ORES_TELEMETRY_MESSAGING_TELEMETRY_PROTOCOL_HPP
#define ORES_TELEMETRY_MESSAGING_TELEMETRY_PROTOCOL_HPP

#include <span>
#include <iosfwd>
#include <vector>
#include <cstdint>
#include <expected>
#include "ores.comms/messaging/message_types.hpp"
#include "ores.comms/messaging/message_traits.hpp"
#include "ores.telemetry/domain/telemetry_log_entry.hpp"
#include "ores.telemetry/domain/telemetry_batch.hpp"
#include "ores.telemetry/domain/telemetry_query.hpp"
#include "ores.telemetry/domain/telemetry_stats.hpp"

namespace ores::telemetry::messaging {

/**
 * @brief Response to submit_log_records_request.
 *
 * Acknowledges receipt of telemetry log entries and reports how many
 * were successfully persisted.
 */
struct submit_telemetry_response final {
    /**
     * @brief Whether the submission was successful.
     */
    bool success = true;

    /**
     * @brief Number of entries that were accepted and persisted.
     */
    std::uint32_t entries_accepted = 0;

    /**
     * @brief Optional error message if some entries failed.
     */
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<submit_telemetry_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const submit_telemetry_response& v);

/**
 * @brief Request to retrieve telemetry log entries.
 *
 * Clients send this to query raw log entries within a time range,
 * with optional filters for source, level, component, etc.
 */
struct get_telemetry_logs_request final {
    /**
     * @brief Query parameters.
     */
    domain::telemetry_query query;

    std::vector<std::byte> serialize() const;
    static std::expected<get_telemetry_logs_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_telemetry_logs_request& v);

/**
 * @brief Response containing telemetry log entries.
 */
struct get_telemetry_logs_response final {
    /**
     * @brief Whether the query was successful.
     */
    bool success = true;

    /**
     * @brief The log entries matching the query.
     */
    std::vector<domain::telemetry_log_entry> entries;

    /**
     * @brief Total count of matching entries (for pagination).
     */
    std::uint64_t total_count = 0;

    /**
     * @brief Error message if query failed.
     */
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<get_telemetry_logs_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_telemetry_logs_response& v);

/**
 * @brief Request to retrieve telemetry statistics.
 *
 * Clients send this to query aggregated statistics from the continuous
 * aggregates (hourly or daily).
 */
struct get_telemetry_stats_request final {
    /**
     * @brief Query parameters.
     */
    domain::telemetry_stats_query query;

    std::vector<std::byte> serialize() const;
    static std::expected<get_telemetry_stats_request, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_telemetry_stats_request& v);

/**
 * @brief Response containing telemetry statistics.
 */
struct get_telemetry_stats_response final {
    /**
     * @brief Whether the query was successful.
     */
    bool success = true;

    /**
     * @brief The aggregated statistics.
     */
    std::vector<domain::telemetry_stats> stats;

    /**
     * @brief Error message if query failed.
     */
    std::string message;

    std::vector<std::byte> serialize() const;
    static std::expected<get_telemetry_stats_response, comms::messaging::error_code>
    deserialize(std::span<const std::byte> data);
};

std::ostream& operator<<(std::ostream& s, const get_telemetry_stats_response& v);

}

namespace ores::comms::messaging {

/**
 * @brief Message traits for get_telemetry_logs_request.
 */
template<>
struct message_traits<telemetry::messaging::get_telemetry_logs_request> {
    using request_type = telemetry::messaging::get_telemetry_logs_request;
    using response_type = telemetry::messaging::get_telemetry_logs_response;
    static constexpr message_type request_message_type =
        message_type::get_telemetry_logs_request;
};

/**
 * @brief Message traits for get_telemetry_stats_request.
 */
template<>
struct message_traits<telemetry::messaging::get_telemetry_stats_request> {
    using request_type = telemetry::messaging::get_telemetry_stats_request;
    using response_type = telemetry::messaging::get_telemetry_stats_response;
    static constexpr message_type request_message_type =
        message_type::get_telemetry_stats_request;
};

}

#endif
