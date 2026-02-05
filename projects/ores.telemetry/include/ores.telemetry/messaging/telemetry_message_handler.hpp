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
#ifndef ORES_TELEMETRY_MESSAGING_TELEMETRY_MESSAGE_HANDLER_HPP
#define ORES_TELEMETRY_MESSAGING_TELEMETRY_MESSAGE_HANDLER_HPP

#include <memory>
#include "ores.utility/serialization/error_code.hpp"
#include "ores.comms/messaging/tenant_aware_handler.hpp"
#include "ores.comms/service/auth_session_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"

namespace ores::telemetry::messaging {

/**
 * @brief Message handler for telemetry subsystem messages.
 *
 * Processes messages in the telemetry subsystem range (0x5000-0x5FFF).
 * Handles:
 * - submit_log_records_request (0x5000): Persists a batch of client log records
 * - get_telemetry_logs_request (0x5010): Queries raw log entries with filters
 * - get_telemetry_stats_request (0x5020): Queries aggregated statistics
 *
 * The handler converts client log_record structures to persisted telemetry_log_entry
 * structures, enriching them with session and account information from the
 * auth_session_service.
 */
class telemetry_message_handler final : public comms::messaging::tenant_aware_handler {
private:
    inline static std::string_view logger_name =
        "ores.telemetry.messaging.telemetry_message_handler";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a telemetry message handler.
     *
     * @param ctx Database context for repository access
     * @param sessions Shared auth session service for session/account lookup
     */
    telemetry_message_handler(::ores::database::context ctx,
        std::shared_ptr<comms::service::auth_session_service> sessions);

    /**
     * @brief Handle a telemetry subsystem message.
     *
     * @param type The message type (must be in range 0x5000-0x5FFF)
     * @param payload The message payload
     * @param remote_address The remote endpoint address of the client connection
     * @return Expected containing response payload, or error code
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_message(comms::messaging::message_type type,
        std::span<const std::byte> payload,
        const std::string& remote_address) override;

private:
    /**
     * @brief Handle submit_log_records_request message.
     *
     * Converts client log records to telemetry_log_entry structures and
     * persists them to the database. Session and account IDs are extracted
     * from the auth_session_service using the remote address.
     *
     * @param payload The serialized submit_log_records_request
     * @param remote_address The client's remote address for session lookup
     * @return The serialized submit_telemetry_response
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_submit_log_records_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_telemetry_logs_request message.
     *
     * Queries raw telemetry log entries based on filter criteria including
     * time range, source, level, component, and session/account IDs.
     *
     * @param payload The serialized get_telemetry_logs_request
     * @param remote_address The client's remote address for session lookup
     * @return The serialized get_telemetry_logs_response
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_telemetry_logs_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Handle get_telemetry_stats_request message.
     *
     * Queries aggregated telemetry statistics from the continuous aggregates.
     * Supports hourly and daily granularity.
     *
     * @param payload The serialized get_telemetry_stats_request
     * @param remote_address The client's remote address for session lookup
     * @return The serialized get_telemetry_stats_response
     */
    boost::asio::awaitable<std::expected<std::vector<std::byte>, ores::utility::serialization::error_code>>
    handle_get_telemetry_stats_request(std::span<const std::byte> payload,
        const std::string& remote_address);

    /**
     * @brief Convert severity_level enum to string representation.
     */
    static std::string severity_to_string(logging::severity_level level);
};

}

#endif
