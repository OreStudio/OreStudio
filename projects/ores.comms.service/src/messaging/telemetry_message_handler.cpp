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
#include "ores.telemetry/messaging/telemetry_message_handler.hpp"

#include <boost/uuid/random_generator.hpp>
#include "ores.telemetry/messaging/log_records_protocol.hpp"
#include "ores.telemetry/messaging/telemetry_protocol.hpp"
#include "ores.telemetry/domain/telemetry_batch.hpp"

namespace ores::telemetry::messaging {

using namespace ores::telemetry::log;

telemetry_message_handler::telemetry_message_handler(database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : ctx_(std::move(ctx))
    , sessions_(std::move(sessions))
    , repo_(ctx_) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
telemetry_message_handler::handle_message(comms::messaging::message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling telemetry message type " << type;

    switch (type) {
    case comms::messaging::message_type::submit_log_records_request:
        co_return co_await handle_submit_log_records_request(payload, remote_address);
    case comms::messaging::message_type::get_telemetry_logs_request:
        co_return co_await handle_get_telemetry_logs_request(payload);
    case comms::messaging::message_type::get_telemetry_stats_request:
        co_return co_await handle_get_telemetry_stats_request(payload);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown telemetry message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(comms::messaging::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
telemetry_message_handler::
handle_submit_log_records_request(std::span<const std::byte> payload,
    const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing submit_log_records_request";

    // Deserialize request
    auto request_result = submit_log_records_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize submit_log_records_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;
    BOOST_LOG_SEV(lg(), debug) << "Received " << request.records.size()
                               << " log records from " << remote_address;

    // Get session info for the client (if available)
    std::optional<boost::uuids::uuid> session_id;
    std::optional<boost::uuids::uuid> account_id;
    std::string source_name = "unknown";

    if (sessions_) {
        auto session_opt = sessions_->get_session(remote_address);
        if (session_opt) {
            // Note: session_info only has account_id; session_id would need
            // to be retrieved from the session repository if needed
            account_id = session_opt->account_id;
        }
    }

    // Convert log_records to telemetry_log_entries
    domain::telemetry_batch batch;
    batch.source = domain::telemetry_source::client;
    // Get source_name from the first record's resource if available
    if (!request.records.empty() && request.records[0].source_resource) {
        auto svc_name = request.records[0].source_resource->service_name();
        if (svc_name) {
            source_name = *svc_name;
        }
    }
    batch.source_name = source_name;

    boost::uuids::random_generator uuid_gen;
    const auto now = std::chrono::system_clock::now();

    for (const auto& record : request.records) {
        domain::telemetry_log_entry entry;
        entry.id = uuid_gen();
        entry.timestamp = record.timestamp;
        entry.source = domain::telemetry_source::client;
        entry.source_name = source_name;
        entry.session_id = session_id;
        entry.account_id = account_id;
        entry.level = severity_to_string(record.severity);
        entry.component = record.logger_name;
        entry.message = record.body;
        entry.tag = "";  // Could extract from attributes if needed
        entry.recorded_at = now;

        batch.entries.push_back(std::move(entry));
    }

    // Persist to database
    submit_telemetry_response response;
    try {
        auto count = repo_.create_batch(batch);
        response.success = true;
        response.entries_accepted = static_cast<std::uint32_t>(count);
        response.message = "Telemetry logged successfully";

        BOOST_LOG_SEV(lg(), info) << "Persisted " << count
                                  << " telemetry log entries from " << source_name;
    } catch (const std::exception& e) {
        response.success = false;
        response.entries_accepted = 0;
        response.message = std::string("Failed to persist telemetry: ") + e.what();

        BOOST_LOG_SEV(lg(), error) << "Failed to persist telemetry: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
telemetry_message_handler::
handle_get_telemetry_logs_request(std::span<const std::byte> payload) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_telemetry_logs_request";

    // Deserialize request
    auto request_result = get_telemetry_logs_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_telemetry_logs_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    // Validate query parameters
    constexpr std::uint32_t max_limit = 1000;
    if (request.query.limit == 0 || request.query.limit > max_limit) {
        BOOST_LOG_SEV(lg(), warn) << "Invalid limit: " << request.query.limit
                                  << ". Must be between 1 and " << max_limit;
        co_return std::unexpected(comms::messaging::error_code::invalid_request);
    }

    get_telemetry_logs_response response;
    try {
        // Get total count for pagination
        response.total_count = repo_.count(request.query);

        // Query log entries
        response.entries = repo_.query(request.query);
        response.success = true;
        response.message = "Query completed successfully";

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.entries.size()
                                  << " of " << response.total_count << " total logs";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Query failed: ") + e.what();
        response.total_count = 0;

        BOOST_LOG_SEV(lg(), error) << "Failed to query telemetry logs: " << e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
                                     comms::messaging::error_code>>
telemetry_message_handler::
handle_get_telemetry_stats_request(std::span<const std::byte> payload) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_telemetry_stats_request";

    // Deserialize request
    auto request_result = get_telemetry_stats_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_telemetry_stats_request";
        co_return std::unexpected(request_result.error());
    }

    const auto& request = *request_result;

    get_telemetry_stats_response response;
    try {
        // Query appropriate continuous aggregate based on granularity
        switch (request.query.granularity) {
        case domain::stats_granularity::hourly:
            response.stats = repo_.read_hourly_stats(request.query);
            break;
        case domain::stats_granularity::daily:
            response.stats = repo_.read_daily_stats(request.query);
            break;
        }

        response.success = true;
        response.message = "Statistics retrieved successfully";

        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.stats.size()
                                  << " statistics entries";
    } catch (const std::exception& e) {
        response.success = false;
        response.message = std::string("Failed to query statistics: ") + e.what();

        BOOST_LOG_SEV(lg(), error) << "Failed to query telemetry stats: " << e.what();
    }

    co_return response.serialize();
}

std::string telemetry_message_handler::severity_to_string(domain::severity_level level) {
    switch (level) {
    case domain::severity_level::trace:
        return "trace";
    case domain::severity_level::debug:
        return "debug";
    case domain::severity_level::info:
        return "info";
    case domain::severity_level::warn:
        return "warn";
    case domain::severity_level::error:
        return "error";
    case domain::severity_level::fatal:
        return "error";  // Map fatal to error for database storage
    default:
        return "info";
    }
}

}
