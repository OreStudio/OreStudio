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
#include "ores.utility/rfl/reflectors.hpp" // Must be before rfl/json.hpp
#include "ores.scheduler/messaging/scheduler_message_handler.hpp"

#include "ores.scheduler/messaging/scheduler_protocol.hpp"
#include "ores.scheduler/service/cron_scheduler.hpp"

namespace ores::scheduler::messaging {

using namespace ores::logging;
using comms::messaging::message_type;

scheduler_message_handler::scheduler_message_handler(
    database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : tenant_aware_handler(std::move(ctx), std::move(sessions)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
scheduler_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling scheduler message type " << type;

    switch (type) {
    case message_type::get_job_definitions_request:
        co_return co_await handle_get_job_definitions_request(payload, remote_address);
    case message_type::schedule_job_request:
        co_return co_await handle_schedule_job_request(payload, remote_address);
    case message_type::unschedule_job_request:
        co_return co_await handle_unschedule_job_request(payload, remote_address);
    case message_type::get_job_history_request:
        co_return co_await handle_get_job_history_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown scheduler message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

// ============================================================================
// get_job_definitions
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
scheduler_message_handler::handle_get_job_definitions_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_job_definitions_request.";

    auto auth = require_authentication(remote_address, "Get job definitions");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_job_definitions_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_job_definitions_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);
    service::cron_scheduler svc(ctx);

    auto definitions = svc.get_all_definitions();
    BOOST_LOG_SEV(lg(), info) << "Retrieved " << definitions.size() << " job definitions";

    get_job_definitions_response response{ .definitions = std::move(definitions) };
    co_return response.serialize();
}

// ============================================================================
// schedule_job
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
scheduler_message_handler::handle_schedule_job_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing schedule_job_request.";

    auto auth = require_authentication(remote_address, "Schedule job");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = schedule_job_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize schedule_job_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    request.definition.modified_by = auth->username;
    request.definition.tenant_id = auth->tenant_id;
    request.definition.party_id = auth->party_id;

    auto ctx = make_request_context(*auth);
    service::cron_scheduler svc(ctx);

    schedule_job_response response;
    try {
        auto scheduled = svc.schedule(std::move(request.definition),
                                      request.change_reason_code,
                                      request.change_commentary);
        BOOST_LOG_SEV(lg(), info) << "Job scheduled: " << scheduled.job_name;
        response.success = true;
        response.message = "Job scheduled successfully.";
        response.definition = std::move(scheduled);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to schedule job: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// unschedule_job
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
scheduler_message_handler::handle_unschedule_job_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing unschedule_job_request.";

    auto auth = require_authentication(remote_address, "Unschedule job");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = unschedule_job_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize unschedule_job_request";
        co_return std::unexpected(request_result.error());
    }

    auto request = std::move(*request_result);
    auto ctx = make_request_context(*auth);
    service::cron_scheduler svc(ctx);

    unschedule_job_response response;
    try {
        svc.unschedule(request.job_definition_id,
                       auth->username,
                       request.change_reason_code,
                       request.change_commentary);
        response.success = true;
        response.message = "Job unscheduled successfully.";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to unschedule job: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

// ============================================================================
// get_job_history
// ============================================================================

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
scheduler_message_handler::handle_get_job_history_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_job_history_request.";

    auto auth = require_authentication(remote_address, "Get job history");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_job_history_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_job_history_request";
        co_return std::unexpected(request_result.error());
    }

    auto& request = *request_result;
    auto ctx = make_request_context(*auth);
    service::cron_scheduler svc(ctx);

    get_job_history_response response;
    try {
        const std::size_t limit = request.limit > 0 ? request.limit : 100;
        auto instances = svc.get_job_history(request.job_definition_id, limit);
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << instances.size()
                                  << " job history entries";
        response.success = true;
        response.message.clear();
        response.instances = std::move(instances);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to get job history: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

}
