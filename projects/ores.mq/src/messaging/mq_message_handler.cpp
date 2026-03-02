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
#include "ores.mq/messaging/mq_message_handler.hpp"

#include "ores.mq/messaging/mq_protocol.hpp"

namespace ores::mq::messaging {

using namespace ores::logging;
using comms::messaging::message_type;

mq_message_handler::mq_message_handler(
    database::context ctx,
    std::shared_ptr<comms::service::auth_session_service> sessions)
    : tenant_aware_handler(std::move(ctx), std::move(sessions)) {}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_message(message_type type,
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Handling MQ message type " << type;

    switch (type) {
    case message_type::get_queues_request:
        co_return co_await handle_get_queues_request(payload, remote_address);
    case message_type::get_queue_metrics_request:
        co_return co_await handle_get_queue_metrics_request(payload, remote_address);
    default:
        BOOST_LOG_SEV(lg(), error) << "Unknown MQ message type " << std::hex
                                   << static_cast<std::uint16_t>(type);
        co_return std::unexpected(ores::utility::serialization::error_code::invalid_message_type);
    }
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_get_queues_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_queues_request.";

    auto auth = require_authentication(remote_address, "Get queues");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_queues_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_queues_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);

    get_queues_response response;
    try {
        response.queues = client_.list_queues(ctx);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Retrieved " << response.queues.size() << " queues";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to list queues: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

boost::asio::awaitable<std::expected<std::vector<std::byte>,
    ores::utility::serialization::error_code>>
mq_message_handler::handle_get_queue_metrics_request(
    std::span<const std::byte> payload, const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug) << "Processing get_queue_metrics_request.";

    auto auth = require_authentication(remote_address, "Get queue metrics");
    if (!auth) co_return std::unexpected(auth.error());

    auto request_result = get_queue_metrics_request::deserialize(payload);
    if (!request_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to deserialize get_queue_metrics_request";
        co_return std::unexpected(request_result.error());
    }

    auto ctx = make_request_context(*auth);

    get_queue_metrics_response response;
    try {
        response.metrics = client_.metrics_all(ctx);
        response.success = true;
        BOOST_LOG_SEV(lg(), info) << "Retrieved metrics for " << response.metrics.size() << " queues";
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Failed to retrieve queue metrics: " << e.what();
        response.success = false;
        response.message = e.what();
    }

    co_return response.serialize();
}

}
