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
#include "ores.comms/service/subscription_handler.hpp"
#include "ores.comms/messaging/subscription_protocol.hpp"

namespace ores::comms::service {

using namespace ores::utility::log;

subscription_handler::subscription_handler(
    std::shared_ptr<subscription_manager> manager)
    : manager_(std::move(manager)) {
    BOOST_LOG_SEV(lg(), debug) << "Subscription handler created.";
}

boost::asio::awaitable<std::expected<std::vector<std::byte>, messaging::error_code>>
subscription_handler::handle_message(messaging::message_type type,
    std::span<const std::byte> payload,
    const std::string& remote_address) {

    BOOST_LOG_SEV(lg(), debug)
        << "Handling subscription message type " << type
        << " from " << remote_address;

    switch (type) {
    case messaging::message_type::subscribe_request:
        co_return handle_subscribe_request(payload, remote_address);

    case messaging::message_type::unsubscribe_request:
        co_return handle_unsubscribe_request(payload, remote_address);

    default:
        BOOST_LOG_SEV(lg(), error)
            << "Unexpected message type in subscription handler: " << type;
        co_return std::unexpected(messaging::error_code::invalid_message_type);
    }
}

std::expected<std::vector<std::byte>, messaging::error_code>
subscription_handler::handle_subscribe_request(std::span<const std::byte> payload,
    const std::string& remote_address) {

    auto request = messaging::subscribe_request::deserialize(payload);
    if (!request) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to deserialize subscribe_request from " << remote_address
            << ": " << static_cast<int>(request.error());
        return std::unexpected(request.error());
    }

    BOOST_LOG_SEV(lg(), info)
        << "Processing subscribe request from " << remote_address
        << " for event type '" << request->event_type << "'";

    messaging::subscribe_response response;

    if (manager_->subscribe(remote_address, request->event_type)) {
        response.success = true;
        response.message = "Successfully subscribed to " + request->event_type;
        BOOST_LOG_SEV(lg(), info)
            << "Subscribe succeeded for " << remote_address
            << " to '" << request->event_type << "'";
    } else {
        response.success = false;
        response.message = "Failed to subscribe - session not registered";
        BOOST_LOG_SEV(lg(), warn)
            << "Subscribe failed for " << remote_address
            << " to '" << request->event_type << "' - session not registered";
    }

    return response.serialize();
}

std::expected<std::vector<std::byte>, messaging::error_code>
subscription_handler::handle_unsubscribe_request(std::span<const std::byte> payload,
    const std::string& remote_address) {

    auto request = messaging::unsubscribe_request::deserialize(payload);
    if (!request) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to deserialize unsubscribe_request from " << remote_address
            << ": " << static_cast<int>(request.error());
        return std::unexpected(request.error());
    }

    BOOST_LOG_SEV(lg(), info)
        << "Processing unsubscribe request from " << remote_address
        << " for event type '" << request->event_type << "'";

    messaging::unsubscribe_response response;

    if (manager_->unsubscribe(remote_address, request->event_type)) {
        response.success = true;
        response.message = "Successfully unsubscribed from " + request->event_type;
        BOOST_LOG_SEV(lg(), info)
            << "Unsubscribe succeeded for " << remote_address
            << " from '" << request->event_type << "'";
    } else {
        response.success = false;
        response.message = "Not subscribed to " + request->event_type;
        BOOST_LOG_SEV(lg(), debug)
            << "Unsubscribe for " << remote_address
            << " from '" << request->event_type << "' - was not subscribed";
    }

    return response.serialize();
}

}
