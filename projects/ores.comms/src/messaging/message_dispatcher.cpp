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
#include "ores.comms/messaging/message_dispatcher.hpp"

namespace ores::comms::messaging {

using namespace ores::telemetry::log;
using ores::utility::serialization::error_code;

message_dispatcher::message_dispatcher(
    std::shared_ptr<service::auth_session_service> sessions)
    : sessions_(std::move(sessions)) {}

void message_dispatcher::register_handler(message_type_range range,
    std::shared_ptr<message_handler> handler) {
    BOOST_LOG_SEV(lg(), debug) << "Registering handler for message type range ["
                               << std::hex << range.min << ", " << range.max << "]";
    handlers_[range] = std::move(handler);
}

boost::asio::awaitable<std::expected<frame, ores::utility::serialization::error_code>>
message_dispatcher::dispatch(const frame& request_frame, std::uint32_t sequence,
    const std::string& remote_address, compression_type response_compression) {
    const auto msg_type = request_frame.header().type;
    BOOST_LOG_SEV(lg(), debug) << "Dispatching message type " << msg_type;

    // Check authorization before routing to handler
    if (sessions_) {
        auto auth_result = sessions_->authorize_request(msg_type, remote_address);
        if (!auth_result) {
            BOOST_LOG_SEV(lg(), warn) << "Authorization denied for " << msg_type
                                      << " from " << remote_address;
            co_return std::unexpected(auth_result.error());
        }
    }

    // Find the appropriate handler
    auto* handler = find_handler(msg_type);
    if (!handler) {
        BOOST_LOG_SEV(lg(), error) << "No handler registered for message type "
                                   << std::hex << static_cast<std::uint16_t>(msg_type);
        co_return std::unexpected(error_code::invalid_message_type);
    }

    // Decompress payload if necessary
    auto payload_result = request_frame.decompressed_payload();
    if (!payload_result) {
        BOOST_LOG_SEV(lg(), error) << "Failed to decompress payload for message type "
                                   << msg_type
                                   << ", compression: " << request_frame.header().compression
                                   << ", payload_size: " << request_frame.payload().size()
                                   << ", error: " << payload_result.error();
        co_return std::unexpected(payload_result.error());
    }

    // Invoke the handler
    auto result = co_await handler->handle_message(msg_type, *payload_result,
        remote_address);
    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Handler failed for message type "
                                   << std::hex << static_cast<std::uint16_t>(msg_type)
                                   << std::dec << ", error: " << result.error();
        co_return std::unexpected(result.error());
    }

    // Create response frame with correlation_id from request and session compression
    auto response_type = get_response_type(msg_type);
    auto correlation_id = request_frame.correlation_id();
    frame response_frame(response_type, sequence, correlation_id, std::move(*result),
        response_compression);

    BOOST_LOG_SEV(lg(), debug) << "Successfully dispatched message, response type "
                               << response_type << " correlation_id=" << correlation_id
                               << " compression=" << response_compression;

    co_return response_frame;
}

message_handler* message_dispatcher::find_handler(message_type type) const {
    for (const auto& [range, handler] : handlers_) {
        if (range.contains(type)) {
            return handler.get();
        }
    }
    return nullptr;
}

message_type message_dispatcher::get_response_type(message_type request_type) const {
    // By convention, response is request + 1
    auto request_value = static_cast<std::uint16_t>(request_type);
    return static_cast<message_type>(request_value + 1);
}

}
