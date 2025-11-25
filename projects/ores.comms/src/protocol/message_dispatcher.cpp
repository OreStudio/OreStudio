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
#include "ores.comms/protocol/message_dispatcher.hpp"

namespace ores::comms::protocol {

using namespace ores::utility::log;

message_dispatcher::message_dispatcher() = default;

void message_dispatcher::register_handler(message_type_range range,
    std::shared_ptr<message_handler> handler) {
    BOOST_LOG_SEV(lg(), debug) << "Registering handler for message type range ["
                               << std::hex << range.min << ", " << range.max << "]";
    handlers_[range] = std::move(handler);
}

boost::asio::awaitable<std::expected<frame, error_code>>
message_dispatcher::dispatch(const frame& request_frame, std::uint32_t sequence,
    const std::string& remote_address) {
    const auto msg_type = request_frame.header().type;
    BOOST_LOG_SEV(lg(), debug) << "Dispatching message type " << msg_type;

    // Find the appropriate handler
    auto* handler = find_handler(msg_type);
    if (!handler) {
        BOOST_LOG_SEV(lg(), error) << "No handler registered for message type "
                                   << std::hex << static_cast<std::uint16_t>(msg_type);
        co_return std::unexpected(error_code::invalid_message_type);
    }

    // Invoke the handler
    auto result = co_await handler->handle_message(msg_type, request_frame.payload(),
        remote_address);
    if (!result) {
        BOOST_LOG_SEV(lg(), error) << "Handler failed for message type "
                                   << std::hex << static_cast<std::uint16_t>(msg_type)
                                   << ", error: " << static_cast<int>(result.error());
        co_return std::unexpected(result.error());
    }

    // Create response frame
    auto response_type = get_response_type(msg_type);
    frame response_frame(response_type, sequence, std::move(*result));

    BOOST_LOG_SEV(lg(), debug) << "Successfully dispatched message, response type "
                               << response_type;

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
