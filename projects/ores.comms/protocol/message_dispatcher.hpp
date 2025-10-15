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
#ifndef ORES_COMMS_PROTOCOL_MESSAGE_DISPATCHER_HPP
#define ORES_COMMS_PROTOCOL_MESSAGE_DISPATCHER_HPP

#include <map>
#include <memory>
#include <expected>
#include <boost/cobalt.hpp>
#include "ores.comms/protocol/frame.hpp"
#include "ores.comms/protocol/message_handler.hpp"

namespace ores::comms::protocol {

namespace cobalt = boost::cobalt;

/**
 * @brief Dispatches incoming messages to registered subsystem handlers.
 *
 * The dispatcher maintains a registry of message handlers for different
 * message type ranges. When a message arrives, it routes it to the appropriate
 * handler based on the message type, and converts the handler's response
 * back into a frame.
 *
 * Thread-safety: This class is not thread-safe. Registration should happen
 * during initialization before concurrent access begins.
 */
class message_dispatcher final {
public:
    message_dispatcher();

    /**
     * @brief Register a handler for a range of message types.
     *
     * @param range The range of message types this handler processes
     * @param handler The handler implementation
     *
     * Note: Ranges must not overlap. Behavior is undefined if overlapping
     * ranges are registered.
     */
    void register_handler(message_type_range range,
        std::shared_ptr<message_handler> handler);

    /**
     * @brief Dispatch a request frame to the appropriate handler.
     *
     * Finds the registered handler for the message type, invokes it,
     * and wraps the response payload in a frame with the appropriate
     * response message type.
     *
     * @param request_frame The incoming request frame
     * @param sequence The sequence number for the response frame
     * @return Expected containing response frame, or error_code if:
     *         - No handler is registered for this message type
     *         - Handler returns an error
     */
    cobalt::promise<std::expected<frame, error_code>>
    dispatch(const frame& request_frame, std::uint32_t sequence);

private:
    /**
     * @brief Find the handler for a given message type.
     *
     * @return Pointer to handler if found, nullptr otherwise
     */
    message_handler* find_handler(message_type type) const;

    /**
     * @brief Determine the response message type for a request.
     *
     * By convention, response types are request type + 1.
     * This can be overridden in the future if needed.
     */
    message_type get_response_type(message_type request_type) const;

    std::map<message_type_range, std::shared_ptr<message_handler>> handlers_;
};

}

#endif
