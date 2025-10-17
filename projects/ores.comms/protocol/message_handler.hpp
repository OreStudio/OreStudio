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
#ifndef ORES_COMMS_PROTOCOL_MESSAGE_HANDLER_HPP
#define ORES_COMMS_PROTOCOL_MESSAGE_HANDLER_HPP

#include <span>
#include <vector>
#include <cstdint>
#include <expected>
#include <boost/cobalt.hpp>
#include "ores.comms/protocol/message_types.hpp"

namespace ores::comms::protocol {

namespace cobalt = boost::cobalt;

/**
 * @brief Range of message types handled by a subsystem.
 *
 * Defines an inclusive range [min, max] of message types.
 */
struct message_type_range final {
    std::uint16_t min;
    std::uint16_t max;

    /**
     * @brief Check if a message type falls within this range.
     */
    bool contains(message_type type) const {
        auto type_value = static_cast<std::uint16_t>(type);
        return type_value >= min && type_value <= max;
    }

    /**
     * @brief Comparison operator for use in ordered containers.
     */
    bool operator<(const message_type_range& other) const {
        return max < other.min; // Non-overlapping ranges
    }
};

/**
 * @brief Abstract interface for handling messages from a subsystem.
 *
 * Subsystems implement this interface to process their domain-specific messages.
 * The handler is responsible for:
 * - Deserializing the payload into subsystem-specific types
 * - Executing the requested operation
 * - Serializing the response back into bytes
 */
class message_handler {
public:
    virtual ~message_handler() = default;

    /**
     * @brief Handle a message and produce a response payload.
     *
     * @param type The message type being processed
     * @param payload The raw message payload to deserialize and process
     * @return Expected containing response payload bytes, or error_code on failure
     */
    virtual cobalt::promise<std::expected<std::vector<std::uint8_t>, error_code>>
    handle_message(message_type type, std::span<const std::uint8_t> payload) = 0;
};

}

#endif
