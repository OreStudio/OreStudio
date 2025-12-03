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
#include "ores.comms/messaging/heartbeat_protocol.hpp"

#include <rfl.hpp>
#include <rfl/bson.hpp>

namespace ores::comms::messaging {

std::vector<std::byte>
ping::serialize(ping v) {
    auto bson_data = rfl::bson::write(v);
    return {
        reinterpret_cast<const std::byte*>(bson_data.data()),
        reinterpret_cast<const std::byte*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<ping, error_code>
ping::deserialize(std::span<const std::byte> data) {
    auto result = rfl::bson::read<ping>(reinterpret_cast<const char*>(data.data()), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

std::vector<std::byte>
pong::serialize(pong v) {
    auto bson_data = rfl::bson::write(v);
    return {
        reinterpret_cast<const std::byte*>(bson_data.data()),
        reinterpret_cast<const std::byte*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<pong, error_code>
pong::deserialize(std::span<const std::byte> data) {
    auto result = rfl::bson::read<pong>(reinterpret_cast<const char*>(data.data()), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

// Frame creation functions
frame create_ping_frame(std::uint32_t sequence) {
    ping p{};
    return {message_type::ping, sequence, p.serialize(p)};
}

frame create_pong_frame(std::uint32_t sequence) {
    pong p{};
    return {message_type::pong, sequence, p.serialize(p)};
}

}
