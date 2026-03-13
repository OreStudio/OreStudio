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
#include "ores.comms/messaging/error_protocol.hpp"

#include <rfl/bson.hpp>
#include "ores.utility/rfl/reflectors.hpp"

namespace ores::comms::messaging {

std::vector<std::byte>
error_response::serialize(error_response v) {
    auto bson_data = rfl::bson::write(v);
    return {
        reinterpret_cast<const std::byte*>(bson_data.data()),
        reinterpret_cast<const std::byte*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<error_response, ores::utility::serialization::error_code>
error_response::deserialize(std::span<const std::byte> data) {
    auto result = rfl::bson::read<error_response>(
        reinterpret_cast<const char*>(data.data()), data.size());

    if (!result) {
        return std::unexpected(
            ores::utility::serialization::error_code::invalid_message_type);
    }

    return std::move(*result);
}

frame create_error_response_frame(
    std::uint32_t sequence,
    std::uint32_t correlation_id,
    ores::utility::serialization::error_code code,
    const std::string& message) {

    error_response err{
        .code = code,
        .message = message
    };

    return {
        message_type::error_response,
        sequence,
        correlation_id,
        error_response::serialize(err)
    };
}

}
