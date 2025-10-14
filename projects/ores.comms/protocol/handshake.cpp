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
#include <rfl.hpp>
#include <rfl/bson.hpp>
#include "ores.comms/protocol/handshake.hpp"

namespace ores::comms::protocol {

// handshake_request implementation
std::vector<uint8_t> handshake_request::serialize() const {
    auto bson_data = rfl::bson::write(*this);
    return {
        reinterpret_cast<const uint8_t*>(bson_data.data()),
        reinterpret_cast<const uint8_t*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<handshake_request, error_code> handshake_request::deserialize(std::span<const uint8_t> data) {
    auto result = rfl::bson::read<handshake_request>(data.data(), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

// handshake_response implementation
std::vector<uint8_t> handshake_response::serialize() const {
    auto bson_data = rfl::bson::write(*this);
    return {
        reinterpret_cast<const uint8_t*>(bson_data.data()),
        reinterpret_cast<const uint8_t*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<handshake_response, error_code> handshake_response::deserialize(std::span<const uint8_t> data) {
    auto result = rfl::bson::read<handshake_response>(data.data(), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

// handshake_ack implementation
std::vector<uint8_t> handshake_ack::serialize() const {
    auto bson_data = rfl::bson::write(*this);
    return {
        reinterpret_cast<const uint8_t*>(bson_data.data()),
        reinterpret_cast<const uint8_t*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<handshake_ack, error_code> handshake_ack::deserialize(std::span<const uint8_t> data) {
    auto result = rfl::bson::read<handshake_ack>(data.data(), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

// Frame creation functions
frame create_handshake_request_frame(
    uint32_t sequence,
    const std::string& client_identifier) {

    handshake_request req {
        .client_version_major = PROTOCOL_VERSION_MAJOR,
        .client_version_minor = PROTOCOL_VERSION_MINOR,
        .client_identifier = client_identifier
    };

    return {message_type::handshake_request, sequence, req.serialize()};
}

frame create_handshake_response_frame(
    uint32_t sequence,
    bool version_compatible,
    const std::string& server_identifier,
    error_code status) {

    handshake_response resp{
        .server_version_major = PROTOCOL_VERSION_MAJOR,
        .server_version_minor = PROTOCOL_VERSION_MINOR,
        .version_compatible = version_compatible,
        .server_identifier = server_identifier,
        .status = status
    };

    return { message_type::handshake_response, sequence, resp.serialize() };
}

frame create_handshake_ack_frame(
    uint32_t sequence,
    error_code status) {

    handshake_ack ack{status};

    return { message_type::handshake_ack, sequence, ack.serialize() };
}

}
