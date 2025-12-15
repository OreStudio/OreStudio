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
#include "ores.comms/messaging/handshake_protocol.hpp"

#include <rfl.hpp>
#include <rfl/bson.hpp>
#include "ores.utility/log/make_logger.hpp"

namespace {

std::string_view logger_name = "ores.comms.messaging.handshake";

auto& lg() {
    using namespace ores::utility::log;
    static auto instance = make_logger(logger_name);
    return instance;
}

}

namespace ores::comms::messaging {

using namespace ores::utility::log;

std::vector<std::byte>
handshake_request::serialize(handshake_request v) {
    auto bson_data = rfl::bson::write(v);
    return {
        reinterpret_cast<const std::byte*>(bson_data.data()),
        reinterpret_cast<const std::byte*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<handshake_request, error_code>
handshake_request::deserialize(std::span<const std::byte> data) {
    auto result = rfl::bson::read<handshake_request>(reinterpret_cast<const char*>(data.data()), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

std::vector<std::byte>
handshake_response::serialize(handshake_response v) {
    auto bson_data = rfl::bson::write(v);
    return {
        reinterpret_cast<const std::byte*>(bson_data.data()),
        reinterpret_cast<const std::byte*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<handshake_response, error_code> handshake_response::
deserialize(std::span<const std::byte> data) {
    auto result = rfl::bson::read<handshake_response>(reinterpret_cast<const char*>(data.data()), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

std::vector<std::byte>
handshake_ack::serialize(handshake_ack v) {
    auto bson_data = rfl::bson::write(v);
    return {
        reinterpret_cast<const std::byte*>(bson_data.data()),
        reinterpret_cast<const std::byte*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<handshake_ack, error_code> handshake_ack::
deserialize(std::span<const std::byte> data) {
    auto result = rfl::bson::read<handshake_ack>(reinterpret_cast<const char*>(data.data()), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

std::vector<std::byte>
error_response::serialize(error_response v) {
    auto bson_data = rfl::bson::write(v);
    return {
        reinterpret_cast<const std::byte*>(bson_data.data()),
        reinterpret_cast<const std::byte*>(bson_data.data()) + bson_data.size()
    };
}

std::expected<error_response, error_code> error_response::
deserialize(std::span<const std::byte> data) {
    auto result = rfl::bson::read<error_response>(reinterpret_cast<const char*>(data.data()), data.size());

    if (!result) {
        return std::unexpected(error_code::invalid_message_type);
    }

    return result.value();
}

// Frame creation functions
frame create_handshake_request_frame(
    std::uint32_t sequence,
    const std::string& client_identifier,
    std::uint8_t supported_compression) {

    handshake_request req {
        .client_version_major = PROTOCOL_VERSION_MAJOR,
        .client_version_minor = PROTOCOL_VERSION_MINOR,
        .client_identifier = client_identifier,
        .supported_compression = supported_compression
    };

    BOOST_LOG_SEV(lg(), info) << "Creating handshake request: client='" << client_identifier
                              << "', supported_compression=0x" << std::hex
                              << static_cast<int>(supported_compression) << std::dec
                              << " (zlib=" << ((supported_compression & COMPRESSION_SUPPORT_ZLIB) ? "yes" : "no")
                              << ", gzip=" << ((supported_compression & COMPRESSION_SUPPORT_GZIP) ? "yes" : "no")
                              << ", bzip2=" << ((supported_compression & COMPRESSION_SUPPORT_BZIP2) ? "yes" : "no")
                              << ")";

    return {message_type::handshake_request, sequence, req.serialize(req)};
}

frame create_handshake_response_frame(
    std::uint32_t sequence,
    bool version_compatible,
    const std::string& server_identifier,
    error_code status,
    compression_type selected_compression) {

    handshake_response resp{
        .server_version_major = PROTOCOL_VERSION_MAJOR,
        .server_version_minor = PROTOCOL_VERSION_MINOR,
        .version_compatible = version_compatible,
        .server_identifier = server_identifier,
        .status = status,
        .selected_compression = selected_compression
    };

    BOOST_LOG_SEV(lg(), info) << "Creating handshake response: server='" << server_identifier
                              << "', version_compatible=" << (version_compatible ? "yes" : "no")
                              << ", selected_compression=" << selected_compression;

    return { message_type::handshake_response, sequence, resp.serialize(resp) };
}

frame create_handshake_ack_frame(
    std::uint32_t sequence,
    error_code status) {

    handshake_ack ack{status};

    return { message_type::handshake_ack, sequence, ack.serialize(ack) };
}

frame create_error_response_frame(
    std::uint32_t sequence,
    error_code code,
    const std::string& message) {

    error_response err{
        .code = code,
        .message = message
    };

    return { message_type::error_response, sequence, err.serialize(err) };
}

compression_type select_compression(
    std::uint8_t supported_compression,
    compression_type preferred) {

    // Map compression type to its bitmask value
    auto type_to_mask = [](compression_type ct) -> std::uint8_t {
        switch (ct) {
            case compression_type::zlib: return COMPRESSION_SUPPORT_ZLIB;
            case compression_type::gzip: return COMPRESSION_SUPPORT_GZIP;
            case compression_type::bzip2: return COMPRESSION_SUPPORT_BZIP2;
            default: return 0;
        }
    };

    compression_type selected = compression_type::none;

    // Check if preferred type is supported
    if (supported_compression & type_to_mask(preferred)) {
        selected = preferred;
    } else if (supported_compression & COMPRESSION_SUPPORT_ZLIB) {
        selected = compression_type::zlib;
    } else if (supported_compression & COMPRESSION_SUPPORT_GZIP) {
        selected = compression_type::gzip;
    } else if (supported_compression & COMPRESSION_SUPPORT_BZIP2) {
        selected = compression_type::bzip2;
    }

    if (selected == compression_type::none) {
        BOOST_LOG_SEV(lg(), info) << "Compression negotiation: no compression "
                                  << "(client supported=0x" << std::hex
                                  << static_cast<int>(supported_compression) << std::dec << ")";
    } else {
        BOOST_LOG_SEV(lg(), info) << "Compression negotiation: selected " << selected
                                  << " (client supported=0x" << std::hex
                                  << static_cast<int>(supported_compression) << std::dec
                                  << ", server preferred=" << preferred << ")";
    }

    return selected;
}

}
