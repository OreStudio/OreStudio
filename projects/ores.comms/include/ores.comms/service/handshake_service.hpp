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
#ifndef ORES_COMMS_SERVICE_HANDSHAKE_SERVICE_HPP
#define ORES_COMMS_SERVICE_HANDSHAKE_SERVICE_HPP

#include <string>
#include <cstdint>
#include <optional>
#include <functional>
#include <boost/asio/awaitable.hpp>
#include "ores.utility/log/make_logger.hpp"
#include "ores.comms/messaging/message_types.hpp"

namespace ores::comms::net { class connection; }

namespace ores::comms::service {

/**
 * @brief Service for managing protocol handshake between client and server.
 *
 * Encapsulates the handshake protocol logic for both client and server sides,
 * handling the three-way handshake:
 * 1. Client sends handshake_request
 * 2. Server responds with handshake_response (including version compatibility)
 * 3. Client sends handshake_ack to complete
 */
class handshake_service final {
private:
    inline static std::string_view logger_name =
        "ores.comms.service.handshake_service";

    static auto& lg() {
        using namespace ores::utility::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Perform client-side handshake.
     *
     * Sends handshake request, waits for response, validates compatibility,
     * and sends acknowledgment.
     *
     * @param conn Connection to perform handshake on
     * @param sequence_generator Function that generates sequence numbers
     * @param client_identifier Client identifier string
     * @param supported_compression Bitmask of supported compression types
     *        (default: 0 = no compression support)
     * @return The compression type selected by the server for this session
     * @throws connection_error if handshake fails
     */
    static boost::asio::awaitable<messaging::compression_type> perform_client_handshake(
        net::connection& conn,
        const std::function<std::uint32_t()>& sequence_generator,
        const std::string& client_identifier,
        std::uint8_t supported_compression = 0);

    /**
     * @brief Perform server-side handshake.
     *
     * Reads handshake request, validates version, sends response,
     * and waits for acknowledgment.
     *
     * @param conn Connection to perform handshake on
     * @param sequence Sequence number for response frame
     * @param server_identifier Server identifier string
     * @return The negotiated compression type on success, std::nullopt on failure
     */
    static boost::asio::awaitable<std::optional<messaging::compression_type>>
    perform_server_handshake(
        net::connection& conn,
        std::uint32_t sequence,
        const std::string& server_identifier);
};

}

#endif
