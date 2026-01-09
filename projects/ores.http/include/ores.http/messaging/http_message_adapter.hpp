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
#ifndef ORES_HTTP_MESSAGING_HTTP_MESSAGE_ADAPTER_HPP
#define ORES_HTTP_MESSAGING_HTTP_MESSAGE_ADAPTER_HPP

#include <map>
#include <span>
#include <memory>
#include <string>
#include <expected>
#include <unordered_map>
#include <boost/asio/awaitable.hpp>
#include "ores.http/domain/http_request.hpp"
#include "ores.http/domain/http_response.hpp"
#include "ores.http/domain/http_method.hpp"
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.comms/messaging/message_types.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::http::messaging {

/**
 * @brief Route mapping from HTTP endpoint to message type.
 */
struct endpoint_mapping final {
    domain::http_method http_method;
    std::string path_pattern;
    comms::messaging::message_type message_type;
    bool requires_auth = true;
    std::string description;
};

/**
 * @brief Adapter that bridges HTTP requests to the existing binary message handlers.
 *
 * This allows reusing the same domain logic exposed via both the binary protocol
 * (ores.comms) and the REST API (ores.http).
 *
 * The adapter:
 * 1. Maps HTTP endpoints (method + path) to message types
 * 2. Converts JSON request body to binary format expected by handlers
 * 3. Routes to existing message handlers
 * 4. Converts binary response back to JSON
 */
class http_message_adapter final {
public:
    http_message_adapter();

    /**
     * @brief Registers a message handler for a range of message types.
     */
    void register_handler(comms::messaging::message_type_range range,
        std::shared_ptr<comms::messaging::message_handler> handler);

    /**
     * @brief Maps an HTTP endpoint to a message type.
     */
    void map_endpoint(const endpoint_mapping& mapping);

    /**
     * @brief Handles an HTTP request by routing to the appropriate message handler.
     */
    boost::asio::awaitable<domain::http_response> handle(
        const domain::http_request& request);

    /**
     * @brief Gets all registered endpoint mappings (for OpenAPI generation).
     */
    const std::vector<endpoint_mapping>& get_mappings() const { return mappings_; }

private:
    inline static std::string_view logger_name =
        "ores.http.messaging.http_message_adapter";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /**
     * @brief Finds the handler for a given message type.
     */
    comms::messaging::message_handler* find_handler(
        comms::messaging::message_type type) const;

    /**
     * @brief Converts JSON body to binary payload.
     */
    std::expected<std::vector<std::byte>, std::string> json_to_binary(
        const std::string& json) const;

    /**
     * @brief Converts binary payload to JSON.
     */
    std::expected<std::string, std::string> binary_to_json(
        std::span<const std::byte> payload) const;

    std::vector<endpoint_mapping> mappings_;
    std::map<comms::messaging::message_type_range,
        std::shared_ptr<comms::messaging::message_handler>> handlers_;
};

}

#endif
