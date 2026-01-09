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
#include "ores.http/messaging/http_message_adapter.hpp"

#include <regex>
#include <rfl/json.hpp>

namespace ores::http::messaging {

using namespace ores::logging;

http_message_adapter::http_message_adapter() {
    BOOST_LOG_SEV(lg(), debug) << "HTTP message adapter created";
}

void http_message_adapter::register_handler(
    comms::messaging::message_type_range range,
    std::shared_ptr<comms::messaging::message_handler> handler) {

    BOOST_LOG_SEV(lg(), info) << "Registering handler for message types "
        << range.min << "-" << range.max;
    handlers_[range] = std::move(handler);
}

void http_message_adapter::map_endpoint(const endpoint_mapping& mapping) {
    BOOST_LOG_SEV(lg(), info) << "Mapping endpoint "
        << static_cast<int>(mapping.http_method) << " " << mapping.path_pattern
        << " -> message type " << static_cast<int>(mapping.message_type);
    mappings_.push_back(mapping);
}

comms::messaging::message_handler* http_message_adapter::find_handler(
    comms::messaging::message_type type) const {

    for (const auto& [range, handler] : handlers_) {
        if (range.contains(type)) {
            return handler.get();
        }
    }
    return nullptr;
}

boost::asio::awaitable<domain::http_response> http_message_adapter::handle(
    const domain::http_request& request) {

    BOOST_LOG_SEV(lg(), debug) << "Handling HTTP request: "
        << static_cast<int>(request.method) << " " << request.target;

    // Extract path without query string
    std::string path = request.target;
    auto query_pos = path.find('?');
    if (query_pos != std::string::npos) {
        path = path.substr(0, query_pos);
    }

    // Find matching endpoint mapping
    for (const auto& mapping : mappings_) {
        if (mapping.http_method != request.method) {
            continue;
        }

        // Simple pattern matching (could be enhanced with regex)
        std::regex pattern(mapping.path_pattern);
        if (std::regex_match(path, pattern)) {
            BOOST_LOG_SEV(lg(), debug) << "Matched endpoint mapping for message type "
                << static_cast<int>(mapping.message_type);

            // Find handler
            auto* handler = find_handler(mapping.message_type);
            if (!handler) {
                BOOST_LOG_SEV(lg(), error) << "No handler registered for message type "
                    << static_cast<int>(mapping.message_type);
                co_return domain::http_response::internal_error(
                    "No handler for endpoint");
            }

            // Convert request body to binary
            auto binary_result = json_to_binary(request.body);
            if (!binary_result) {
                BOOST_LOG_SEV(lg(), warn) << "Failed to convert JSON to binary: "
                    << binary_result.error();
                co_return domain::http_response::bad_request(binary_result.error());
            }

            // Call handler
            auto result = co_await handler->handle_message(
                mapping.message_type,
                std::span<const std::byte>(binary_result.value()),
                request.remote_address);

            if (!result) {
                BOOST_LOG_SEV(lg(), error) << "Handler returned error: "
                    << static_cast<int>(result.error());
                co_return domain::http_response::internal_error(
                    "Handler error: " + std::to_string(static_cast<int>(result.error())));
            }

            // Convert response to JSON
            auto json_result = binary_to_json(result.value());
            if (!json_result) {
                BOOST_LOG_SEV(lg(), error) << "Failed to convert binary to JSON: "
                    << json_result.error();
                co_return domain::http_response::internal_error(json_result.error());
            }

            BOOST_LOG_SEV(lg(), debug) << "Request handled successfully";
            co_return domain::http_response::json(json_result.value());
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "No endpoint mapping found for request";
    co_return domain::http_response::not_found("Endpoint not found");
}

std::expected<std::vector<std::byte>, std::string> http_message_adapter::json_to_binary(
    const std::string& json) const {

    if (json.empty()) {
        return std::vector<std::byte>{};
    }

    // For now, just convert the JSON string to bytes
    // The actual conversion depends on how the message handlers expect data
    std::vector<std::byte> result;
    result.reserve(json.size());
    for (char c : json) {
        result.push_back(static_cast<std::byte>(c));
    }
    return result;
}

std::expected<std::string, std::string> http_message_adapter::binary_to_json(
    std::span<const std::byte> payload) const {

    if (payload.empty()) {
        return "{}";
    }

    // For now, just convert bytes back to string
    // The actual conversion depends on how the message handlers return data
    std::string result;
    result.reserve(payload.size());
    for (std::byte b : payload) {
        result.push_back(static_cast<char>(b));
    }
    return result;
}

}
