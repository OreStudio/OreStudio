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
#ifndef ORES_HTTP_DOMAIN_ROUTE_HPP
#define ORES_HTTP_DOMAIN_ROUTE_HPP

#include <regex>
#include <string>
#include <vector>
#include <optional>
#include <functional>
#include <boost/asio/awaitable.hpp>
#include "ores.http/domain/http_method.hpp"
#include "ores.http/domain/http_request.hpp"
#include "ores.http/domain/http_response.hpp"

namespace ores::http::domain {

/**
 * @brief Describes a query parameter for OpenAPI.
 */
struct query_param final {
    std::string name;
    std::string type = "string";
    std::string format;           // Optional format (e.g., uuid, date)
    bool required = false;
    std::string description;
    std::optional<std::string> default_value;
};

/**
 * @brief Describes the request body schema for OpenAPI.
 *
 * Uses rfl::json::to_schema<T>() to generate the JSON schema automatically
 * from C++ types, ensuring the schema always matches the actual serialization.
 */
struct request_body_schema final {
    std::string content_type = "application/json";
    bool required = true;
    std::string json_schema;   // JSON schema from rfl::json::to_schema<T>()
    std::string example_json;  // Example JSON from generator
};

/**
 * @brief Describes the response schema for OpenAPI.
 *
 * Uses rfl::json::to_schema<T>() to generate the JSON schema automatically
 * from C++ types, ensuring the schema always matches the actual serialization.
 */
struct response_schema final {
    std::string status_code = "200";
    std::string description = "Successful response";
    std::string content_type = "application/json";
    std::string json_schema;   // JSON schema from rfl::json::to_schema<T>()
    std::string example_json;  // Example JSON from generator
};

/**
 * @brief Handler function type for HTTP requests.
 */
using request_handler = std::function<
    boost::asio::awaitable<http_response>(const http_request&)>;

/**
 * @brief Represents a registered route with pattern matching.
 */
struct route final {
    /**
     * @brief HTTP method for this route.
     */
    http_method method;

    /**
     * @brief Original path pattern (e.g., "/users/{id}").
     */
    std::string pattern;

    /**
     * @brief Compiled regex for matching.
     */
    std::regex regex;

    /**
     * @brief Names of path parameters in order.
     */
    std::vector<std::string> param_names;

    /**
     * @brief Handler function for the route.
     */
    request_handler handler;

    /**
     * @brief Whether this route requires authentication.
     */
    bool requires_auth = false;

    /**
     * @brief Required roles for accessing this route (empty = any authenticated user).
     */
    std::vector<std::string> required_roles;

    /**
     * @brief OpenAPI summary for documentation.
     */
    std::string summary;

    /**
     * @brief OpenAPI description for documentation.
     */
    std::string description;

    /**
     * @brief OpenAPI tags for grouping.
     */
    std::vector<std::string> tags;

    /**
     * @brief Query parameters for this route.
     */
    std::vector<query_param> query_params;

    /**
     * @brief Request body schema for POST/PUT/PATCH routes.
     */
    std::optional<request_body_schema> body_schema;

    /**
     * @brief Response schema for the 200 response.
     */
    std::optional<response_schema> success_response_schema;

    /**
     * @brief Attempts to match the given path and extracts parameters.
     * @returns True if matched, with path_params populated.
     */
    bool match(const std::string& path,
        std::unordered_map<std::string, std::string>& path_params) const;
};

}

#endif
