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
#ifndef ORES_HTTP_DOMAIN_HTTP_RESPONSE_HPP
#define ORES_HTTP_DOMAIN_HTTP_RESPONSE_HPP

#include <string>
#include <unordered_map>
#include "ores.http/domain/http_status.hpp"

namespace ores::http::domain {

/**
 * @brief Represents an HTTP response to be sent to the client.
 */
struct http_response final {
    /**
     * @brief HTTP status code.
     */
    http_status status = http_status::ok;

    /**
     * @brief Response headers.
     */
    std::unordered_map<std::string, std::string> headers;

    /**
     * @brief Response body.
     */
    std::string body;

    /**
     * @brief Content type header value.
     */
    std::string content_type = "application/json";

    /**
     * @brief Creates a successful JSON response.
     */
    static http_response json(const std::string& body,
        http_status status = http_status::ok);

    /**
     * @brief Creates an error response.
     */
    static http_response error(http_status status, const std::string& message);

    /**
     * @brief Creates a 404 Not Found response.
     */
    static http_response not_found(const std::string& message = "Not Found");

    /**
     * @brief Creates a 401 Unauthorized response.
     */
    static http_response unauthorized(const std::string& message = "Unauthorized");

    /**
     * @brief Creates a 403 Forbidden response.
     */
    static http_response forbidden(const std::string& message = "Forbidden");

    /**
     * @brief Creates a 400 Bad Request response.
     */
    static http_response bad_request(const std::string& message);

    /**
     * @brief Creates a 500 Internal Server Error response.
     */
    static http_response internal_error(const std::string& message);

    /**
     * @brief Sets a response header.
     */
    http_response& set_header(const std::string& name, const std::string& value);
};

}

#endif
