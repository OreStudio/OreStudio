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
#ifndef ORES_HTTP_DOMAIN_HTTP_REQUEST_HPP
#define ORES_HTTP_DOMAIN_HTTP_REQUEST_HPP

#include <string>
#include <cstdint>
#include <optional>
#include <unordered_map>
#include "ores.http/domain/http_method.hpp"
#include "ores.http/domain/jwt_claims.hpp"

namespace ores::http::domain {

/**
 * @brief Represents an incoming HTTP request.
 */
struct http_request final {
    /**
     * @brief HTTP method (GET, POST, etc.)
     */
    http_method method;

    /**
     * @brief Request target/path.
     */
    std::string target;

    /**
     * @brief Request headers.
     */
    std::unordered_map<std::string, std::string> headers;

    /**
     * @brief Query parameters parsed from URL.
     */
    std::unordered_map<std::string, std::string> query_params;

    /**
     * @brief Path parameters extracted from route pattern.
     */
    std::unordered_map<std::string, std::string> path_params;

    /**
     * @brief Request body.
     */
    std::string body;

    /**
     * @brief Remote endpoint address.
     */
    std::string remote_address;

    /**
     * @brief HTTP version major number (e.g., 1 for HTTP/1.1).
     */
    std::uint16_t http_version_major = 1;

    /**
     * @brief HTTP version minor number (e.g., 1 for HTTP/1.1).
     */
    std::uint16_t http_version_minor = 1;

    /**
     * @brief Authenticated user claims, if present.
     */
    std::optional<jwt_claims> authenticated_user;

    /**
     * @brief Returns header value or empty string if not found.
     */
    std::string get_header(const std::string& name) const;

    /**
     * @brief Returns query parameter value or empty string if not found.
     */
    std::string get_query_param(const std::string& name) const;

    /**
     * @brief Returns path parameter value or empty string if not found.
     */
    std::string get_path_param(const std::string& name) const;

    /**
     * @brief Returns the Authorization header bearer token if present.
     */
    std::optional<std::string> get_bearer_token() const;
};

}

#endif
