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
#ifndef ORES_HTTP_NET_HTTP_SERVER_OPTIONS_HPP
#define ORES_HTTP_NET_HTTP_SERVER_OPTIONS_HPP

#include <string>
#include <cstdint>
#include <chrono>
#include <iosfwd>

namespace ores::http::net {

/**
 * @brief Configuration options for the HTTP server.
 */
struct http_server_options final {
    /**
     * @brief Address to bind to (default: 0.0.0.0).
     */
    std::string address = "0.0.0.0";

    /**
     * @brief Port to listen on.
     */
    std::uint16_t port = 8080;

    /**
     * @brief Maximum number of concurrent connections.
     */
    std::uint32_t max_connections = 100;

    /**
     * @brief Request timeout duration.
     */
    std::chrono::seconds request_timeout{30};

    /**
     * @brief Whether to enable SSL/TLS.
     */
    bool enable_ssl = false;

    /**
     * @brief Path to SSL certificate file.
     */
    std::string certificate_file;

    /**
     * @brief Path to SSL private key file.
     */
    std::string private_key_file;

    /**
     * @brief JWT secret for authentication (if using symmetric key).
     */
    std::string jwt_secret;

    /**
     * @brief Path to JWT public key file (if using RSA).
     */
    std::string jwt_public_key_file;

    /**
     * @brief JWT issuer for validation.
     */
    std::string jwt_issuer = "ores";

    /**
     * @brief JWT audience for validation.
     */
    std::string jwt_audience = "ores-api";

    /**
     * @brief Enable CORS support.
     */
    bool enable_cors = true;

    /**
     * @brief Allowed origins for CORS (empty = all).
     */
    std::string cors_allowed_origins = "*";

    /**
     * @brief Server identifier for responses.
     */
    std::string server_identifier = "ores-http-server";
};

std::ostream& operator<<(std::ostream& s, const http_server_options& v);

}

#endif
