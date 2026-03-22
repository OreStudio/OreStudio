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
#ifndef ORES_HTTP_MESSAGING_HTTP_INFO_PROTOCOL_HPP
#define ORES_HTTP_MESSAGING_HTTP_INFO_PROTOCOL_HPP

#include <string>
#include <string_view>

namespace ores::http::messaging {

/**
 * @brief Service discovery request for the HTTP server's base URL.
 *
 * Clients send this request after connecting to NATS to discover the
 * HTTP server's base URL without requiring manual configuration.
 * No authentication is required.
 */
struct get_http_info_request {
    using response_type = struct get_http_info_response;
    static constexpr std::string_view nats_subject =
        "http-server.v1.info.get";
};

/**
 * @brief Response to get_http_info_request.
 *
 * Contains the HTTP server's externally accessible base URL.
 */
struct get_http_info_response {
    /**
     * @brief The HTTP server's base URL, e.g. "http://localhost:51000".
     */
    std::string base_url;
    bool success = false;
    std::string message;
};

}

#endif
