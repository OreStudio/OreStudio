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
#ifndef ORES_HTTP_DOMAIN_HTTP_METHOD_HPP
#define ORES_HTTP_DOMAIN_HTTP_METHOD_HPP

#include <cstdint>
#include <string_view>

namespace ores::http::domain {

/**
 * @brief HTTP methods supported by the server.
 */
enum class http_method : std::uint8_t { get, post, put, patch, delete_, head, options };

/**
 * @brief The method as a caller of the API spells it.
 */
inline std::string_view method_name(http_method method) {
    switch (method) {
        case http_method::get:
            return "GET";
        case http_method::post:
            return "POST";
        case http_method::put:
            return "PUT";
        case http_method::patch:
            return "PATCH";
        case http_method::delete_:
            return "DELETE";
        case http_method::head:
            return "HEAD";
        case http_method::options:
            return "OPTIONS";
    }
    return "UNKNOWN";
}

}

#endif
