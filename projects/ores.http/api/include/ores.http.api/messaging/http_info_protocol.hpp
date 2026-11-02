/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_HTTP_API_MESSAGING_HTTP_INFO_PROTOCOL_HPP
#define ORES_HTTP_API_MESSAGING_HTTP_INFO_PROTOCOL_HPP

#include "ores.http.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::http::messaging {

/**
 * @brief Asks for the HTTP server's base URL.
 *
 * It carries no fields, because the answer is the same for every caller.
 * Requires a valid session: the base URL is internal infrastructure and
 * is not exposed to unauthenticated callers.
 */
struct get_http_info_request {
    using response_type = struct get_http_info_response;
    static constexpr std::string_view nats_subject = "http.v1.info.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The HTTP server's externally reachable base URL.
 */
struct get_http_info_response {
    /**
     * @brief The base URL, for example "http://localhost:51000".
     */
    std::string base_url;
    /**
     * @brief Whether the base URL was produced.
     */
    bool success = false;
    /**
     * @brief Why it was not, when it was not.
     */
    std::string message;
};

/**
 * @brief The members of get_http_info_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_http_info_request&) {
    return std::tuple{};
}

/**
 * @brief The members of get_http_info_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_http_info_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"base_url", &get_http_info_response::base_url},
        ::ores::utility::codec::field{"success", &get_http_info_response::success},
        ::ores::utility::codec::field{"message", &get_http_info_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_HTTP_API_EXPORT wire_traits<ores::http::messaging::get_http_info_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::http::messaging::get_http_info_request& v);
    static ores::http::messaging::get_http_info_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::http::messaging::get_http_info_request& v);
    static ores::http::messaging::get_http_info_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_HTTP_API_EXPORT wire_traits<ores::http::messaging::get_http_info_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::http::messaging::get_http_info_response& v);
    static ores::http::messaging::get_http_info_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::http::messaging::get_http_info_response& v);
    static ores::http::messaging::get_http_info_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
