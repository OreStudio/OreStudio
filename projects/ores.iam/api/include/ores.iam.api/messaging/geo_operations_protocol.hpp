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
#ifndef ORES_IAM_API_MESSAGING_GEO_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_GEO_OPERATIONS_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::iam::messaging {

/**
 * @brief Resolve one address to the country code it came from.
 *
 * The search is the caller's tenant's ranges. An address the tenant's ranges
 * do not cover is not found, which is an answer rather than an error: a
 * private address never resolves.
 */
struct lookup_country_request {
    using response_type = struct lookup_country_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.lookup_country";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string address;

    friend bool operator==(const lookup_country_request&, const lookup_country_request&) = default;
};

struct lookup_country_response {
    std::string country_code;
    bool found = false;
    bool success = false;
    std::string message;

    friend bool operator==(const lookup_country_response&,
                           const lookup_country_response&) = default;
};

/**
 * @brief The members of lookup_country_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<lookup_country_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"address", &lookup_country_request::address},
    };
}

/**
 * @brief The members of lookup_country_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<lookup_country_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"country_code", &lookup_country_response::country_code},
        ::ores::utility::codec::field{"found", &lookup_country_response::found},
        ::ores::utility::codec::field{"success", &lookup_country_response::success},
        ::ores::utility::codec::field{"message", &lookup_country_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::lookup_country_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::lookup_country_request& v);
    static ores::iam::messaging::lookup_country_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::lookup_country_request& v);
    static ores::iam::messaging::lookup_country_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::lookup_country_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::lookup_country_response& v);
    static ores::iam::messaging::lookup_country_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::lookup_country_response& v);
    static ores::iam::messaging::lookup_country_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
