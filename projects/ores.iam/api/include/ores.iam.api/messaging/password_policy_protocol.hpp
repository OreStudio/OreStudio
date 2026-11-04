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
#ifndef ORES_IAM_API_MESSAGING_PASSWORD_POLICY_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_PASSWORD_POLICY_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::iam::messaging {

struct get_password_policy_request {
    using response_type = struct get_password_policy_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_password_policy";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;

    friend bool operator==(const get_password_policy_request&,
                           const get_password_policy_request&) = default;
};

struct get_password_policy_response {
    bool success = false;
    std::string message;
    int min_length = 0;
    bool require_uppercase = false;
    bool require_lowercase = false;
    bool require_digit = false;
    bool require_special = false;
    std::string special_chars;

    friend bool operator==(const get_password_policy_response&,
                           const get_password_policy_response&) = default;
};

/**
 * @brief The members of get_password_policy_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_password_policy_request>) {
    return std::tuple{};
}

/**
 * @brief The members of get_password_policy_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_password_policy_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_password_policy_response::success},
        ::ores::utility::codec::field{"message", &get_password_policy_response::message},
        ::ores::utility::codec::field{"min_length", &get_password_policy_response::min_length},
        ::ores::utility::codec::field{"require_uppercase",
                                      &get_password_policy_response::require_uppercase},
        ::ores::utility::codec::field{"require_lowercase",
                                      &get_password_policy_response::require_lowercase},
        ::ores::utility::codec::field{"require_digit",
                                      &get_password_policy_response::require_digit},
        ::ores::utility::codec::field{"require_special",
                                      &get_password_policy_response::require_special},
        ::ores::utility::codec::field{"special_chars",
                                      &get_password_policy_response::special_chars},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_password_policy_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_password_policy_request& v);
    static ores::iam::messaging::get_password_policy_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_password_policy_request& v);
    static ores::iam::messaging::get_password_policy_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_password_policy_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_password_policy_response& v);
    static ores::iam::messaging::get_password_policy_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_password_policy_response& v);
    static ores::iam::messaging::get_password_policy_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
