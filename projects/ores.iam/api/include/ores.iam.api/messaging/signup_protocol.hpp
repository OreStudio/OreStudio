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
#ifndef ORES_IAM_API_MESSAGING_SIGNUP_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SIGNUP_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::iam::messaging {

struct signup_request {
    using response_type = struct signup_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.signup";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    std::string principal;
    std::string password;
    std::string email;
    /**
     * @brief The address the registration arrived at.
     *
     * The address names the tenant the account lands in: the service resolves
     * it by hostname and uses the tenant flagged @c is_registration_default
     * only when the address names none. Without it a registration lands in the
     * system tenant by omission, which is the defect the survey found.
     */
    std::string hostname;

    friend bool operator==(const signup_request&, const signup_request&) = default;
};

struct signup_response {
    bool success = false;
    std::string message;
    std::string account_id;
    /**
     * @brief The state the account was created in: @c active or @c pending.
     *
     * An account is active when its tenant nominated a default party, so it
     * received an association and can sign in at once. It is pending when the
     * tenant nominated none: the account exists, but it waits for an
     * administrator to finish setting it up.
     */
    std::string account_status;
    /**
     * @brief The party the account received, when the tenant nominated one.
     *
     * Empty when the account is pending, because a pending account has no
     * association yet.
     */
    std::string party_id;
    /**
     * @brief The role the account received: the tenant's nominated default.
     *
     * Empty when the tenant nominated no role.
     */
    std::string role_id;
    /**
     * @brief The stable code a client branches on.
     *
     * Empty when the registration succeeded. The message beside it is for a
     * person and may change; this is what a screen branches on, so a refusal is
     * a value rather than a sentence to match. The codes a registration can
     * answer with are collected on the Entry journeys page.
     */
    std::string error_code;

    friend bool operator==(const signup_response&, const signup_response&) = default;
};

/**
 * @brief The members of signup_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<signup_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"principal", &signup_request::principal},
        ::ores::utility::codec::field{"password", &signup_request::password},
        ::ores::utility::codec::field{"email", &signup_request::email},
        ::ores::utility::codec::field{"hostname", &signup_request::hostname},
    };
}

/**
 * @brief The members of signup_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<signup_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &signup_response::success},
        ::ores::utility::codec::field{"message", &signup_response::message},
        ::ores::utility::codec::field{"account_id", &signup_response::account_id},
        ::ores::utility::codec::field{"account_status", &signup_response::account_status},
        ::ores::utility::codec::field{"party_id", &signup_response::party_id},
        ::ores::utility::codec::field{"role_id", &signup_response::role_id},
        ::ores::utility::codec::field{"error_code", &signup_response::error_code},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::signup_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::signup_request& v);
    static ores::iam::messaging::signup_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::signup_request& v);
    static ores::iam::messaging::signup_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::signup_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::signup_response& v);
    static ores::iam::messaging::signup_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::signup_response& v);
    static ores::iam::messaging::signup_response read_msgpack(std::span<const std::byte> data);
};
}


#endif
