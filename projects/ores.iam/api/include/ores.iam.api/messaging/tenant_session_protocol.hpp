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
#ifndef ORES_IAM_API_MESSAGING_TENANT_SESSION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_TENANT_SESSION_PROTOCOL_HPP

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
 * @brief Enter one tenant as a system administrator, reading only.
 *
 * Refused to a caller outside the system tenant, to a caller already inside a
 * tenant, to a caller without iam::tenants:impersonate, and for the system
 * tenant, an unknown tenant, or a tenant with no system party yet.
 */
struct enter_tenant_request {
    using response_type = struct enter_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.enter_tenant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The id of the tenant to enter.
     */
    std::string tenant_id;

    friend bool operator==(const enter_tenant_request&, const enter_tenant_request&) = default;
};

struct enter_tenant_response {
    bool success = false;
    /**
     * @brief Why the entry was refused, or empty.
     */
    std::string message;
    /**
     * @brief The session token scoped to the tenant.
     */
    std::string token;
    std::string tenant_id;
    std::string tenant_code;
    /**
     * @brief The tenant's name, which the screens show while inside.
     */
    std::string tenant_name;
    /**
     * @brief The tenant's system party, which the session acts as.
     */
    std::string party_id;
    /**
     * @brief The system party's name, which the screens show while inside.
     */
    std::string party_name;
    /**
     * @brief How long the session lasts. It is not refreshed.
     */
    int access_lifetime_s = 1800;

    friend bool operator==(const enter_tenant_response&, const enter_tenant_response&) = default;
};

/**
 * @brief Leave the tenant the caller's session acts in.
 *
 * The tenant comes from the caller's token, so the request carries nothing.
 * Refused to a session that is not inside a tenant.
 */
struct leave_tenant_request {
    using response_type = struct leave_tenant_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.leave_tenant";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const leave_tenant_request&, const leave_tenant_request&) = default;
};

struct leave_tenant_response {
    bool success = false;
    std::string message;

    friend bool operator==(const leave_tenant_response&, const leave_tenant_response&) = default;
};

/**
 * @brief The members of enter_tenant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<enter_tenant_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"tenant_id", &enter_tenant_request::tenant_id},
    };
}

/**
 * @brief The members of enter_tenant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<enter_tenant_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &enter_tenant_response::success},
        ::ores::utility::codec::field{"message", &enter_tenant_response::message},
        ::ores::utility::codec::field{"token", &enter_tenant_response::token},
        ::ores::utility::codec::field{"tenant_id", &enter_tenant_response::tenant_id},
        ::ores::utility::codec::field{"tenant_code", &enter_tenant_response::tenant_code},
        ::ores::utility::codec::field{"tenant_name", &enter_tenant_response::tenant_name},
        ::ores::utility::codec::field{"party_id", &enter_tenant_response::party_id},
        ::ores::utility::codec::field{"party_name", &enter_tenant_response::party_name},
        ::ores::utility::codec::field{"access_lifetime_s",
                                      &enter_tenant_response::access_lifetime_s},
    };
}

/**
 * @brief The members of leave_tenant_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<leave_tenant_request>) {
    return std::tuple{};
}

/**
 * @brief The members of leave_tenant_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<leave_tenant_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &leave_tenant_response::success},
        ::ores::utility::codec::field{"message", &leave_tenant_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::enter_tenant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::enter_tenant_request& v);
    static ores::iam::messaging::enter_tenant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::enter_tenant_request& v);
    static ores::iam::messaging::enter_tenant_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::enter_tenant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::enter_tenant_response& v);
    static ores::iam::messaging::enter_tenant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::enter_tenant_response& v);
    static ores::iam::messaging::enter_tenant_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::leave_tenant_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::leave_tenant_request& v);
    static ores::iam::messaging::leave_tenant_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::leave_tenant_request& v);
    static ores::iam::messaging::leave_tenant_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::leave_tenant_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::leave_tenant_response& v);
    static ores::iam::messaging::leave_tenant_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::leave_tenant_response& v);
    static ores::iam::messaging::leave_tenant_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
