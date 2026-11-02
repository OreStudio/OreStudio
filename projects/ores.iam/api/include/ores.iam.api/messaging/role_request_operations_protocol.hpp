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
#ifndef ORES_IAM_API_MESSAGING_ROLE_REQUEST_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_REQUEST_OPERATIONS_PROTOCOL_HPP

#include "ores.iam.api/domain/role.hpp"
#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief Asks for roles for the signed-in person.
 *
 * Refused when a role is unknown, is not one the tenant offers to its
 * members, is already held, or is already asked for in a request that still
 * waits.
 */
struct ask_for_roles_request {
    using response_type = struct ask_for_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.ask_for_roles";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The roles asked for, as UUID strings.
     */
    std::vector<std::string> role_ids;
    /**
     * @brief Why the person asks, in their words.
     */
    std::string reason;

    friend bool operator==(const ask_for_roles_request&, const ask_for_roles_request&) = default;
};

struct ask_for_roles_response {
    ores::utility::domain::result result;
    /**
     * @brief The approval request raised, when the outcome is ok.
     */
    std::string request_id;

    friend bool operator==(const ask_for_roles_response&, const ask_for_roles_response&) = default;
};

/**
 * @brief Reads the roles one approval request asks for.
 *
 * Answered when the caller raised the request, and when the caller may read
 * the roles of role grant requests. Answered as not found otherwise, so a
 * caller learns nothing about a request they may not read.
 */
struct get_request_roles_request {
    using response_type = struct get_request_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_request_roles";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The approval request to read the roles of, as a UUID string.
     */
    std::string request_id;

    friend bool operator==(const get_request_roles_request&,
                           const get_request_roles_request&) = default;
};

/**
 * @brief One role a request asks for, with the request's own record of it.
 *
 * The role is the catalogue row, whole, so a screen draws it without a second
 * read. The tail is the junction row the ask wrote: when it was written, and
 * when IAM applied it afterwards and on whose authority. It is what the
 * request's story needs to say that a role was given, and the only place that
 * fact is kept.
 */
struct requested_role {
    ores::iam::domain::role role;
    /**
     * @brief When the request recorded the role, which is when the person asked.
     */
    std::chrono::system_clock::time_point asked_at;
    /**
     * @brief When IAM applied the role after the request was approved, or empty
     * until it has. A role the person already held is marked applied without a
     * grant, so this says the request was dealt with rather than that a role moved.
     */
    std::optional<std::chrono::system_clock::time_point> applied_at;
    /**
     * @brief Who applied it, or empty until somebody has.
     */
    std::string applied_by;

    friend bool operator==(const requested_role&, const requested_role&) = default;
};

struct get_request_roles_response {
    ores::utility::domain::result result;
    std::vector<requested_role> roles;

    friend bool operator==(const get_request_roles_response&,
                           const get_request_roles_response&) = default;
};

/**
 * @brief The members of ask_for_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ask_for_roles_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"role_ids", &ask_for_roles_request::role_ids},
        ::ores::utility::codec::field{"reason", &ask_for_roles_request::reason},
    };
}

/**
 * @brief The members of ask_for_roles_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ask_for_roles_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &ask_for_roles_response::result},
        ::ores::utility::codec::field{"request_id", &ask_for_roles_response::request_id},
    };
}

/**
 * @brief The members of get_request_roles_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_request_roles_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"request_id", &get_request_roles_request::request_id},
    };
}

/**
 * @brief The members of requested_role, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const requested_role&) {
    return std::tuple{
        ::ores::utility::codec::field{"role", &requested_role::role},
        ::ores::utility::codec::field{"asked_at", &requested_role::asked_at},
        ::ores::utility::codec::field{"applied_at", &requested_role::applied_at},
        ::ores::utility::codec::field{"applied_by", &requested_role::applied_by},
    };
}

/**
 * @brief The members of get_request_roles_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_request_roles_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_request_roles_response::result},
        ::ores::utility::codec::field{"roles", &get_request_roles_response::roles},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::ask_for_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::ask_for_roles_request& v);
    static ores::iam::messaging::ask_for_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::ask_for_roles_request& v);
    static ores::iam::messaging::ask_for_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::ask_for_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::ask_for_roles_response& v);
    static ores::iam::messaging::ask_for_roles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::ask_for_roles_response& v);
    static ores::iam::messaging::ask_for_roles_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_request_roles_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_request_roles_request& v);
    static ores::iam::messaging::get_request_roles_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_request_roles_request& v);
    static ores::iam::messaging::get_request_roles_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::requested_role> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::requested_role& v);
    static ores::iam::messaging::requested_role read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::requested_role& v);
    static ores::iam::messaging::requested_role read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_request_roles_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_request_roles_response& v);
    static ores::iam::messaging::get_request_roles_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_request_roles_response& v);
    static ores::iam::messaging::get_request_roles_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
