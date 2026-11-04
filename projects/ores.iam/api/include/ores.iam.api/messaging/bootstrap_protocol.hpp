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
#ifndef ORES_IAM_API_MESSAGING_BOOTSTRAP_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_BOOTSTRAP_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>

namespace ores::iam::messaging {

struct bootstrap_status_request {
    using response_type = struct bootstrap_status_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.bootstrap_status";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;

    friend bool operator==(const bootstrap_status_request&,
                           const bootstrap_status_request&) = default;
};

struct bootstrap_status_response {
    bool is_in_bootstrap_mode = false;
    std::string message;
    /**
     * @brief Whether the deployment has a tenant of its own.
     *
     * The system tenant is the deployment's own bookkeeping and not a tenant
     * somebody set up, so it does not count. A deployment without one has not
     * been set up: it has no work in it, and the screen that brings it to life
     * is still the screen a person belongs on, however far through that screen
     * they got before they closed the browser.
     */
    bool has_tenant = false;
    /**
     * @brief The build the answering service runs, in full.
     *
     * The read that reaches a browser before it has a session is the one a screen
     * can state a deployment's version from, and the version belongs to the
     * deployment rather than to the bootstrap question: an interface shows it
     * after the administrator exists as well. A client's own version travels with
     * the client, so the two together say whether the screen in front of somebody
     * came from the build the server is running.
     */
    std::string version;

    friend bool operator==(const bootstrap_status_response&,
                           const bootstrap_status_response&) = default;
};

struct create_initial_admin_request {
    using response_type = struct create_initial_admin_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.create_initial_admin";
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

    friend bool operator==(const create_initial_admin_request&,
                           const create_initial_admin_request&) = default;
};

struct create_initial_admin_response {
    bool success = false;
    std::string error_message;
    std::string account_id;
    std::string tenant_name;
    std::string tenant_id;

    friend bool operator==(const create_initial_admin_response&,
                           const create_initial_admin_response&) = default;
};

/**
 * @brief The members of bootstrap_status_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bootstrap_status_request>) {
    return std::tuple{};
}

/**
 * @brief The members of bootstrap_status_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<bootstrap_status_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"is_in_bootstrap_mode",
                                      &bootstrap_status_response::is_in_bootstrap_mode},
        ::ores::utility::codec::field{"message", &bootstrap_status_response::message},
        ::ores::utility::codec::field{"has_tenant", &bootstrap_status_response::has_tenant},
        ::ores::utility::codec::field{"version", &bootstrap_status_response::version},
    };
}

/**
 * @brief The members of create_initial_admin_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<create_initial_admin_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"principal", &create_initial_admin_request::principal},
        ::ores::utility::codec::field{"password", &create_initial_admin_request::password},
        ::ores::utility::codec::field{"email", &create_initial_admin_request::email},
    };
}

/**
 * @brief The members of create_initial_admin_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<create_initial_admin_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &create_initial_admin_response::success},
        ::ores::utility::codec::field{"error_message",
                                      &create_initial_admin_response::error_message},
        ::ores::utility::codec::field{"account_id", &create_initial_admin_response::account_id},
        ::ores::utility::codec::field{"tenant_name", &create_initial_admin_response::tenant_name},
        ::ores::utility::codec::field{"tenant_id", &create_initial_admin_response::tenant_id},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::bootstrap_status_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::bootstrap_status_request& v);
    static ores::iam::messaging::bootstrap_status_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::bootstrap_status_request& v);
    static ores::iam::messaging::bootstrap_status_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::bootstrap_status_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::bootstrap_status_response& v);
    static ores::iam::messaging::bootstrap_status_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::bootstrap_status_response& v);
    static ores::iam::messaging::bootstrap_status_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::create_initial_admin_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::create_initial_admin_request& v);
    static ores::iam::messaging::create_initial_admin_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::create_initial_admin_request& v);
    static ores::iam::messaging::create_initial_admin_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::create_initial_admin_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::create_initial_admin_response& v);
    static ores::iam::messaging::create_initial_admin_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::create_initial_admin_response& v);
    static ores::iam::messaging::create_initial_admin_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
