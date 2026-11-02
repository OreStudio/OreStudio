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
#ifndef ORES_IAM_API_MESSAGING_SESSION_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SESSION_OPERATIONS_PROTOCOL_HPP

#include "ores.iam.api/domain/session.hpp"
#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::iam::messaging {

/**
 * @brief A session with its party-scoped context.
 *
 * The session is the entity; party_id, visible_party_ids and username are
 * the denormalised fields reached through the account-party association.
 * They are message fields because no column backs them.
 */
struct session_view {
    ores::iam::domain::session session;
    std::string party_id;
    std::vector<std::string> visible_party_ids;
    std::string username;
};

struct get_active_sessions_request {
    using response_type = struct get_active_sessions_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.get_active_sessions";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_active_sessions_response {
    std::vector<ores::iam::domain::session> sessions;
    bool success = false;
    std::string message;
};

/**
 * @brief End another account's session in the caller's tenant.
 *
 * A session is opened by signing in and closed by the service that ends it,
 * so the act is an operation rather than a write to the row. The operator
 * names the session by its identifier; the tenant scope comes from the
 * caller's own session, and a session belonging to another tenant is not
 * there to end.
 */
struct end_session_request {
    using response_type = struct end_session_response;
    static constexpr std::string_view nats_subject = "iam.v1.ops.end_session";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid session_id;
};

struct end_session_response {
    bool success = false;
    std::string message;
};

/**
 * @brief The members of session_view, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const session_view&) {
    return std::tuple{
        ::ores::utility::codec::field{"session", &session_view::session},
        ::ores::utility::codec::field{"party_id", &session_view::party_id},
        ::ores::utility::codec::field{"visible_party_ids", &session_view::visible_party_ids},
        ::ores::utility::codec::field{"username", &session_view::username},
    };
}

/**
 * @brief The members of get_active_sessions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_active_sessions_request&) {
    return std::tuple{};
}

/**
 * @brief The members of get_active_sessions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_active_sessions_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"sessions", &get_active_sessions_response::sessions},
        ::ores::utility::codec::field{"success", &get_active_sessions_response::success},
        ::ores::utility::codec::field{"message", &get_active_sessions_response::message},
    };
}

/**
 * @brief The members of end_session_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const end_session_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"session_id", &end_session_request::session_id},
    };
}

/**
 * @brief The members of end_session_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const end_session_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &end_session_response::success},
        ::ores::utility::codec::field{"message", &end_session_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::session_view> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::session_view& v);
    static ores::iam::messaging::session_view read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::session_view& v);
    static ores::iam::messaging::session_view read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_active_sessions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_active_sessions_request& v);
    static ores::iam::messaging::get_active_sessions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_active_sessions_request& v);
    static ores::iam::messaging::get_active_sessions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::get_active_sessions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::get_active_sessions_response& v);
    static ores::iam::messaging::get_active_sessions_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::get_active_sessions_response& v);
    static ores::iam::messaging::get_active_sessions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::end_session_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::end_session_request& v);
    static ores::iam::messaging::end_session_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::end_session_request& v);
    static ores::iam::messaging::end_session_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::end_session_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::end_session_response& v);
    static ores::iam::messaging::end_session_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::end_session_response& v);
    static ores::iam::messaging::end_session_response read_msgpack(std::span<const std::byte> data);
};
}


#endif
