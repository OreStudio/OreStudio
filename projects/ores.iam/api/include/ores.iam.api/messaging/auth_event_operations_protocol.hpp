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
#ifndef ORES_IAM_API_MESSAGING_AUTH_EVENT_OPERATIONS_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_AUTH_EVENT_OPERATIONS_PROTOCOL_HPP

#include "ores.iam.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
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
 * @brief One authentication event, as the log records it.
 *
 * The event time is an instant, so it crosses the wire in the project's one
 * timestamp shape: the rfl parser writes it with `to_iso8601_utc`, and the
 * bytes do not depend on the timezone of the session that read the row. See
 * =doc/knowledge/architecture/time_architecture.org=.
 */
struct auth_event {
    std::string id;
    std::chrono::system_clock::time_point event_time;
    std::string account_id;
    std::string event_type;
    std::string username;
    std::string session_id;
    std::string party_id;
    std::string error_detail;

    friend bool operator==(const auth_event&, const auth_event&) = default;
};

/**
 * @brief A window over the caller's tenant authentication events.
 *
 * A bound the caller states no value for is null, and the read then applies no
 * bound on that side. The window is newest first, so the screen reads the most
 * recent events without asking for an order.
 */
struct list_auth_events_request {
    using response_type = struct list_auth_events_response;
    static constexpr std::string_view nats_subject = "iam.v1.auth_events.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string account_id;
    std::string event_type;
    std::optional<std::chrono::system_clock::time_point> from_time;
    std::optional<std::chrono::system_clock::time_point> to_time;
    std::uint32_t limit = 200;
    std::uint32_t offset = 0;

    friend bool operator==(const list_auth_events_request&,
                           const list_auth_events_request&) = default;
};

struct list_auth_events_response {
    std::vector<auth_event> events;
    bool success = false;
    std::string message;

    friend bool operator==(const list_auth_events_response&,
                           const list_auth_events_response&) = default;
};

/**
 * @brief The members of auth_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<auth_event>) {
    return std::tuple{
        ::ores::utility::codec::field{"id", &auth_event::id},
        ::ores::utility::codec::field{"event_time", &auth_event::event_time},
        ::ores::utility::codec::field{"account_id", &auth_event::account_id},
        ::ores::utility::codec::field{"event_type", &auth_event::event_type},
        ::ores::utility::codec::field{"username", &auth_event::username},
        ::ores::utility::codec::field{"session_id", &auth_event::session_id},
        ::ores::utility::codec::field{"party_id", &auth_event::party_id},
        ::ores::utility::codec::field{"error_detail", &auth_event::error_detail},
    };
}

/**
 * @brief The members of list_auth_events_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_auth_events_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"account_id", &list_auth_events_request::account_id},
        ::ores::utility::codec::field{"event_type", &list_auth_events_request::event_type},
        ::ores::utility::codec::field{"from_time", &list_auth_events_request::from_time},
        ::ores::utility::codec::field{"to_time", &list_auth_events_request::to_time},
        ::ores::utility::codec::field{"limit", &list_auth_events_request::limit},
        ::ores::utility::codec::field{"offset", &list_auth_events_request::offset},
    };
}

/**
 * @brief The members of list_auth_events_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_auth_events_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"events", &list_auth_events_response::events},
        ::ores::utility::codec::field{"success", &list_auth_events_response::success},
        ::ores::utility::codec::field{"message", &list_auth_events_response::message},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::auth_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::auth_event& v);
    static ores::iam::messaging::auth_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::auth_event& v);
    static ores::iam::messaging::auth_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_auth_events_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_auth_events_request& v);
    static ores::iam::messaging::list_auth_events_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_auth_events_request& v);
    static ores::iam::messaging::list_auth_events_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_IAM_API_EXPORT wire_traits<ores::iam::messaging::list_auth_events_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::iam::messaging::list_auth_events_response& v);
    static ores::iam::messaging::list_auth_events_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::iam::messaging::list_auth_events_response& v);
    static ores::iam::messaging::list_auth_events_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
