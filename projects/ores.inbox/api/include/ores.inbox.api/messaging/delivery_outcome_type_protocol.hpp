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
#ifndef ORES_INBOX_API_MESSAGING_DELIVERY_OUTCOME_TYPE_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_DELIVERY_OUTCOME_TYPE_PROTOCOL_HPP

#include "ores.inbox.api/domain/delivery_outcome_type.hpp"
#include "ores.inbox.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::inbox::messaging {

struct delivery_outcome_type_key {
    std::string code;

    friend bool operator==(const delivery_outcome_type_key&,
                           const delivery_outcome_type_key&) = default;
};

struct delivery_outcome_type_lookup {
    delivery_outcome_type_key key;
    std::optional<ores::inbox::domain::delivery_outcome_type> delivery_outcome_type;

    friend bool operator==(const delivery_outcome_type_lookup&,
                           const delivery_outcome_type_lookup&) = default;
};

struct delivery_outcome_types_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const delivery_outcome_types_filter&,
                           const delivery_outcome_types_filter&) = default;
};

struct delivery_outcome_type_event {
    boost::uuids::uuid event_id;
    delivery_outcome_type_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const delivery_outcome_type_event&,
                           const delivery_outcome_type_event&) = default;
};

struct list_delivery_outcome_types_request {
    using response_type = struct list_delivery_outcome_types_response;
    static constexpr std::string_view nats_subject = "inbox.v1.delivery_outcome_types.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<delivery_outcome_types_filter> filter;

    friend bool operator==(const list_delivery_outcome_types_request&,
                           const list_delivery_outcome_types_request&) = default;
};

struct list_delivery_outcome_types_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::delivery_outcome_type> delivery_outcome_types;
    std::uint64_t total;

    friend bool operator==(const list_delivery_outcome_types_response&,
                           const list_delivery_outcome_types_response&) = default;
};

struct get_delivery_outcome_type_request {
    using response_type = struct get_delivery_outcome_type_response;
    static constexpr std::string_view nats_subject = "inbox.v1.delivery_outcome_types.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    delivery_outcome_type_key key;

    friend bool operator==(const get_delivery_outcome_type_request&,
                           const get_delivery_outcome_type_request&) = default;
};

struct get_delivery_outcome_type_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::delivery_outcome_type> delivery_outcome_type;

    friend bool operator==(const get_delivery_outcome_type_response&,
                           const get_delivery_outcome_type_response&) = default;
};

struct get_many_delivery_outcome_types_request {
    using response_type = struct get_many_delivery_outcome_types_response;
    static constexpr std::string_view nats_subject = "inbox.v1.delivery_outcome_types.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<delivery_outcome_type_key> keys;

    friend bool operator==(const get_many_delivery_outcome_types_request&,
                           const get_many_delivery_outcome_types_request&) = default;
};

struct get_many_delivery_outcome_types_response {
    ores::utility::domain::result result;
    std::vector<delivery_outcome_type_lookup> entries;

    friend bool operator==(const get_many_delivery_outcome_types_response&,
                           const get_many_delivery_outcome_types_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace delivery_outcome_type_event_subjects {
inline constexpr std::string_view created = "inbox.v1.delivery_outcome_types_events.created";
inline constexpr std::string_view updated = "inbox.v1.delivery_outcome_types_events.updated";
inline constexpr std::string_view deleted = "inbox.v1.delivery_outcome_types_events.deleted";
}
/**
 * @brief The members of delivery_outcome_type_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delivery_outcome_type_key&) {
    return std::tuple{
        utility::codec::field{"code", &delivery_outcome_type_key::code},
    };
}

/**
 * @brief The members of delivery_outcome_type_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delivery_outcome_type_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &delivery_outcome_type_lookup::key},
        utility::codec::field{"delivery_outcome_type",
                              &delivery_outcome_type_lookup::delivery_outcome_type},
    };
}

/**
 * @brief The members of delivery_outcome_types_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delivery_outcome_types_filter&) {
    return std::tuple{
        utility::codec::field{"code_one_of", &delivery_outcome_types_filter::code_one_of},
    };
}

/**
 * @brief The members of delivery_outcome_type_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delivery_outcome_type_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &delivery_outcome_type_event::event_id},
        utility::codec::field{"key", &delivery_outcome_type_event::key},
        utility::codec::field{"action", &delivery_outcome_type_event::action},
        utility::codec::field{"version", &delivery_outcome_type_event::version},
        utility::codec::field{"occurred_at", &delivery_outcome_type_event::occurred_at},
        utility::codec::field{"correlation_id", &delivery_outcome_type_event::correlation_id},
    };
}

/**
 * @brief The members of list_delivery_outcome_types_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_delivery_outcome_types_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_delivery_outcome_types_request::offset},
        utility::codec::field{"limit", &list_delivery_outcome_types_request::limit},
        utility::codec::field{"order", &list_delivery_outcome_types_request::order},
        utility::codec::field{"filter", &list_delivery_outcome_types_request::filter},
    };
}

/**
 * @brief The members of list_delivery_outcome_types_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_delivery_outcome_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_delivery_outcome_types_response::result},
        utility::codec::field{"delivery_outcome_types",
                              &list_delivery_outcome_types_response::delivery_outcome_types},
        utility::codec::field{"total", &list_delivery_outcome_types_response::total},
    };
}

/**
 * @brief The members of get_delivery_outcome_type_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_delivery_outcome_type_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_delivery_outcome_type_request::key},
    };
}

/**
 * @brief The members of get_delivery_outcome_type_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_delivery_outcome_type_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_delivery_outcome_type_response::result},
        utility::codec::field{"delivery_outcome_type",
                              &get_delivery_outcome_type_response::delivery_outcome_type},
    };
}

/**
 * @brief The members of get_many_delivery_outcome_types_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_delivery_outcome_types_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_delivery_outcome_types_request::keys},
    };
}

/**
 * @brief The members of get_many_delivery_outcome_types_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_delivery_outcome_types_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_delivery_outcome_types_response::result},
        utility::codec::field{"entries", &get_many_delivery_outcome_types_response::entries},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delivery_outcome_type_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::delivery_outcome_type_key& v);
    static ores::inbox::messaging::delivery_outcome_type_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::delivery_outcome_type_key& v);
    static ores::inbox::messaging::delivery_outcome_type_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delivery_outcome_type_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::delivery_outcome_type_lookup& v);
    static ores::inbox::messaging::delivery_outcome_type_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::delivery_outcome_type_lookup& v);
    static ores::inbox::messaging::delivery_outcome_type_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delivery_outcome_types_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::delivery_outcome_types_filter& v);
    static ores::inbox::messaging::delivery_outcome_types_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::delivery_outcome_types_filter& v);
    static ores::inbox::messaging::delivery_outcome_types_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT wire_traits<ores::inbox::messaging::delivery_outcome_type_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::inbox::messaging::delivery_outcome_type_event& v);
    static ores::inbox::messaging::delivery_outcome_type_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::inbox::messaging::delivery_outcome_type_event& v);
    static ores::inbox::messaging::delivery_outcome_type_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_delivery_outcome_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_delivery_outcome_types_request& v);
    static ores::inbox::messaging::list_delivery_outcome_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_delivery_outcome_types_request& v);
    static ores::inbox::messaging::list_delivery_outcome_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::list_delivery_outcome_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::list_delivery_outcome_types_response& v);
    static ores::inbox::messaging::list_delivery_outcome_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::list_delivery_outcome_types_response& v);
    static ores::inbox::messaging::list_delivery_outcome_types_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_delivery_outcome_type_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_delivery_outcome_type_request& v);
    static ores::inbox::messaging::get_delivery_outcome_type_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_delivery_outcome_type_request& v);
    static ores::inbox::messaging::get_delivery_outcome_type_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_delivery_outcome_type_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_delivery_outcome_type_response& v);
    static ores::inbox::messaging::get_delivery_outcome_type_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_delivery_outcome_type_response& v);
    static ores::inbox::messaging::get_delivery_outcome_type_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_delivery_outcome_types_request& v);
    static ores::inbox::messaging::get_many_delivery_outcome_types_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_delivery_outcome_types_request& v);
    static ores::inbox::messaging::get_many_delivery_outcome_types_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_INBOX_API_EXPORT
    wire_traits<ores::inbox::messaging::get_many_delivery_outcome_types_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::inbox::messaging::get_many_delivery_outcome_types_response& v);
    static ores::inbox::messaging::get_many_delivery_outcome_types_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::inbox::messaging::get_many_delivery_outcome_types_response& v);
    static ores::inbox::messaging::get_many_delivery_outcome_types_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
