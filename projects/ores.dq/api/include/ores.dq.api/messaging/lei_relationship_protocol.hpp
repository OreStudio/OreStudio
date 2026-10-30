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
#ifndef ORES_DQ_API_MESSAGING_LEI_RELATIONSHIP_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_LEI_RELATIONSHIP_PROTOCOL_HPP

#include "ores.dq.api/domain/lei_relationship.hpp"
#include "ores.dq.api/export.hpp"
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

namespace ores::dq::messaging {

struct lei_relationship_key {
    std::string relationship_start_node_node_id;
};

struct lei_relationship_lookup {
    lei_relationship_key key;
    std::optional<ores::dq::domain::lei_relationship> lei_relationship;
};

struct lei_relationships_filter {
    std::optional<std::vector<std::string>> relationship_start_node_node_id_one_of;
};

struct lei_relationship_event {
    boost::uuids::uuid event_id;
    lei_relationship_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_lei_relationships_request {
    using response_type = struct list_lei_relationships_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_relationships.list";
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
    std::optional<lei_relationships_filter> filter;
};

struct list_lei_relationships_response {
    ores::utility::domain::result result;
    std::vector<ores::dq::domain::lei_relationship> relationships;
    std::uint64_t total;
};

struct get_lei_relationship_request {
    using response_type = struct get_lei_relationship_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_relationships.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    lei_relationship_key key;
};

struct get_lei_relationship_response {
    ores::utility::domain::result result;
    std::optional<ores::dq::domain::lei_relationship> lei_relationship;
};

struct get_many_lei_relationships_request {
    using response_type = struct get_many_lei_relationships_response;
    static constexpr std::string_view nats_subject = "dq.v1.lei_relationships.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<lei_relationship_key> keys;
};

struct get_many_lei_relationships_response {
    ores::utility::domain::result result;
    std::vector<lei_relationship_lookup> entries;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace lei_relationship_event_subjects {
inline constexpr std::string_view created = "dq.v1.lei_relationships_events.created";
inline constexpr std::string_view updated = "dq.v1.lei_relationships_events.updated";
inline constexpr std::string_view deleted = "dq.v1.lei_relationships_events.deleted";
}

/**
 * @brief The members of lei_relationship_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_relationship_key&) {
    return std::tuple{
        utility::codec::field{"relationship_start_node_node_id",
                              &lei_relationship_key::relationship_start_node_node_id},
    };
}

/**
 * @brief The members of lei_relationship_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_relationship_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &lei_relationship_lookup::key},
        utility::codec::field{"lei_relationship", &lei_relationship_lookup::lei_relationship},
    };
}

/**
 * @brief The members of lei_relationships_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_relationships_filter&) {
    return std::tuple{
        utility::codec::field{"relationship_start_node_node_id_one_of",
                              &lei_relationships_filter::relationship_start_node_node_id_one_of},
    };
}

/**
 * @brief The members of lei_relationship_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const lei_relationship_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &lei_relationship_event::event_id},
        utility::codec::field{"key", &lei_relationship_event::key},
        utility::codec::field{"action", &lei_relationship_event::action},
        utility::codec::field{"version", &lei_relationship_event::version},
        utility::codec::field{"occurred_at", &lei_relationship_event::occurred_at},
        utility::codec::field{"correlation_id", &lei_relationship_event::correlation_id},
    };
}

/**
 * @brief The members of list_lei_relationships_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_lei_relationships_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_lei_relationships_request::offset},
        utility::codec::field{"limit", &list_lei_relationships_request::limit},
        utility::codec::field{"order", &list_lei_relationships_request::order},
        utility::codec::field{"filter", &list_lei_relationships_request::filter},
    };
}

/**
 * @brief The members of list_lei_relationships_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_lei_relationships_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_lei_relationships_response::result},
        utility::codec::field{"relationships", &list_lei_relationships_response::relationships},
        utility::codec::field{"total", &list_lei_relationships_response::total},
    };
}

/**
 * @brief The members of get_lei_relationship_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_lei_relationship_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_lei_relationship_request::key},
    };
}

/**
 * @brief The members of get_lei_relationship_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_lei_relationship_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_lei_relationship_response::result},
        utility::codec::field{"lei_relationship", &get_lei_relationship_response::lei_relationship},
    };
}

/**
 * @brief The members of get_many_lei_relationships_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_lei_relationships_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_lei_relationships_request::keys},
    };
}

/**
 * @brief The members of get_many_lei_relationships_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_lei_relationships_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_lei_relationships_response::result},
        utility::codec::field{"entries", &get_many_lei_relationships_response::entries},
    };
}

}

namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_relationship_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_relationship_key& v);
    static ores::dq::messaging::lei_relationship_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_relationship_key& v);
    static ores::dq::messaging::lei_relationship_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_relationship_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_relationship_lookup& v);
    static ores::dq::messaging::lei_relationship_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_relationship_lookup& v);
    static ores::dq::messaging::lei_relationship_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_relationships_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_relationships_filter& v);
    static ores::dq::messaging::lei_relationships_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_relationships_filter& v);
    static ores::dq::messaging::lei_relationships_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::lei_relationship_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::lei_relationship_event& v);
    static ores::dq::messaging::lei_relationship_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::lei_relationship_event& v);
    static ores::dq::messaging::lei_relationship_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_lei_relationships_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_lei_relationships_request& v);
    static ores::dq::messaging::list_lei_relationships_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_lei_relationships_request& v);
    static ores::dq::messaging::list_lei_relationships_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::list_lei_relationships_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::list_lei_relationships_response& v);
    static ores::dq::messaging::list_lei_relationships_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::list_lei_relationships_response& v);
    static ores::dq::messaging::list_lei_relationships_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_lei_relationship_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_lei_relationship_request& v);
    static ores::dq::messaging::get_lei_relationship_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_lei_relationship_request& v);
    static ores::dq::messaging::get_lei_relationship_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_lei_relationship_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_lei_relationship_response& v);
    static ores::dq::messaging::get_lei_relationship_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::get_lei_relationship_response& v);
    static ores::dq::messaging::get_lei_relationship_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_lei_relationships_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::get_many_lei_relationships_request& v);
    static ores::dq::messaging::get_many_lei_relationships_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_lei_relationships_request& v);
    static ores::dq::messaging::get_many_lei_relationships_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::get_many_lei_relationships_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::get_many_lei_relationships_response& v);
    static ores::dq::messaging::get_many_lei_relationships_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::get_many_lei_relationships_response& v);
    static ores::dq::messaging::get_many_lei_relationships_response
    read_msgpack(std::span<const std::byte> data);
};
}

#endif
