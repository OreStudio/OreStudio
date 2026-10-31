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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_IDENTIFIER_CHANGE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_IDENTIFIER_CHANGE_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty_identifier_change.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct counterparty_identifier_change_key {
    boost::uuids::uuid id;
};

struct counterparty_identifier_change_lookup {
    counterparty_identifier_change_key key;
    std::optional<ores::refdata::domain::counterparty_identifier_change>
        counterparty_identifier_change;
};

struct counterparty_identifier_changes_filter {
    std::optional<boost::uuids::uuid> request_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> request_id_one_of;
};

struct counterparty_identifier_change_event {
    boost::uuids::uuid event_id;
    counterparty_identifier_change_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct counterparty_identifier_change_version_key {
    counterparty_identifier_change_key counterparty_identifier_change;
    std::uint32_t version;
};

struct counterparty_identifier_change_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_counterparty_identifier_changes_request {
    using response_type = struct list_counterparty_identifier_changes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifier_changes.list";
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
    std::optional<counterparty_identifier_changes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_counterparty_identifier_changes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_identifier_change>
        counterparty_identifier_changes;
    std::uint64_t total;
};

struct get_counterparty_identifier_change_request {
    using response_type = struct get_counterparty_identifier_change_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifier_changes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_identifier_change_key key;
};

struct get_counterparty_identifier_change_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_identifier_change>
        counterparty_identifier_change;
};

struct get_many_counterparty_identifier_changes_request {
    using response_type = struct get_many_counterparty_identifier_changes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifier_changes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_identifier_change_key> keys;
};

struct get_many_counterparty_identifier_changes_response {
    ores::utility::domain::result result;
    std::vector<counterparty_identifier_change_lookup> entries;
};

struct list_by_request_id_counterparty_identifier_changes_request {
    using response_type = struct list_by_request_id_counterparty_identifier_changes_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifier_changes.list_by_request_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid request_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_identifier_changes_filter> filter;
};

struct list_by_request_id_counterparty_identifier_changes_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_identifier_change>
        counterparty_identifier_changes;
    std::uint64_t total;
};

struct list_counterparty_identifier_change_versions_request {
    using response_type = struct list_counterparty_identifier_change_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifier_changes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_identifier_change_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_identifier_change_versions_filter> filter;
};

struct list_counterparty_identifier_change_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty_identifier_change> versions;
    std::uint64_t total;
};

struct get_counterparty_identifier_change_version_request {
    using response_type = struct get_counterparty_identifier_change_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.counterparty_identifier_changes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_identifier_change_version_key key;
};

struct get_counterparty_identifier_change_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty_identifier_change> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace counterparty_identifier_change_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.counterparty_identifier_changes_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.counterparty_identifier_changes_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.counterparty_identifier_changes_events.deleted";
}
/**
 * @brief The members of counterparty_identifier_change_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_identifier_change_key&) {
    return std::tuple{
        utility::codec::field{"id", &counterparty_identifier_change_key::id},
    };
}

/**
 * @brief The members of counterparty_identifier_change_lookup, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_identifier_change_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &counterparty_identifier_change_lookup::key},
        utility::codec::field{
            "counterparty_identifier_change",
            &counterparty_identifier_change_lookup::counterparty_identifier_change},
    };
}

/**
 * @brief The members of counterparty_identifier_changes_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_identifier_changes_filter&) {
    return std::tuple{
        utility::codec::field{"request_id", &counterparty_identifier_changes_filter::request_id},
        utility::codec::field{"id_one_of", &counterparty_identifier_changes_filter::id_one_of},
        utility::codec::field{"request_id_one_of",
                              &counterparty_identifier_changes_filter::request_id_one_of},
    };
}

/**
 * @brief The members of counterparty_identifier_change_event, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_identifier_change_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &counterparty_identifier_change_event::event_id},
        utility::codec::field{"key", &counterparty_identifier_change_event::key},
        utility::codec::field{"action", &counterparty_identifier_change_event::action},
        utility::codec::field{"version", &counterparty_identifier_change_event::version},
        utility::codec::field{"occurred_at", &counterparty_identifier_change_event::occurred_at},
        utility::codec::field{"correlation_id",
                              &counterparty_identifier_change_event::correlation_id},
    };
}

/**
 * @brief The members of counterparty_identifier_change_version_key, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_identifier_change_version_key&) {
    return std::tuple{
        utility::codec::field{
            "counterparty_identifier_change",
            &counterparty_identifier_change_version_key::counterparty_identifier_change},
        utility::codec::field{"version", &counterparty_identifier_change_version_key::version},
    };
}

/**
 * @brief The members of counterparty_identifier_change_versions_filter, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_identifier_change_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &counterparty_identifier_change_versions_filter::version},
        utility::codec::field{"from_version",
                              &counterparty_identifier_change_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &counterparty_identifier_change_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_counterparty_identifier_changes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparty_identifier_changes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_counterparty_identifier_changes_request::offset},
        utility::codec::field{"limit", &list_counterparty_identifier_changes_request::limit},
        utility::codec::field{"order", &list_counterparty_identifier_changes_request::order},
        utility::codec::field{"filter", &list_counterparty_identifier_changes_request::filter},
        utility::codec::field{"as_of", &list_counterparty_identifier_changes_request::as_of},
    };
}

/**
 * @brief The members of list_counterparty_identifier_changes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparty_identifier_changes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_counterparty_identifier_changes_response::result},
        utility::codec::field{
            "counterparty_identifier_changes",
            &list_counterparty_identifier_changes_response::counterparty_identifier_changes},
        utility::codec::field{"total", &list_counterparty_identifier_changes_response::total},
    };
}

/**
 * @brief The members of get_counterparty_identifier_change_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_identifier_change_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_counterparty_identifier_change_request::key},
    };
}

/**
 * @brief The members of get_counterparty_identifier_change_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_identifier_change_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_counterparty_identifier_change_response::result},
        utility::codec::field{
            "counterparty_identifier_change",
            &get_counterparty_identifier_change_response::counterparty_identifier_change},
    };
}

/**
 * @brief The members of get_many_counterparty_identifier_changes_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_counterparty_identifier_changes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_counterparty_identifier_changes_request::keys},
    };
}

/**
 * @brief The members of get_many_counterparty_identifier_changes_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_counterparty_identifier_changes_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_counterparty_identifier_changes_response::result},
        utility::codec::field{"entries",
                              &get_many_counterparty_identifier_changes_response::entries},
    };
}

/**
 * @brief The members of list_by_request_id_counterparty_identifier_changes_request, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_request_id_counterparty_identifier_changes_request&) {
    return std::tuple{
        utility::codec::field{
            "request_id", &list_by_request_id_counterparty_identifier_changes_request::request_id},
        utility::codec::field{"scope",
                              &list_by_request_id_counterparty_identifier_changes_request::scope},
        utility::codec::field{"offset",
                              &list_by_request_id_counterparty_identifier_changes_request::offset},
        utility::codec::field{"limit",
                              &list_by_request_id_counterparty_identifier_changes_request::limit},
        utility::codec::field{"order",
                              &list_by_request_id_counterparty_identifier_changes_request::order},
        utility::codec::field{"filter",
                              &list_by_request_id_counterparty_identifier_changes_request::filter},
    };
}

/**
 * @brief The members of list_by_request_id_counterparty_identifier_changes_response, named as the
 * wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_by_request_id_counterparty_identifier_changes_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_request_id_counterparty_identifier_changes_response::result},
        utility::codec::field{"counterparty_identifier_changes",
                              &list_by_request_id_counterparty_identifier_changes_response::
                                  counterparty_identifier_changes},
        utility::codec::field{"total",
                              &list_by_request_id_counterparty_identifier_changes_response::total},
    };
}

/**
 * @brief The members of list_counterparty_identifier_change_versions_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_counterparty_identifier_change_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_counterparty_identifier_change_versions_request::key},
        utility::codec::field{"offset",
                              &list_counterparty_identifier_change_versions_request::offset},
        utility::codec::field{"limit",
                              &list_counterparty_identifier_change_versions_request::limit},
        utility::codec::field{"order",
                              &list_counterparty_identifier_change_versions_request::order},
        utility::codec::field{"filter",
                              &list_counterparty_identifier_change_versions_request::filter},
    };
}

/**
 * @brief The members of list_counterparty_identifier_change_versions_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const list_counterparty_identifier_change_versions_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_counterparty_identifier_change_versions_response::result},
        utility::codec::field{"versions",
                              &list_counterparty_identifier_change_versions_response::versions},
        utility::codec::field{"total",
                              &list_counterparty_identifier_change_versions_response::total},
    };
}

/**
 * @brief The members of get_counterparty_identifier_change_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_counterparty_identifier_change_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_counterparty_identifier_change_version_request::key},
    };
}

/**
 * @brief The members of get_counterparty_identifier_change_version_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(const get_counterparty_identifier_change_version_response&) {
    return std::tuple{
        utility::codec::field{"result",
                              &get_counterparty_identifier_change_version_response::result},
        utility::codec::field{"version",
                              &get_counterparty_identifier_change_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_change_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_change_key& v);
    static ores::refdata::messaging::counterparty_identifier_change_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_change_key& v);
    static ores::refdata::messaging::counterparty_identifier_change_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_change_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_change_lookup& v);
    static ores::refdata::messaging::counterparty_identifier_change_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_change_lookup& v);
    static ores::refdata::messaging::counterparty_identifier_change_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_changes_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_changes_filter& v);
    static ores::refdata::messaging::counterparty_identifier_changes_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_changes_filter& v);
    static ores::refdata::messaging::counterparty_identifier_changes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_change_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_change_event& v);
    static ores::refdata::messaging::counterparty_identifier_change_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_change_event& v);
    static ores::refdata::messaging::counterparty_identifier_change_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_change_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_change_version_key& v);
    static ores::refdata::messaging::counterparty_identifier_change_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_identifier_change_version_key& v);
    static ores::refdata::messaging::counterparty_identifier_change_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::counterparty_identifier_change_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::counterparty_identifier_change_versions_filter& v);
    static ores::refdata::messaging::counterparty_identifier_change_versions_filter
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::counterparty_identifier_change_versions_filter& v);
    static ores::refdata::messaging::counterparty_identifier_change_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_identifier_changes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_identifier_changes_request& v);
    static ores::refdata::messaging::list_counterparty_identifier_changes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_identifier_changes_request& v);
    static ores::refdata::messaging::list_counterparty_identifier_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_identifier_changes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_identifier_changes_response& v);
    static ores::refdata::messaging::list_counterparty_identifier_changes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_identifier_changes_response& v);
    static ores::refdata::messaging::list_counterparty_identifier_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_identifier_change_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_identifier_change_request& v);
    static ores::refdata::messaging::get_counterparty_identifier_change_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_identifier_change_request& v);
    static ores::refdata::messaging::get_counterparty_identifier_change_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_identifier_change_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_identifier_change_response& v);
    static ores::refdata::messaging::get_counterparty_identifier_change_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_identifier_change_response& v);
    static ores::refdata::messaging::get_counterparty_identifier_change_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparty_identifier_changes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_counterparty_identifier_changes_request& v);
    static ores::refdata::messaging::get_many_counterparty_identifier_changes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_counterparty_identifier_changes_request& v);
    static ores::refdata::messaging::get_many_counterparty_identifier_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparty_identifier_changes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_many_counterparty_identifier_changes_response& v);
    static ores::refdata::messaging::get_many_counterparty_identifier_changes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_many_counterparty_identifier_changes_response& v);
    static ores::refdata::messaging::get_many_counterparty_identifier_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_request&
            v);
    static ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_request&
            v);
    static ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<
    ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_response&
            v);
    static ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_response&
            v);
    static ores::refdata::messaging::list_by_request_id_counterparty_identifier_changes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_identifier_change_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_counterparty_identifier_change_versions_request& v);
    static ores::refdata::messaging::list_counterparty_identifier_change_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_counterparty_identifier_change_versions_request& v);
    static ores::refdata::messaging::list_counterparty_identifier_change_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_identifier_change_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::list_counterparty_identifier_change_versions_response& v);
    static ores::refdata::messaging::list_counterparty_identifier_change_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::list_counterparty_identifier_change_versions_response& v);
    static ores::refdata::messaging::list_counterparty_identifier_change_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_identifier_change_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_counterparty_identifier_change_version_request& v);
    static ores::refdata::messaging::get_counterparty_identifier_change_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_counterparty_identifier_change_version_request& v);
    static ores::refdata::messaging::get_counterparty_identifier_change_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_identifier_change_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::refdata::messaging::get_counterparty_identifier_change_version_response& v);
    static ores::refdata::messaging::get_counterparty_identifier_change_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::refdata::messaging::get_counterparty_identifier_change_version_response& v);
    static ores::refdata::messaging::get_counterparty_identifier_change_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
