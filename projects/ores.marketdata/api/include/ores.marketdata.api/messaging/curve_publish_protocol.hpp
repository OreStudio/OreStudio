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
#ifndef ORES_MARKETDATA_API_MESSAGING_CURVE_PUBLISH_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_CURVE_PUBLISH_PROTOCOL_HPP

#include "ores.marketdata.api/domain/curve_publish.hpp"
#include "ores.marketdata.api/export.hpp"
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

namespace ores::marketdata::messaging {

struct curve_publish_key {
    boost::uuids::uuid id;
};

struct curve_publish_lookup {
    curve_publish_key key;
    std::optional<ores::marketdata::domain::curve_publish> curve_publish;
};

struct curve_publishes_filter {
    std::optional<boost::uuids::uuid> request_id;
    std::optional<boost::uuids::uuid> party_id;
    std::optional<boost::uuids::uuid> bootstrap_config_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> request_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> party_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> bootstrap_config_id_one_of;
};

struct curve_publish_event {
    boost::uuids::uuid event_id;
    curve_publish_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct curve_publish_version_key {
    curve_publish_key curve_publish;
    std::uint32_t version;
};

struct curve_publish_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_curve_publishes_request {
    using response_type = struct list_curve_publishes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve_publishes.list";
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
    std::optional<curve_publishes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_curve_publishes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::curve_publish> curve_publishes;
    std::uint64_t total;
};

struct get_curve_publish_request {
    using response_type = struct get_curve_publish_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve_publishes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_publish_key key;
};

struct get_curve_publish_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::curve_publish> curve_publish;
};

struct get_many_curve_publishes_request {
    using response_type = struct get_many_curve_publishes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve_publishes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<curve_publish_key> keys;
};

struct get_many_curve_publishes_response {
    ores::utility::domain::result result;
    std::vector<curve_publish_lookup> entries;
};

struct list_by_request_id_curve_publishes_request {
    using response_type = struct list_by_request_id_curve_publishes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.curve_publishes.list_by_request_id";
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
    std::optional<curve_publishes_filter> filter;
};

struct list_by_request_id_curve_publishes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::curve_publish> curve_publishes;
    std::uint64_t total;
};

struct list_curve_publish_versions_request {
    using response_type = struct list_curve_publish_versions_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve_publishes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_publish_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<curve_publish_versions_filter> filter;
};

struct list_curve_publish_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::curve_publish> versions;
    std::uint64_t total;
};

struct get_curve_publish_version_request {
    using response_type = struct get_curve_publish_version_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve_publishes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    curve_publish_version_key key;
};

struct get_curve_publish_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::curve_publish> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace curve_publish_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.curve_publishes_events.created";
inline constexpr std::string_view updated = "marketdata.v1.curve_publishes_events.updated";
inline constexpr std::string_view deleted = "marketdata.v1.curve_publishes_events.deleted";
}
/**
 * @brief The members of curve_publish_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_publish_key&) {
    return std::tuple{
        utility::codec::field{"id", &curve_publish_key::id},
    };
}

/**
 * @brief The members of curve_publish_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_publish_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &curve_publish_lookup::key},
        utility::codec::field{"curve_publish", &curve_publish_lookup::curve_publish},
    };
}

/**
 * @brief The members of curve_publishes_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_publishes_filter&) {
    return std::tuple{
        utility::codec::field{"request_id", &curve_publishes_filter::request_id},
        utility::codec::field{"party_id", &curve_publishes_filter::party_id},
        utility::codec::field{"bootstrap_config_id", &curve_publishes_filter::bootstrap_config_id},
        utility::codec::field{"id_one_of", &curve_publishes_filter::id_one_of},
        utility::codec::field{"request_id_one_of", &curve_publishes_filter::request_id_one_of},
        utility::codec::field{"party_id_one_of", &curve_publishes_filter::party_id_one_of},
        utility::codec::field{"bootstrap_config_id_one_of",
                              &curve_publishes_filter::bootstrap_config_id_one_of},
    };
}

/**
 * @brief The members of curve_publish_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_publish_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &curve_publish_event::event_id},
        utility::codec::field{"key", &curve_publish_event::key},
        utility::codec::field{"action", &curve_publish_event::action},
        utility::codec::field{"version", &curve_publish_event::version},
        utility::codec::field{"occurred_at", &curve_publish_event::occurred_at},
        utility::codec::field{"correlation_id", &curve_publish_event::correlation_id},
    };
}

/**
 * @brief The members of curve_publish_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_publish_version_key&) {
    return std::tuple{
        utility::codec::field{"curve_publish", &curve_publish_version_key::curve_publish},
        utility::codec::field{"version", &curve_publish_version_key::version},
    };
}

/**
 * @brief The members of curve_publish_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const curve_publish_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &curve_publish_versions_filter::version},
        utility::codec::field{"from_version", &curve_publish_versions_filter::from_version},
        utility::codec::field{"to_version", &curve_publish_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_curve_publishes_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_publishes_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_curve_publishes_request::offset},
        utility::codec::field{"limit", &list_curve_publishes_request::limit},
        utility::codec::field{"order", &list_curve_publishes_request::order},
        utility::codec::field{"filter", &list_curve_publishes_request::filter},
        utility::codec::field{"as_of", &list_curve_publishes_request::as_of},
    };
}

/**
 * @brief The members of list_curve_publishes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_publishes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_publishes_response::result},
        utility::codec::field{"curve_publishes", &list_curve_publishes_response::curve_publishes},
        utility::codec::field{"total", &list_curve_publishes_response::total},
    };
}

/**
 * @brief The members of get_curve_publish_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_publish_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_publish_request::key},
    };
}

/**
 * @brief The members of get_curve_publish_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_publish_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_publish_response::result},
        utility::codec::field{"curve_publish", &get_curve_publish_response::curve_publish},
    };
}

/**
 * @brief The members of get_many_curve_publishes_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_publishes_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_curve_publishes_request::keys},
    };
}

/**
 * @brief The members of get_many_curve_publishes_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_curve_publishes_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_curve_publishes_response::result},
        utility::codec::field{"entries", &get_many_curve_publishes_response::entries},
    };
}

/**
 * @brief The members of list_by_request_id_curve_publishes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_request_id_curve_publishes_request&) {
    return std::tuple{
        utility::codec::field{"request_id",
                              &list_by_request_id_curve_publishes_request::request_id},
        utility::codec::field{"scope", &list_by_request_id_curve_publishes_request::scope},
        utility::codec::field{"offset", &list_by_request_id_curve_publishes_request::offset},
        utility::codec::field{"limit", &list_by_request_id_curve_publishes_request::limit},
        utility::codec::field{"order", &list_by_request_id_curve_publishes_request::order},
        utility::codec::field{"filter", &list_by_request_id_curve_publishes_request::filter},
    };
}

/**
 * @brief The members of list_by_request_id_curve_publishes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_by_request_id_curve_publishes_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_by_request_id_curve_publishes_response::result},
        utility::codec::field{"curve_publishes",
                              &list_by_request_id_curve_publishes_response::curve_publishes},
        utility::codec::field{"total", &list_by_request_id_curve_publishes_response::total},
    };
}

/**
 * @brief The members of list_curve_publish_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_publish_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_curve_publish_versions_request::key},
        utility::codec::field{"offset", &list_curve_publish_versions_request::offset},
        utility::codec::field{"limit", &list_curve_publish_versions_request::limit},
        utility::codec::field{"order", &list_curve_publish_versions_request::order},
        utility::codec::field{"filter", &list_curve_publish_versions_request::filter},
    };
}

/**
 * @brief The members of list_curve_publish_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_curve_publish_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_curve_publish_versions_response::result},
        utility::codec::field{"versions", &list_curve_publish_versions_response::versions},
        utility::codec::field{"total", &list_curve_publish_versions_response::total},
    };
}

/**
 * @brief The members of get_curve_publish_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_publish_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_curve_publish_version_request::key},
    };
}

/**
 * @brief The members of get_curve_publish_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_curve_publish_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_curve_publish_version_response::result},
        utility::codec::field{"version", &get_curve_publish_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::curve_publish_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::curve_publish_key& v);
    static ores::marketdata::messaging::curve_publish_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::curve_publish_key& v);
    static ores::marketdata::messaging::curve_publish_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::curve_publish_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::curve_publish_lookup& v);
    static ores::marketdata::messaging::curve_publish_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::curve_publish_lookup& v);
    static ores::marketdata::messaging::curve_publish_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::curve_publishes_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::curve_publishes_filter& v);
    static ores::marketdata::messaging::curve_publishes_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::curve_publishes_filter& v);
    static ores::marketdata::messaging::curve_publishes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::curve_publish_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::curve_publish_event& v);
    static ores::marketdata::messaging::curve_publish_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::curve_publish_event& v);
    static ores::marketdata::messaging::curve_publish_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::curve_publish_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::curve_publish_version_key& v);
    static ores::marketdata::messaging::curve_publish_version_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::curve_publish_version_key& v);
    static ores::marketdata::messaging::curve_publish_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::curve_publish_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::curve_publish_versions_filter& v);
    static ores::marketdata::messaging::curve_publish_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::curve_publish_versions_filter& v);
    static ores::marketdata::messaging::curve_publish_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_curve_publishes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_curve_publishes_request& v);
    static ores::marketdata::messaging::list_curve_publishes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_curve_publishes_request& v);
    static ores::marketdata::messaging::list_curve_publishes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_curve_publishes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_curve_publishes_response& v);
    static ores::marketdata::messaging::list_curve_publishes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_curve_publishes_response& v);
    static ores::marketdata::messaging::list_curve_publishes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_curve_publish_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_curve_publish_request& v);
    static ores::marketdata::messaging::get_curve_publish_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_curve_publish_request& v);
    static ores::marketdata::messaging::get_curve_publish_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_curve_publish_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_curve_publish_response& v);
    static ores::marketdata::messaging::get_curve_publish_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_curve_publish_response& v);
    static ores::marketdata::messaging::get_curve_publish_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_curve_publishes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_curve_publishes_request& v);
    static ores::marketdata::messaging::get_many_curve_publishes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_curve_publishes_request& v);
    static ores::marketdata::messaging::get_many_curve_publishes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_curve_publishes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_curve_publishes_response& v);
    static ores::marketdata::messaging::get_many_curve_publishes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_curve_publishes_response& v);
    static ores::marketdata::messaging::get_many_curve_publishes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_by_request_id_curve_publishes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_by_request_id_curve_publishes_request& v);
    static ores::marketdata::messaging::list_by_request_id_curve_publishes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_by_request_id_curve_publishes_request& v);
    static ores::marketdata::messaging::list_by_request_id_curve_publishes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_by_request_id_curve_publishes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_by_request_id_curve_publishes_response& v);
    static ores::marketdata::messaging::list_by_request_id_curve_publishes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_by_request_id_curve_publishes_response& v);
    static ores::marketdata::messaging::list_by_request_id_curve_publishes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_curve_publish_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_curve_publish_versions_request& v);
    static ores::marketdata::messaging::list_curve_publish_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_curve_publish_versions_request& v);
    static ores::marketdata::messaging::list_curve_publish_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_curve_publish_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_curve_publish_versions_response& v);
    static ores::marketdata::messaging::list_curve_publish_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_curve_publish_versions_response& v);
    static ores::marketdata::messaging::list_curve_publish_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_curve_publish_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_curve_publish_version_request& v);
    static ores::marketdata::messaging::get_curve_publish_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_curve_publish_version_request& v);
    static ores::marketdata::messaging::get_curve_publish_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_curve_publish_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_curve_publish_version_response& v);
    static ores::marketdata::messaging::get_curve_publish_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_curve_publish_version_response& v);
    static ores::marketdata::messaging::get_curve_publish_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
