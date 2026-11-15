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
#ifndef ORES_MARKETDATA_API_MESSAGING_FX_FORWARD_PUBLISH_SPOT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_FX_FORWARD_PUBLISH_SPOT_PROTOCOL_HPP

#include "ores.marketdata.api/domain/fx_forward_publish_spot_codec.hpp"
#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::marketdata::messaging {

struct fx_forward_publish_spot_key {
    boost::uuids::uuid id;

    friend bool operator==(const fx_forward_publish_spot_key&,
                           const fx_forward_publish_spot_key&) = default;
};

struct fx_forward_publish_spot_lookup {
    fx_forward_publish_spot_key key;
    std::optional<ores::marketdata::domain::fx_forward_publish_spot> fx_forward_publish_spot;

    friend bool operator==(const fx_forward_publish_spot_lookup&,
                           const fx_forward_publish_spot_lookup&) = default;
};

struct fx_forward_publish_spots_filter {
    std::optional<boost::uuids::uuid> publish_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> publish_id_one_of;

    friend bool operator==(const fx_forward_publish_spots_filter&,
                           const fx_forward_publish_spots_filter&) = default;
};

struct fx_forward_publish_spot_event {
    boost::uuids::uuid event_id;
    fx_forward_publish_spot_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const fx_forward_publish_spot_event&,
                           const fx_forward_publish_spot_event&) = default;
};

struct fx_forward_publish_spot_version_key {
    fx_forward_publish_spot_key fx_forward_publish_spot;
    std::uint32_t version{};

    friend bool operator==(const fx_forward_publish_spot_version_key&,
                           const fx_forward_publish_spot_version_key&) = default;
};

struct fx_forward_publish_spot_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const fx_forward_publish_spot_versions_filter&,
                           const fx_forward_publish_spot_versions_filter&) = default;
};

struct list_fx_forward_publish_spots_request {
    using response_type = struct list_fx_forward_publish_spots_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.fx_forward_publish_spots.list";
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
    std::optional<fx_forward_publish_spots_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_fx_forward_publish_spots_request&,
                           const list_fx_forward_publish_spots_request&) = default;
};

struct list_fx_forward_publish_spots_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::fx_forward_publish_spot> spots;
    std::uint64_t total{};

    friend bool operator==(const list_fx_forward_publish_spots_response&,
                           const list_fx_forward_publish_spots_response&) = default;
};

struct get_fx_forward_publish_spot_request {
    using response_type = struct get_fx_forward_publish_spot_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.fx_forward_publish_spots.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_forward_publish_spot_key key;

    friend bool operator==(const get_fx_forward_publish_spot_request&,
                           const get_fx_forward_publish_spot_request&) = default;
};

struct get_fx_forward_publish_spot_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::fx_forward_publish_spot> fx_forward_publish_spot;

    friend bool operator==(const get_fx_forward_publish_spot_response&,
                           const get_fx_forward_publish_spot_response&) = default;
};

struct get_many_fx_forward_publish_spots_request {
    using response_type = struct get_many_fx_forward_publish_spots_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.fx_forward_publish_spots.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<fx_forward_publish_spot_key> keys;

    friend bool operator==(const get_many_fx_forward_publish_spots_request&,
                           const get_many_fx_forward_publish_spots_request&) = default;
};

struct get_many_fx_forward_publish_spots_response {
    ores::utility::domain::result result;
    std::vector<fx_forward_publish_spot_lookup> entries;

    friend bool operator==(const get_many_fx_forward_publish_spots_response&,
                           const get_many_fx_forward_publish_spots_response&) = default;
};

struct list_fx_forward_publish_spot_versions_request {
    using response_type = struct list_fx_forward_publish_spot_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.fx_forward_publish_spots_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_forward_publish_spot_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<fx_forward_publish_spot_versions_filter> filter;

    friend bool operator==(const list_fx_forward_publish_spot_versions_request&,
                           const list_fx_forward_publish_spot_versions_request&) = default;
};

struct list_fx_forward_publish_spot_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::fx_forward_publish_spot> versions;
    std::uint64_t total{};

    friend bool operator==(const list_fx_forward_publish_spot_versions_response&,
                           const list_fx_forward_publish_spot_versions_response&) = default;
};

struct get_fx_forward_publish_spot_version_request {
    using response_type = struct get_fx_forward_publish_spot_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.fx_forward_publish_spots_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    fx_forward_publish_spot_version_key key;

    friend bool operator==(const get_fx_forward_publish_spot_version_request&,
                           const get_fx_forward_publish_spot_version_request&) = default;
};

struct get_fx_forward_publish_spot_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::fx_forward_publish_spot> version;

    friend bool operator==(const get_fx_forward_publish_spot_version_response&,
                           const get_fx_forward_publish_spot_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace fx_forward_publish_spot_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.fx_forward_publish_spots.created";
inline constexpr std::string_view updated = "marketdata.v1.events.fx_forward_publish_spots.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.fx_forward_publish_spots.deleted";
}
/**
 * @brief The members of fx_forward_publish_spot_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_publish_spot_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &fx_forward_publish_spot_key::id},
    };
}

/**
 * @brief The members of fx_forward_publish_spot_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_publish_spot_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &fx_forward_publish_spot_lookup::key},
        utility::codec::field{"fx_forward_publish_spot",
                              &fx_forward_publish_spot_lookup::fx_forward_publish_spot},
    };
}

/**
 * @brief The members of fx_forward_publish_spots_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_publish_spots_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"publish_id", &fx_forward_publish_spots_filter::publish_id},
        utility::codec::field{"id_one_of", &fx_forward_publish_spots_filter::id_one_of},
        utility::codec::field{"publish_id_one_of",
                              &fx_forward_publish_spots_filter::publish_id_one_of},
    };
}

/**
 * @brief The members of fx_forward_publish_spot_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_publish_spot_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &fx_forward_publish_spot_event::event_id},
        utility::codec::field{"key", &fx_forward_publish_spot_event::key},
        utility::codec::field{"action", &fx_forward_publish_spot_event::action},
        utility::codec::field{"version", &fx_forward_publish_spot_event::version},
        utility::codec::field{"occurred_at", &fx_forward_publish_spot_event::occurred_at},
        utility::codec::field{"correlation_id", &fx_forward_publish_spot_event::correlation_id},
    };
}

/**
 * @brief The members of fx_forward_publish_spot_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<fx_forward_publish_spot_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"fx_forward_publish_spot",
                              &fx_forward_publish_spot_version_key::fx_forward_publish_spot},
        utility::codec::field{"version", &fx_forward_publish_spot_version_key::version},
    };
}

/**
 * @brief The members of fx_forward_publish_spot_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<fx_forward_publish_spot_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &fx_forward_publish_spot_versions_filter::version},
        utility::codec::field{"from_version",
                              &fx_forward_publish_spot_versions_filter::from_version},
        utility::codec::field{"to_version", &fx_forward_publish_spot_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_fx_forward_publish_spots_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_forward_publish_spots_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_fx_forward_publish_spots_request::offset},
        utility::codec::field{"limit", &list_fx_forward_publish_spots_request::limit},
        utility::codec::field{"order", &list_fx_forward_publish_spots_request::order},
        utility::codec::field{"filter", &list_fx_forward_publish_spots_request::filter},
        utility::codec::field{"as_of", &list_fx_forward_publish_spots_request::as_of},
    };
}

/**
 * @brief The members of list_fx_forward_publish_spots_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_forward_publish_spots_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_fx_forward_publish_spots_response::result},
        utility::codec::field{"spots", &list_fx_forward_publish_spots_response::spots},
        utility::codec::field{"total", &list_fx_forward_publish_spots_response::total},
    };
}

/**
 * @brief The members of get_fx_forward_publish_spot_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_forward_publish_spot_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_fx_forward_publish_spot_request::key},
    };
}

/**
 * @brief The members of get_fx_forward_publish_spot_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_fx_forward_publish_spot_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_fx_forward_publish_spot_response::result},
        utility::codec::field{"fx_forward_publish_spot",
                              &get_fx_forward_publish_spot_response::fx_forward_publish_spot},
    };
}

/**
 * @brief The members of get_many_fx_forward_publish_spots_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_fx_forward_publish_spots_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_fx_forward_publish_spots_request::keys},
    };
}

/**
 * @brief The members of get_many_fx_forward_publish_spots_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_fx_forward_publish_spots_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_fx_forward_publish_spots_response::result},
        utility::codec::field{"entries", &get_many_fx_forward_publish_spots_response::entries},
    };
}

/**
 * @brief The members of list_fx_forward_publish_spot_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_forward_publish_spot_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_fx_forward_publish_spot_versions_request::key},
        utility::codec::field{"offset", &list_fx_forward_publish_spot_versions_request::offset},
        utility::codec::field{"limit", &list_fx_forward_publish_spot_versions_request::limit},
        utility::codec::field{"order", &list_fx_forward_publish_spot_versions_request::order},
        utility::codec::field{"filter", &list_fx_forward_publish_spot_versions_request::filter},
    };
}

/**
 * @brief The members of list_fx_forward_publish_spot_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_fx_forward_publish_spot_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_fx_forward_publish_spot_versions_response::result},
        utility::codec::field{"versions",
                              &list_fx_forward_publish_spot_versions_response::versions},
        utility::codec::field{"total", &list_fx_forward_publish_spot_versions_response::total},
    };
}

/**
 * @brief The members of get_fx_forward_publish_spot_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_forward_publish_spot_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_fx_forward_publish_spot_version_request::key},
    };
}

/**
 * @brief The members of get_fx_forward_publish_spot_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_fx_forward_publish_spot_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_fx_forward_publish_spot_version_response::result},
        utility::codec::field{"version", &get_fx_forward_publish_spot_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fx_forward_publish_spot_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::fx_forward_publish_spot_key& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::fx_forward_publish_spot_key& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fx_forward_publish_spot_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::fx_forward_publish_spot_lookup& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::fx_forward_publish_spot_lookup& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fx_forward_publish_spots_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::fx_forward_publish_spots_filter& v);
    static ores::marketdata::messaging::fx_forward_publish_spots_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::fx_forward_publish_spots_filter& v);
    static ores::marketdata::messaging::fx_forward_publish_spots_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fx_forward_publish_spot_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::fx_forward_publish_spot_event& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::fx_forward_publish_spot_event& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fx_forward_publish_spot_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::fx_forward_publish_spot_version_key& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::fx_forward_publish_spot_version_key& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::fx_forward_publish_spot_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::fx_forward_publish_spot_versions_filter& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::fx_forward_publish_spot_versions_filter& v);
    static ores::marketdata::messaging::fx_forward_publish_spot_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_fx_forward_publish_spots_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_fx_forward_publish_spots_request& v);
    static ores::marketdata::messaging::list_fx_forward_publish_spots_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_fx_forward_publish_spots_request& v);
    static ores::marketdata::messaging::list_fx_forward_publish_spots_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_fx_forward_publish_spots_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_fx_forward_publish_spots_response& v);
    static ores::marketdata::messaging::list_fx_forward_publish_spots_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_fx_forward_publish_spots_response& v);
    static ores::marketdata::messaging::list_fx_forward_publish_spots_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_fx_forward_publish_spot_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_fx_forward_publish_spot_request& v);
    static ores::marketdata::messaging::get_fx_forward_publish_spot_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_fx_forward_publish_spot_request& v);
    static ores::marketdata::messaging::get_fx_forward_publish_spot_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_fx_forward_publish_spot_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_fx_forward_publish_spot_response& v);
    static ores::marketdata::messaging::get_fx_forward_publish_spot_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_fx_forward_publish_spot_response& v);
    static ores::marketdata::messaging::get_fx_forward_publish_spot_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_fx_forward_publish_spots_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_fx_forward_publish_spots_request& v);
    static ores::marketdata::messaging::get_many_fx_forward_publish_spots_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_fx_forward_publish_spots_request& v);
    static ores::marketdata::messaging::get_many_fx_forward_publish_spots_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_fx_forward_publish_spots_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_fx_forward_publish_spots_response& v);
    static ores::marketdata::messaging::get_many_fx_forward_publish_spots_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_fx_forward_publish_spots_response& v);
    static ores::marketdata::messaging::get_many_fx_forward_publish_spots_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_fx_forward_publish_spot_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_fx_forward_publish_spot_versions_request& v);
    static ores::marketdata::messaging::list_fx_forward_publish_spot_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_fx_forward_publish_spot_versions_request& v);
    static ores::marketdata::messaging::list_fx_forward_publish_spot_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_fx_forward_publish_spot_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::marketdata::messaging::list_fx_forward_publish_spot_versions_response& v);
    static ores::marketdata::messaging::list_fx_forward_publish_spot_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_fx_forward_publish_spot_versions_response& v);
    static ores::marketdata::messaging::list_fx_forward_publish_spot_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_fx_forward_publish_spot_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_fx_forward_publish_spot_version_request& v);
    static ores::marketdata::messaging::get_fx_forward_publish_spot_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_fx_forward_publish_spot_version_request& v);
    static ores::marketdata::messaging::get_fx_forward_publish_spot_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_fx_forward_publish_spot_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_fx_forward_publish_spot_version_response& v);
    static ores::marketdata::messaging::get_fx_forward_publish_spot_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::get_fx_forward_publish_spot_version_response& v);
    static ores::marketdata::messaging::get_fx_forward_publish_spot_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
