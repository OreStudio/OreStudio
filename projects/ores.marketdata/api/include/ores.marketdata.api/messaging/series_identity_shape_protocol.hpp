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
#ifndef ORES_MARKETDATA_API_MESSAGING_SERIES_IDENTITY_SHAPE_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_SERIES_IDENTITY_SHAPE_PROTOCOL_HPP

#include "ores.marketdata.api/domain/series_identity_shape_codec.hpp"
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

struct series_identity_shape_key {
    std::string asset_class;
    std::string series_subclass;
    std::string index_family;
    std::string field;

    friend bool operator==(const series_identity_shape_key&,
                           const series_identity_shape_key&) = default;
};

struct series_identity_shape_write {
    std::string asset_class;
    std::string series_subclass;
    std::string index_family;
    std::string field;
    std::string requirement;
    std::string description;

    friend bool operator==(const series_identity_shape_write&,
                           const series_identity_shape_write&) = default;
};

struct series_identity_shape_change {
    series_identity_shape_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const series_identity_shape_change&,
                           const series_identity_shape_change&) = default;
};

struct series_identity_shape_removal {
    series_identity_shape_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const series_identity_shape_removal&,
                           const series_identity_shape_removal&) = default;
};

struct series_identity_shape_lookup {
    series_identity_shape_key key;
    std::optional<ores::marketdata::domain::series_identity_shape> series_identity_shape;

    friend bool operator==(const series_identity_shape_lookup&,
                           const series_identity_shape_lookup&) = default;
};

struct series_identity_shape_event {
    boost::uuids::uuid event_id;
    series_identity_shape_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const series_identity_shape_event&,
                           const series_identity_shape_event&) = default;
};

struct series_identity_shape_version_key {
    series_identity_shape_key series_identity_shape;
    std::uint32_t version{};

    friend bool operator==(const series_identity_shape_version_key&,
                           const series_identity_shape_version_key&) = default;
};

struct series_identity_shape_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const series_identity_shape_versions_filter&,
                           const series_identity_shape_versions_filter&) = default;
};

struct list_series_identity_shapes_request {
    using response_type = struct list_series_identity_shapes_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.series_identity_shapes.list";
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
    std::optional<std::string> as_of;

    friend bool operator==(const list_series_identity_shapes_request&,
                           const list_series_identity_shapes_request&) = default;
};

struct list_series_identity_shapes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::series_identity_shape> shapes;
    std::uint64_t total{};

    friend bool operator==(const list_series_identity_shapes_response&,
                           const list_series_identity_shapes_response&) = default;
};

struct get_series_identity_shape_request {
    using response_type = struct get_series_identity_shape_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.series_identity_shapes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_identity_shape_key key;

    friend bool operator==(const get_series_identity_shape_request&,
                           const get_series_identity_shape_request&) = default;
};

struct get_series_identity_shape_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::series_identity_shape> series_identity_shape;

    friend bool operator==(const get_series_identity_shape_response&,
                           const get_series_identity_shape_response&) = default;
};

struct get_many_series_identity_shapes_request {
    using response_type = struct get_many_series_identity_shapes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.series_identity_shapes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_identity_shape_key> keys;

    friend bool operator==(const get_many_series_identity_shapes_request&,
                           const get_many_series_identity_shapes_request&) = default;
};

struct get_many_series_identity_shapes_response {
    ores::utility::domain::result result;
    std::vector<series_identity_shape_lookup> entries;

    friend bool operator==(const get_many_series_identity_shapes_response&,
                           const get_many_series_identity_shapes_response&) = default;
};

struct put_series_identity_shape_request {
    using response_type = struct put_series_identity_shape_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.series_identity_shapes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_identity_shape_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_series_identity_shape_request&,
                           const put_series_identity_shape_request&) = default;
};

struct put_series_identity_shape_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::series_identity_shape> series_identity_shape;

    friend bool operator==(const put_series_identity_shape_response&,
                           const put_series_identity_shape_response&) = default;
};

struct put_many_series_identity_shapes_request {
    using response_type = struct put_many_series_identity_shapes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.series_identity_shapes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_identity_shape_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_series_identity_shapes_request&,
                           const put_many_series_identity_shapes_request&) = default;
};

struct put_many_series_identity_shapes_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::series_identity_shape> shapes;

    friend bool operator==(const put_many_series_identity_shapes_response&,
                           const put_many_series_identity_shapes_response&) = default;
};

struct delete_series_identity_shape_request {
    using response_type = struct delete_series_identity_shape_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.series_identity_shapes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_identity_shape_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_series_identity_shape_request&,
                           const delete_series_identity_shape_request&) = default;
};

struct delete_series_identity_shape_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_series_identity_shape_response&,
                           const delete_series_identity_shape_response&) = default;
};

struct delete_many_series_identity_shapes_request {
    using response_type = struct delete_many_series_identity_shapes_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.series_identity_shapes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<series_identity_shape_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_series_identity_shapes_request&,
                           const delete_many_series_identity_shapes_request&) = default;
};

struct delete_many_series_identity_shapes_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_series_identity_shapes_response&,
                           const delete_many_series_identity_shapes_response&) = default;
};

struct list_series_identity_shape_versions_request {
    using response_type = struct list_series_identity_shape_versions_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.series_identity_shapes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_identity_shape_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<series_identity_shape_versions_filter> filter;

    friend bool operator==(const list_series_identity_shape_versions_request&,
                           const list_series_identity_shape_versions_request&) = default;
};

struct list_series_identity_shape_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::marketdata::domain::series_identity_shape> versions;
    std::uint64_t total{};

    friend bool operator==(const list_series_identity_shape_versions_response&,
                           const list_series_identity_shape_versions_response&) = default;
};

struct get_series_identity_shape_version_request {
    using response_type = struct get_series_identity_shape_version_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.series_identity_shapes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    series_identity_shape_version_key key;

    friend bool operator==(const get_series_identity_shape_version_request&,
                           const get_series_identity_shape_version_request&) = default;
};

struct get_series_identity_shape_version_response {
    ores::utility::domain::result result;
    std::optional<ores::marketdata::domain::series_identity_shape> version;

    friend bool operator==(const get_series_identity_shape_version_response&,
                           const get_series_identity_shape_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace series_identity_shape_event_subjects {
inline constexpr std::string_view created = "marketdata.v1.events.series_identity_shapes.created";
inline constexpr std::string_view updated = "marketdata.v1.events.series_identity_shapes.updated";
inline constexpr std::string_view deleted = "marketdata.v1.events.series_identity_shapes.deleted";
}
/**
 * @brief The members of series_identity_shape_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_identity_shape_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"asset_class", &series_identity_shape_key::asset_class},
        utility::codec::field{"series_subclass", &series_identity_shape_key::series_subclass},
        utility::codec::field{"index_family", &series_identity_shape_key::index_family},
        utility::codec::field{"field", &series_identity_shape_key::field},
    };
}

/**
 * @brief The members of series_identity_shape_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_identity_shape_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"asset_class", &series_identity_shape_write::asset_class},
        utility::codec::field{"series_subclass", &series_identity_shape_write::series_subclass},
        utility::codec::field{"index_family", &series_identity_shape_write::index_family},
        utility::codec::field{"field", &series_identity_shape_write::field},
        utility::codec::field{"requirement", &series_identity_shape_write::requirement},
        utility::codec::field{"description", &series_identity_shape_write::description},
    };
}

/**
 * @brief The members of series_identity_shape_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_identity_shape_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &series_identity_shape_change::write},
        utility::codec::field{"precondition", &series_identity_shape_change::precondition},
    };
}

/**
 * @brief The members of series_identity_shape_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_identity_shape_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &series_identity_shape_removal::key},
        utility::codec::field{"precondition", &series_identity_shape_removal::precondition},
    };
}

/**
 * @brief The members of series_identity_shape_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_identity_shape_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &series_identity_shape_lookup::key},
        utility::codec::field{"series_identity_shape",
                              &series_identity_shape_lookup::series_identity_shape},
    };
}

/**
 * @brief The members of series_identity_shape_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_identity_shape_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &series_identity_shape_event::event_id},
        utility::codec::field{"key", &series_identity_shape_event::key},
        utility::codec::field{"action", &series_identity_shape_event::action},
        utility::codec::field{"version", &series_identity_shape_event::version},
        utility::codec::field{"occurred_at", &series_identity_shape_event::occurred_at},
        utility::codec::field{"correlation_id", &series_identity_shape_event::correlation_id},
    };
}

/**
 * @brief The members of series_identity_shape_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_identity_shape_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"series_identity_shape",
                              &series_identity_shape_version_key::series_identity_shape},
        utility::codec::field{"version", &series_identity_shape_version_key::version},
    };
}

/**
 * @brief The members of series_identity_shape_versions_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<series_identity_shape_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &series_identity_shape_versions_filter::version},
        utility::codec::field{"from_version", &series_identity_shape_versions_filter::from_version},
        utility::codec::field{"to_version", &series_identity_shape_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_series_identity_shapes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_series_identity_shapes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_series_identity_shapes_request::offset},
        utility::codec::field{"limit", &list_series_identity_shapes_request::limit},
        utility::codec::field{"order", &list_series_identity_shapes_request::order},
        utility::codec::field{"as_of", &list_series_identity_shapes_request::as_of},
    };
}

/**
 * @brief The members of list_series_identity_shapes_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_series_identity_shapes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_series_identity_shapes_response::result},
        utility::codec::field{"shapes", &list_series_identity_shapes_response::shapes},
        utility::codec::field{"total", &list_series_identity_shapes_response::total},
    };
}

/**
 * @brief The members of get_series_identity_shape_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_series_identity_shape_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_series_identity_shape_request::key},
    };
}

/**
 * @brief The members of get_series_identity_shape_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_series_identity_shape_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_series_identity_shape_response::result},
        utility::codec::field{"series_identity_shape",
                              &get_series_identity_shape_response::series_identity_shape},
    };
}

/**
 * @brief The members of get_many_series_identity_shapes_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_series_identity_shapes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_series_identity_shapes_request::keys},
    };
}

/**
 * @brief The members of get_many_series_identity_shapes_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_series_identity_shapes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_series_identity_shapes_response::result},
        utility::codec::field{"entries", &get_many_series_identity_shapes_response::entries},
    };
}

/**
 * @brief The members of put_series_identity_shape_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_series_identity_shape_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_series_identity_shape_request::change},
        utility::codec::field{"intent", &put_series_identity_shape_request::intent},
    };
}

/**
 * @brief The members of put_series_identity_shape_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_series_identity_shape_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_series_identity_shape_response::result},
        utility::codec::field{"series_identity_shape",
                              &put_series_identity_shape_response::series_identity_shape},
    };
}

/**
 * @brief The members of put_many_series_identity_shapes_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_series_identity_shapes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_series_identity_shapes_request::changes},
        utility::codec::field{"intent", &put_many_series_identity_shapes_request::intent},
    };
}

/**
 * @brief The members of put_many_series_identity_shapes_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_series_identity_shapes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_series_identity_shapes_response::result},
        utility::codec::field{"shapes", &put_many_series_identity_shapes_response::shapes},
    };
}

/**
 * @brief The members of delete_series_identity_shape_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_series_identity_shape_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_series_identity_shape_request::removal},
        utility::codec::field{"intent", &delete_series_identity_shape_request::intent},
    };
}

/**
 * @brief The members of delete_series_identity_shape_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_series_identity_shape_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_series_identity_shape_response::result},
    };
}

/**
 * @brief The members of delete_many_series_identity_shapes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_series_identity_shapes_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_series_identity_shapes_request::removals},
        utility::codec::field{"intent", &delete_many_series_identity_shapes_request::intent},
    };
}

/**
 * @brief The members of delete_many_series_identity_shapes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_series_identity_shapes_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_series_identity_shapes_response::result},
    };
}

/**
 * @brief The members of list_series_identity_shape_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_series_identity_shape_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_series_identity_shape_versions_request::key},
        utility::codec::field{"offset", &list_series_identity_shape_versions_request::offset},
        utility::codec::field{"limit", &list_series_identity_shape_versions_request::limit},
        utility::codec::field{"order", &list_series_identity_shape_versions_request::order},
        utility::codec::field{"filter", &list_series_identity_shape_versions_request::filter},
    };
}

/**
 * @brief The members of list_series_identity_shape_versions_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_series_identity_shape_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_series_identity_shape_versions_response::result},
        utility::codec::field{"versions", &list_series_identity_shape_versions_response::versions},
        utility::codec::field{"total", &list_series_identity_shape_versions_response::total},
    };
}

/**
 * @brief The members of get_series_identity_shape_version_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_series_identity_shape_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_series_identity_shape_version_request::key},
    };
}

/**
 * @brief The members of get_series_identity_shape_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_series_identity_shape_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_series_identity_shape_version_response::result},
        utility::codec::field{"version", &get_series_identity_shape_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::series_identity_shape_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::series_identity_shape_key& v);
    static ores::marketdata::messaging::series_identity_shape_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::series_identity_shape_key& v);
    static ores::marketdata::messaging::series_identity_shape_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::series_identity_shape_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::series_identity_shape_write& v);
    static ores::marketdata::messaging::series_identity_shape_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::series_identity_shape_write& v);
    static ores::marketdata::messaging::series_identity_shape_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::series_identity_shape_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::series_identity_shape_change& v);
    static ores::marketdata::messaging::series_identity_shape_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::series_identity_shape_change& v);
    static ores::marketdata::messaging::series_identity_shape_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::series_identity_shape_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::series_identity_shape_removal& v);
    static ores::marketdata::messaging::series_identity_shape_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::series_identity_shape_removal& v);
    static ores::marketdata::messaging::series_identity_shape_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::series_identity_shape_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::series_identity_shape_lookup& v);
    static ores::marketdata::messaging::series_identity_shape_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::series_identity_shape_lookup& v);
    static ores::marketdata::messaging::series_identity_shape_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::series_identity_shape_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::series_identity_shape_event& v);
    static ores::marketdata::messaging::series_identity_shape_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::series_identity_shape_event& v);
    static ores::marketdata::messaging::series_identity_shape_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::series_identity_shape_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::series_identity_shape_version_key& v);
    static ores::marketdata::messaging::series_identity_shape_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::series_identity_shape_version_key& v);
    static ores::marketdata::messaging::series_identity_shape_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::series_identity_shape_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::series_identity_shape_versions_filter& v);
    static ores::marketdata::messaging::series_identity_shape_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::series_identity_shape_versions_filter& v);
    static ores::marketdata::messaging::series_identity_shape_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_series_identity_shapes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_series_identity_shapes_request& v);
    static ores::marketdata::messaging::list_series_identity_shapes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_series_identity_shapes_request& v);
    static ores::marketdata::messaging::list_series_identity_shapes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_series_identity_shapes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_series_identity_shapes_response& v);
    static ores::marketdata::messaging::list_series_identity_shapes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_series_identity_shapes_response& v);
    static ores::marketdata::messaging::list_series_identity_shapes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_identity_shape_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_series_identity_shape_request& v);
    static ores::marketdata::messaging::get_series_identity_shape_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_identity_shape_request& v);
    static ores::marketdata::messaging::get_series_identity_shape_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_identity_shape_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_series_identity_shape_response& v);
    static ores::marketdata::messaging::get_series_identity_shape_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_identity_shape_response& v);
    static ores::marketdata::messaging::get_series_identity_shape_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_series_identity_shapes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_series_identity_shapes_request& v);
    static ores::marketdata::messaging::get_many_series_identity_shapes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_series_identity_shapes_request& v);
    static ores::marketdata::messaging::get_many_series_identity_shapes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_many_series_identity_shapes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_many_series_identity_shapes_response& v);
    static ores::marketdata::messaging::get_many_series_identity_shapes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_many_series_identity_shapes_response& v);
    static ores::marketdata::messaging::get_many_series_identity_shapes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_series_identity_shape_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_series_identity_shape_request& v);
    static ores::marketdata::messaging::put_series_identity_shape_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_series_identity_shape_request& v);
    static ores::marketdata::messaging::put_series_identity_shape_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_series_identity_shape_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_series_identity_shape_response& v);
    static ores::marketdata::messaging::put_series_identity_shape_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_series_identity_shape_response& v);
    static ores::marketdata::messaging::put_series_identity_shape_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_series_identity_shapes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_series_identity_shapes_request& v);
    static ores::marketdata::messaging::put_many_series_identity_shapes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_series_identity_shapes_request& v);
    static ores::marketdata::messaging::put_many_series_identity_shapes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::put_many_series_identity_shapes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::put_many_series_identity_shapes_response& v);
    static ores::marketdata::messaging::put_many_series_identity_shapes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::put_many_series_identity_shapes_response& v);
    static ores::marketdata::messaging::put_many_series_identity_shapes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_series_identity_shape_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_series_identity_shape_request& v);
    static ores::marketdata::messaging::delete_series_identity_shape_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_series_identity_shape_request& v);
    static ores::marketdata::messaging::delete_series_identity_shape_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_series_identity_shape_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_series_identity_shape_response& v);
    static ores::marketdata::messaging::delete_series_identity_shape_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_series_identity_shape_response& v);
    static ores::marketdata::messaging::delete_series_identity_shape_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_series_identity_shapes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_series_identity_shapes_request& v);
    static ores::marketdata::messaging::delete_many_series_identity_shapes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::delete_many_series_identity_shapes_request& v);
    static ores::marketdata::messaging::delete_many_series_identity_shapes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::delete_many_series_identity_shapes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::delete_many_series_identity_shapes_response& v);
    static ores::marketdata::messaging::delete_many_series_identity_shapes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::delete_many_series_identity_shapes_response& v);
    static ores::marketdata::messaging::delete_many_series_identity_shapes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_series_identity_shape_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_series_identity_shape_versions_request& v);
    static ores::marketdata::messaging::list_series_identity_shape_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_series_identity_shape_versions_request& v);
    static ores::marketdata::messaging::list_series_identity_shape_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_series_identity_shape_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_series_identity_shape_versions_response& v);
    static ores::marketdata::messaging::list_series_identity_shape_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::marketdata::messaging::list_series_identity_shape_versions_response& v);
    static ores::marketdata::messaging::list_series_identity_shape_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_identity_shape_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_series_identity_shape_version_request& v);
    static ores::marketdata::messaging::get_series_identity_shape_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_identity_shape_version_request& v);
    static ores::marketdata::messaging::get_series_identity_shape_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_identity_shape_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_series_identity_shape_version_response& v);
    static ores::marketdata::messaging::get_series_identity_shape_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_identity_shape_version_response& v);
    static ores::marketdata::messaging::get_series_identity_shape_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
