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
#ifndef ORES_ANALYTICS_API_MESSAGING_TODAYS_MARKET_COLLECTION_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_TODAYS_MARKET_COLLECTION_PROTOCOL_HPP

#include "ores.analytics.api/domain/todays_market_collection.hpp"
#include "ores.analytics.api/export.hpp"
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

namespace ores::analytics::messaging {

struct todays_market_collection_key {
    std::string collection;

    friend bool operator==(const todays_market_collection_key&,
                           const todays_market_collection_key&) = default;
};

struct todays_market_collection_write {
    boost::uuids::uuid id;
    boost::uuids::uuid todays_market_config_id;
    std::string collection;
    std::optional<std::string> collection_id;
    int position;

    friend bool operator==(const todays_market_collection_write&,
                           const todays_market_collection_write&) = default;
};

struct todays_market_collection_change {
    todays_market_collection_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const todays_market_collection_change&,
                           const todays_market_collection_change&) = default;
};

struct todays_market_collection_removal {
    todays_market_collection_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const todays_market_collection_removal&,
                           const todays_market_collection_removal&) = default;
};

struct todays_market_collection_lookup {
    todays_market_collection_key key;
    std::optional<ores::analytics::domain::todays_market_collection> todays_market_collection;

    friend bool operator==(const todays_market_collection_lookup&,
                           const todays_market_collection_lookup&) = default;
};

struct todays_market_collections_filter {
    std::optional<boost::uuids::uuid> todays_market_config_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> todays_market_config_id_one_of;

    friend bool operator==(const todays_market_collections_filter&,
                           const todays_market_collections_filter&) = default;
};

struct todays_market_collection_event {
    boost::uuids::uuid event_id;
    todays_market_collection_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const todays_market_collection_event&,
                           const todays_market_collection_event&) = default;
};

struct todays_market_collection_version_key {
    todays_market_collection_key todays_market_collection;
    std::uint32_t version;

    friend bool operator==(const todays_market_collection_version_key&,
                           const todays_market_collection_version_key&) = default;
};

struct todays_market_collection_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const todays_market_collection_versions_filter&,
                           const todays_market_collection_versions_filter&) = default;
};

struct list_todays_market_collections_request {
    using response_type = struct list_todays_market_collections_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_collections.list";
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
    std::optional<todays_market_collections_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_todays_market_collections_request&,
                           const list_todays_market_collections_request&) = default;
};

struct list_todays_market_collections_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_collection> collections;
    std::uint64_t total;

    friend bool operator==(const list_todays_market_collections_response&,
                           const list_todays_market_collections_response&) = default;
};

struct get_todays_market_collection_request {
    using response_type = struct get_todays_market_collection_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_collections.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_key key;

    friend bool operator==(const get_todays_market_collection_request&,
                           const get_todays_market_collection_request&) = default;
};

struct get_todays_market_collection_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_collection> todays_market_collection;

    friend bool operator==(const get_todays_market_collection_response&,
                           const get_todays_market_collection_response&) = default;
};

struct get_many_todays_market_collections_request {
    using response_type = struct get_many_todays_market_collections_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collections.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_collection_key> keys;

    friend bool operator==(const get_many_todays_market_collections_request&,
                           const get_many_todays_market_collections_request&) = default;
};

struct get_many_todays_market_collections_response {
    ores::utility::domain::result result;
    std::vector<todays_market_collection_lookup> entries;

    friend bool operator==(const get_many_todays_market_collections_response&,
                           const get_many_todays_market_collections_response&) = default;
};

struct put_todays_market_collection_request {
    using response_type = struct put_todays_market_collection_response;
    static constexpr std::string_view nats_subject = "analytics.v1.todays_market_collections.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_todays_market_collection_request&,
                           const put_todays_market_collection_request&) = default;
};

struct put_todays_market_collection_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_collection> todays_market_collection;

    friend bool operator==(const put_todays_market_collection_response&,
                           const put_todays_market_collection_response&) = default;
};

struct put_many_todays_market_collections_request {
    using response_type = struct put_many_todays_market_collections_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collections.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_collection_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_todays_market_collections_request&,
                           const put_many_todays_market_collections_request&) = default;
};

struct put_many_todays_market_collections_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_collection> collections;

    friend bool operator==(const put_many_todays_market_collections_response&,
                           const put_many_todays_market_collections_response&) = default;
};

struct delete_todays_market_collection_request {
    using response_type = struct delete_todays_market_collection_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collections.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_todays_market_collection_request&,
                           const delete_todays_market_collection_request&) = default;
};

struct delete_todays_market_collection_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_todays_market_collection_response&,
                           const delete_todays_market_collection_response&) = default;
};

struct delete_many_todays_market_collections_request {
    using response_type = struct delete_many_todays_market_collections_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collections.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_collection_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_todays_market_collections_request&,
                           const delete_many_todays_market_collections_request&) = default;
};

struct delete_many_todays_market_collections_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_todays_market_collections_response&,
                           const delete_many_todays_market_collections_response&) = default;
};

struct list_by_todays_market_config_id_todays_market_collections_request {
    using response_type = struct list_by_todays_market_config_id_todays_market_collections_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collections.list_by_todays_market_config_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid todays_market_config_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<todays_market_collections_filter> filter;

    friend bool
    operator==(const list_by_todays_market_config_id_todays_market_collections_request&,
               const list_by_todays_market_config_id_todays_market_collections_request&) = default;
};

struct list_by_todays_market_config_id_todays_market_collections_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_collection> collections;
    std::uint64_t total;

    friend bool
    operator==(const list_by_todays_market_config_id_todays_market_collections_response&,
               const list_by_todays_market_config_id_todays_market_collections_response&) = default;
};

struct list_todays_market_collection_versions_request {
    using response_type = struct list_todays_market_collection_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collections_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<todays_market_collection_versions_filter> filter;

    friend bool operator==(const list_todays_market_collection_versions_request&,
                           const list_todays_market_collection_versions_request&) = default;
};

struct list_todays_market_collection_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_collection> versions;
    std::uint64_t total;

    friend bool operator==(const list_todays_market_collection_versions_response&,
                           const list_todays_market_collection_versions_response&) = default;
};

struct get_todays_market_collection_version_request {
    using response_type = struct get_todays_market_collection_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_collections_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_collection_version_key key;

    friend bool operator==(const get_todays_market_collection_version_request&,
                           const get_todays_market_collection_version_request&) = default;
};

struct get_todays_market_collection_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_collection> version;

    friend bool operator==(const get_todays_market_collection_version_response&,
                           const get_todays_market_collection_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace todays_market_collection_event_subjects {
inline constexpr std::string_view created = "analytics.v1.events.todays_market_collections.created";
inline constexpr std::string_view updated = "analytics.v1.events.todays_market_collections.updated";
inline constexpr std::string_view deleted = "analytics.v1.events.todays_market_collections.deleted";
}
/**
 * @brief The members of todays_market_collection_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_collection_key>) {
    return std::tuple{
        utility::codec::field{"collection", &todays_market_collection_key::collection},
    };
}

/**
 * @brief The members of todays_market_collection_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_collection_write>) {
    return std::tuple{
        utility::codec::field{"id", &todays_market_collection_write::id},
        utility::codec::field{"todays_market_config_id",
                              &todays_market_collection_write::todays_market_config_id},
        utility::codec::field{"collection", &todays_market_collection_write::collection},
        utility::codec::field{"collection_id", &todays_market_collection_write::collection_id},
        utility::codec::field{"position", &todays_market_collection_write::position},
    };
}

/**
 * @brief The members of todays_market_collection_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_collection_change>) {
    return std::tuple{
        utility::codec::field{"write", &todays_market_collection_change::write},
        utility::codec::field{"precondition", &todays_market_collection_change::precondition},
    };
}

/**
 * @brief The members of todays_market_collection_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_collection_removal>) {
    return std::tuple{
        utility::codec::field{"key", &todays_market_collection_removal::key},
        utility::codec::field{"precondition", &todays_market_collection_removal::precondition},
    };
}

/**
 * @brief The members of todays_market_collection_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_collection_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &todays_market_collection_lookup::key},
        utility::codec::field{"todays_market_collection",
                              &todays_market_collection_lookup::todays_market_collection},
    };
}

/**
 * @brief The members of todays_market_collections_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_collections_filter>) {
    return std::tuple{
        utility::codec::field{"todays_market_config_id",
                              &todays_market_collections_filter::todays_market_config_id},
        utility::codec::field{"id_one_of", &todays_market_collections_filter::id_one_of},
        utility::codec::field{"todays_market_config_id_one_of",
                              &todays_market_collections_filter::todays_market_config_id_one_of},
    };
}

/**
 * @brief The members of todays_market_collection_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_collection_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &todays_market_collection_event::event_id},
        utility::codec::field{"key", &todays_market_collection_event::key},
        utility::codec::field{"action", &todays_market_collection_event::action},
        utility::codec::field{"version", &todays_market_collection_event::version},
        utility::codec::field{"occurred_at", &todays_market_collection_event::occurred_at},
        utility::codec::field{"correlation_id", &todays_market_collection_event::correlation_id},
    };
}

/**
 * @brief The members of todays_market_collection_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_collection_version_key>) {
    return std::tuple{
        utility::codec::field{"todays_market_collection",
                              &todays_market_collection_version_key::todays_market_collection},
        utility::codec::field{"version", &todays_market_collection_version_key::version},
    };
}

/**
 * @brief The members of todays_market_collection_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<todays_market_collection_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &todays_market_collection_versions_filter::version},
        utility::codec::field{"from_version",
                              &todays_market_collection_versions_filter::from_version},
        utility::codec::field{"to_version", &todays_market_collection_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_todays_market_collections_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_todays_market_collections_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_todays_market_collections_request::offset},
        utility::codec::field{"limit", &list_todays_market_collections_request::limit},
        utility::codec::field{"order", &list_todays_market_collections_request::order},
        utility::codec::field{"filter", &list_todays_market_collections_request::filter},
        utility::codec::field{"as_of", &list_todays_market_collections_request::as_of},
    };
}

/**
 * @brief The members of list_todays_market_collections_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_todays_market_collections_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_todays_market_collections_response::result},
        utility::codec::field{"collections", &list_todays_market_collections_response::collections},
        utility::codec::field{"total", &list_todays_market_collections_response::total},
    };
}

/**
 * @brief The members of get_todays_market_collection_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_todays_market_collection_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_todays_market_collection_request::key},
    };
}

/**
 * @brief The members of get_todays_market_collection_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_todays_market_collection_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_todays_market_collection_response::result},
        utility::codec::field{"todays_market_collection",
                              &get_todays_market_collection_response::todays_market_collection},
    };
}

/**
 * @brief The members of get_many_todays_market_collections_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_todays_market_collections_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_todays_market_collections_request::keys},
    };
}

/**
 * @brief The members of get_many_todays_market_collections_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_todays_market_collections_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_todays_market_collections_response::result},
        utility::codec::field{"entries", &get_many_todays_market_collections_response::entries},
    };
}

/**
 * @brief The members of put_todays_market_collection_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_todays_market_collection_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_todays_market_collection_request::change},
        utility::codec::field{"intent", &put_todays_market_collection_request::intent},
    };
}

/**
 * @brief The members of put_todays_market_collection_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_todays_market_collection_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_todays_market_collection_response::result},
        utility::codec::field{"todays_market_collection",
                              &put_todays_market_collection_response::todays_market_collection},
    };
}

/**
 * @brief The members of put_many_todays_market_collections_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_todays_market_collections_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_todays_market_collections_request::changes},
        utility::codec::field{"intent", &put_many_todays_market_collections_request::intent},
    };
}

/**
 * @brief The members of put_many_todays_market_collections_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_todays_market_collections_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_todays_market_collections_response::result},
        utility::codec::field{"collections",
                              &put_many_todays_market_collections_response::collections},
    };
}

/**
 * @brief The members of delete_todays_market_collection_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_todays_market_collection_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_todays_market_collection_request::removal},
        utility::codec::field{"intent", &delete_todays_market_collection_request::intent},
    };
}

/**
 * @brief The members of delete_todays_market_collection_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_todays_market_collection_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_todays_market_collection_response::result},
    };
}

/**
 * @brief The members of delete_many_todays_market_collections_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_todays_market_collections_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_todays_market_collections_request::removals},
        utility::codec::field{"intent", &delete_many_todays_market_collections_request::intent},
    };
}

/**
 * @brief The members of delete_many_todays_market_collections_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_todays_market_collections_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_todays_market_collections_response::result},
    };
}

/**
 * @brief The members of list_by_todays_market_config_id_todays_market_collections_request, named as
 * the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_todays_market_config_id_todays_market_collections_request>) {
    return std::tuple{
        utility::codec::field{"todays_market_config_id",
                              &list_by_todays_market_config_id_todays_market_collections_request::
                                  todays_market_config_id},
        utility::codec::field{
            "scope", &list_by_todays_market_config_id_todays_market_collections_request::scope},
        utility::codec::field{
            "offset", &list_by_todays_market_config_id_todays_market_collections_request::offset},
        utility::codec::field{
            "limit", &list_by_todays_market_config_id_todays_market_collections_request::limit},
        utility::codec::field{
            "order", &list_by_todays_market_config_id_todays_market_collections_request::order},
        utility::codec::field{
            "filter", &list_by_todays_market_config_id_todays_market_collections_request::filter},
    };
}

/**
 * @brief The members of list_by_todays_market_config_id_todays_market_collections_response, named
 * as the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(
    std::type_identity<list_by_todays_market_config_id_todays_market_collections_response>) {
    return std::tuple{
        utility::codec::field{
            "result", &list_by_todays_market_config_id_todays_market_collections_response::result},
        utility::codec::field{
            "collections",
            &list_by_todays_market_config_id_todays_market_collections_response::collections},
        utility::codec::field{
            "total", &list_by_todays_market_config_id_todays_market_collections_response::total},
    };
}

/**
 * @brief The members of list_todays_market_collection_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_todays_market_collection_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_todays_market_collection_versions_request::key},
        utility::codec::field{"offset", &list_todays_market_collection_versions_request::offset},
        utility::codec::field{"limit", &list_todays_market_collection_versions_request::limit},
        utility::codec::field{"order", &list_todays_market_collection_versions_request::order},
        utility::codec::field{"filter", &list_todays_market_collection_versions_request::filter},
    };
}

/**
 * @brief The members of list_todays_market_collection_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_todays_market_collection_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_todays_market_collection_versions_response::result},
        utility::codec::field{"versions",
                              &list_todays_market_collection_versions_response::versions},
        utility::codec::field{"total", &list_todays_market_collection_versions_response::total},
    };
}

/**
 * @brief The members of get_todays_market_collection_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_todays_market_collection_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_todays_market_collection_version_request::key},
    };
}

/**
 * @brief The members of get_todays_market_collection_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_todays_market_collection_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_todays_market_collection_version_response::result},
        utility::codec::field{"version", &get_todays_market_collection_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collection_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collection_key& v);
    static ores::analytics::messaging::todays_market_collection_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collection_key& v);
    static ores::analytics::messaging::todays_market_collection_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collection_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collection_write& v);
    static ores::analytics::messaging::todays_market_collection_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collection_write& v);
    static ores::analytics::messaging::todays_market_collection_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collection_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collection_change& v);
    static ores::analytics::messaging::todays_market_collection_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collection_change& v);
    static ores::analytics::messaging::todays_market_collection_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collection_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collection_removal& v);
    static ores::analytics::messaging::todays_market_collection_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collection_removal& v);
    static ores::analytics::messaging::todays_market_collection_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collection_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collection_lookup& v);
    static ores::analytics::messaging::todays_market_collection_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collection_lookup& v);
    static ores::analytics::messaging::todays_market_collection_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collections_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collections_filter& v);
    static ores::analytics::messaging::todays_market_collections_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collections_filter& v);
    static ores::analytics::messaging::todays_market_collections_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collection_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collection_event& v);
    static ores::analytics::messaging::todays_market_collection_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collection_event& v);
    static ores::analytics::messaging::todays_market_collection_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collection_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collection_version_key& v);
    static ores::analytics::messaging::todays_market_collection_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collection_version_key& v);
    static ores::analytics::messaging::todays_market_collection_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_collection_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_collection_versions_filter& v);
    static ores::analytics::messaging::todays_market_collection_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_collection_versions_filter& v);
    static ores::analytics::messaging::todays_market_collection_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_todays_market_collections_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_todays_market_collections_request& v);
    static ores::analytics::messaging::list_todays_market_collections_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_todays_market_collections_request& v);
    static ores::analytics::messaging::list_todays_market_collections_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_todays_market_collections_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_todays_market_collections_response& v);
    static ores::analytics::messaging::list_todays_market_collections_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_todays_market_collections_response& v);
    static ores::analytics::messaging::list_todays_market_collections_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_collection_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_todays_market_collection_request& v);
    static ores::analytics::messaging::get_todays_market_collection_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_todays_market_collection_request& v);
    static ores::analytics::messaging::get_todays_market_collection_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_collection_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_todays_market_collection_response& v);
    static ores::analytics::messaging::get_todays_market_collection_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_todays_market_collection_response& v);
    static ores::analytics::messaging::get_todays_market_collection_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_todays_market_collections_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_todays_market_collections_request& v);
    static ores::analytics::messaging::get_many_todays_market_collections_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_todays_market_collections_request& v);
    static ores::analytics::messaging::get_many_todays_market_collections_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_todays_market_collections_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_todays_market_collections_response& v);
    static ores::analytics::messaging::get_many_todays_market_collections_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_todays_market_collections_response& v);
    static ores::analytics::messaging::get_many_todays_market_collections_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_todays_market_collection_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_todays_market_collection_request& v);
    static ores::analytics::messaging::put_todays_market_collection_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_todays_market_collection_request& v);
    static ores::analytics::messaging::put_todays_market_collection_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_todays_market_collection_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_todays_market_collection_response& v);
    static ores::analytics::messaging::put_todays_market_collection_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_todays_market_collection_response& v);
    static ores::analytics::messaging::put_todays_market_collection_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_todays_market_collections_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_todays_market_collections_request& v);
    static ores::analytics::messaging::put_many_todays_market_collections_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_todays_market_collections_request& v);
    static ores::analytics::messaging::put_many_todays_market_collections_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_todays_market_collections_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_todays_market_collections_response& v);
    static ores::analytics::messaging::put_many_todays_market_collections_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_todays_market_collections_response& v);
    static ores::analytics::messaging::put_many_todays_market_collections_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_todays_market_collection_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_todays_market_collection_request& v);
    static ores::analytics::messaging::delete_todays_market_collection_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_todays_market_collection_request& v);
    static ores::analytics::messaging::delete_todays_market_collection_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_todays_market_collection_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_todays_market_collection_response& v);
    static ores::analytics::messaging::delete_todays_market_collection_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_todays_market_collection_response& v);
    static ores::analytics::messaging::delete_todays_market_collection_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_todays_market_collections_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_todays_market_collections_request& v);
    static ores::analytics::messaging::delete_many_todays_market_collections_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::delete_many_todays_market_collections_request& v);
    static ores::analytics::messaging::delete_many_todays_market_collections_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_todays_market_collections_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_todays_market_collections_response& v);
    static ores::analytics::messaging::delete_many_todays_market_collections_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::delete_many_todays_market_collections_response& v);
    static ores::analytics::messaging::delete_many_todays_market_collections_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<
    ores::analytics::messaging::list_by_todays_market_config_id_todays_market_collections_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::
                   list_by_todays_market_config_id_todays_market_collections_request& v);
    static ores::analytics::messaging::
        list_by_todays_market_config_id_todays_market_collections_request
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::
                      list_by_todays_market_config_id_todays_market_collections_request& v);
    static ores::analytics::messaging::
        list_by_todays_market_config_id_todays_market_collections_request
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::
                    list_by_todays_market_config_id_todays_market_collections_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::
                   list_by_todays_market_config_id_todays_market_collections_response& v);
    static ores::analytics::messaging::
        list_by_todays_market_config_id_todays_market_collections_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::
                      list_by_todays_market_config_id_todays_market_collections_response& v);
    static ores::analytics::messaging::
        list_by_todays_market_config_id_todays_market_collections_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_todays_market_collection_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_todays_market_collection_versions_request& v);
    static ores::analytics::messaging::list_todays_market_collection_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::list_todays_market_collection_versions_request& v);
    static ores::analytics::messaging::list_todays_market_collection_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_todays_market_collection_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::analytics::messaging::list_todays_market_collection_versions_response& v);
    static ores::analytics::messaging::list_todays_market_collection_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::list_todays_market_collection_versions_response& v);
    static ores::analytics::messaging::list_todays_market_collection_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_collection_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_todays_market_collection_version_request& v);
    static ores::analytics::messaging::get_todays_market_collection_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::get_todays_market_collection_version_request& v);
    static ores::analytics::messaging::get_todays_market_collection_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_collection_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_todays_market_collection_version_response& v);
    static ores::analytics::messaging::get_todays_market_collection_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::get_todays_market_collection_version_response& v);
    static ores::analytics::messaging::get_todays_market_collection_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
