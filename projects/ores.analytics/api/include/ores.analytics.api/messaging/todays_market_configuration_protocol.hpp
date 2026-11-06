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
#ifndef ORES_ANALYTICS_API_MESSAGING_TODAYS_MARKET_CONFIGURATION_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_TODAYS_MARKET_CONFIGURATION_PROTOCOL_HPP

#include "ores.analytics.api/domain/todays_market_configuration.hpp"
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

struct todays_market_configuration_key {
    std::string configuration_id;

    friend bool operator==(const todays_market_configuration_key&,
                           const todays_market_configuration_key&) = default;
};

struct todays_market_configuration_write {
    boost::uuids::uuid id;
    boost::uuids::uuid todays_market_config_id;
    std::string configuration_id;
    int position;

    friend bool operator==(const todays_market_configuration_write&,
                           const todays_market_configuration_write&) = default;
};

struct todays_market_configuration_change {
    todays_market_configuration_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const todays_market_configuration_change&,
                           const todays_market_configuration_change&) = default;
};

struct todays_market_configuration_removal {
    todays_market_configuration_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const todays_market_configuration_removal&,
                           const todays_market_configuration_removal&) = default;
};

struct todays_market_configuration_lookup {
    todays_market_configuration_key key;
    std::optional<ores::analytics::domain::todays_market_configuration> todays_market_configuration;

    friend bool operator==(const todays_market_configuration_lookup&,
                           const todays_market_configuration_lookup&) = default;
};

struct todays_market_configurations_filter {
    std::optional<boost::uuids::uuid> todays_market_config_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> todays_market_config_id_one_of;

    friend bool operator==(const todays_market_configurations_filter&,
                           const todays_market_configurations_filter&) = default;
};

struct todays_market_configuration_event {
    boost::uuids::uuid event_id;
    todays_market_configuration_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const todays_market_configuration_event&,
                           const todays_market_configuration_event&) = default;
};

struct todays_market_configuration_version_key {
    todays_market_configuration_key todays_market_configuration;
    std::uint32_t version;

    friend bool operator==(const todays_market_configuration_version_key&,
                           const todays_market_configuration_version_key&) = default;
};

struct todays_market_configuration_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const todays_market_configuration_versions_filter&,
                           const todays_market_configuration_versions_filter&) = default;
};

struct list_todays_market_configurations_request {
    using response_type = struct list_todays_market_configurations_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations.list";
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
    std::optional<todays_market_configurations_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_todays_market_configurations_request&,
                           const list_todays_market_configurations_request&) = default;
};

struct list_todays_market_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_configuration> configurations;
    std::uint64_t total;

    friend bool operator==(const list_todays_market_configurations_response&,
                           const list_todays_market_configurations_response&) = default;
};

struct get_todays_market_configuration_request {
    using response_type = struct get_todays_market_configuration_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_configuration_key key;

    friend bool operator==(const get_todays_market_configuration_request&,
                           const get_todays_market_configuration_request&) = default;
};

struct get_todays_market_configuration_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_configuration> todays_market_configuration;

    friend bool operator==(const get_todays_market_configuration_response&,
                           const get_todays_market_configuration_response&) = default;
};

struct get_many_todays_market_configurations_request {
    using response_type = struct get_many_todays_market_configurations_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_configuration_key> keys;

    friend bool operator==(const get_many_todays_market_configurations_request&,
                           const get_many_todays_market_configurations_request&) = default;
};

struct get_many_todays_market_configurations_response {
    ores::utility::domain::result result;
    std::vector<todays_market_configuration_lookup> entries;

    friend bool operator==(const get_many_todays_market_configurations_response&,
                           const get_many_todays_market_configurations_response&) = default;
};

struct put_todays_market_configuration_request {
    using response_type = struct put_todays_market_configuration_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_configuration_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_todays_market_configuration_request&,
                           const put_todays_market_configuration_request&) = default;
};

struct put_todays_market_configuration_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_configuration> todays_market_configuration;

    friend bool operator==(const put_todays_market_configuration_response&,
                           const put_todays_market_configuration_response&) = default;
};

struct put_many_todays_market_configurations_request {
    using response_type = struct put_many_todays_market_configurations_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_configuration_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_todays_market_configurations_request&,
                           const put_many_todays_market_configurations_request&) = default;
};

struct put_many_todays_market_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_configuration> configurations;

    friend bool operator==(const put_many_todays_market_configurations_response&,
                           const put_many_todays_market_configurations_response&) = default;
};

struct delete_todays_market_configuration_request {
    using response_type = struct delete_todays_market_configuration_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_configuration_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_todays_market_configuration_request&,
                           const delete_todays_market_configuration_request&) = default;
};

struct delete_todays_market_configuration_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_todays_market_configuration_response&,
                           const delete_todays_market_configuration_response&) = default;
};

struct delete_many_todays_market_configurations_request {
    using response_type = struct delete_many_todays_market_configurations_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<todays_market_configuration_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_todays_market_configurations_request&,
                           const delete_many_todays_market_configurations_request&) = default;
};

struct delete_many_todays_market_configurations_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_todays_market_configurations_response&,
                           const delete_many_todays_market_configurations_response&) = default;
};

struct list_by_todays_market_config_id_todays_market_configurations_request {
    using response_type =
        struct list_by_todays_market_config_id_todays_market_configurations_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations.list_by_todays_market_config_id";
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
    std::optional<todays_market_configurations_filter> filter;

    friend bool operator==(
        const list_by_todays_market_config_id_todays_market_configurations_request&,
        const list_by_todays_market_config_id_todays_market_configurations_request&) = default;
};

struct list_by_todays_market_config_id_todays_market_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_configuration> configurations;
    std::uint64_t total;

    friend bool operator==(
        const list_by_todays_market_config_id_todays_market_configurations_response&,
        const list_by_todays_market_config_id_todays_market_configurations_response&) = default;
};

struct list_todays_market_configuration_versions_request {
    using response_type = struct list_todays_market_configuration_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_configuration_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<todays_market_configuration_versions_filter> filter;

    friend bool operator==(const list_todays_market_configuration_versions_request&,
                           const list_todays_market_configuration_versions_request&) = default;
};

struct list_todays_market_configuration_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::todays_market_configuration> versions;
    std::uint64_t total;

    friend bool operator==(const list_todays_market_configuration_versions_response&,
                           const list_todays_market_configuration_versions_response&) = default;
};

struct get_todays_market_configuration_version_request {
    using response_type = struct get_todays_market_configuration_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.todays_market_configurations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    todays_market_configuration_version_key key;

    friend bool operator==(const get_todays_market_configuration_version_request&,
                           const get_todays_market_configuration_version_request&) = default;
};

struct get_todays_market_configuration_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::todays_market_configuration> version;

    friend bool operator==(const get_todays_market_configuration_version_response&,
                           const get_todays_market_configuration_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace todays_market_configuration_event_subjects {
inline constexpr std::string_view created =
    "analytics.v1.events.todays_market_configurations.created";
inline constexpr std::string_view updated =
    "analytics.v1.events.todays_market_configurations.updated";
inline constexpr std::string_view deleted =
    "analytics.v1.events.todays_market_configurations.deleted";
}
/**
 * @brief The members of todays_market_configuration_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_configuration_key>) {
    return std::tuple{
        utility::codec::field{"configuration_id",
                              &todays_market_configuration_key::configuration_id},
    };
}

/**
 * @brief The members of todays_market_configuration_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_configuration_write>) {
    return std::tuple{
        utility::codec::field{"id", &todays_market_configuration_write::id},
        utility::codec::field{"todays_market_config_id",
                              &todays_market_configuration_write::todays_market_config_id},
        utility::codec::field{"configuration_id",
                              &todays_market_configuration_write::configuration_id},
        utility::codec::field{"position", &todays_market_configuration_write::position},
    };
}

/**
 * @brief The members of todays_market_configuration_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_configuration_change>) {
    return std::tuple{
        utility::codec::field{"write", &todays_market_configuration_change::write},
        utility::codec::field{"precondition", &todays_market_configuration_change::precondition},
    };
}

/**
 * @brief The members of todays_market_configuration_removal, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_configuration_removal>) {
    return std::tuple{
        utility::codec::field{"key", &todays_market_configuration_removal::key},
        utility::codec::field{"precondition", &todays_market_configuration_removal::precondition},
    };
}

/**
 * @brief The members of todays_market_configuration_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_configuration_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &todays_market_configuration_lookup::key},
        utility::codec::field{"todays_market_configuration",
                              &todays_market_configuration_lookup::todays_market_configuration},
    };
}

/**
 * @brief The members of todays_market_configurations_filter, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_configurations_filter>) {
    return std::tuple{
        utility::codec::field{"todays_market_config_id",
                              &todays_market_configurations_filter::todays_market_config_id},
        utility::codec::field{"id_one_of", &todays_market_configurations_filter::id_one_of},
        utility::codec::field{"todays_market_config_id_one_of",
                              &todays_market_configurations_filter::todays_market_config_id_one_of},
    };
}

/**
 * @brief The members of todays_market_configuration_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<todays_market_configuration_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &todays_market_configuration_event::event_id},
        utility::codec::field{"key", &todays_market_configuration_event::key},
        utility::codec::field{"action", &todays_market_configuration_event::action},
        utility::codec::field{"version", &todays_market_configuration_event::version},
        utility::codec::field{"occurred_at", &todays_market_configuration_event::occurred_at},
        utility::codec::field{"correlation_id", &todays_market_configuration_event::correlation_id},
    };
}

/**
 * @brief The members of todays_market_configuration_version_key, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<todays_market_configuration_version_key>) {
    return std::tuple{
        utility::codec::field{
            "todays_market_configuration",
            &todays_market_configuration_version_key::todays_market_configuration},
        utility::codec::field{"version", &todays_market_configuration_version_key::version},
    };
}

/**
 * @brief The members of todays_market_configuration_versions_filter, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<todays_market_configuration_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &todays_market_configuration_versions_filter::version},
        utility::codec::field{"from_version",
                              &todays_market_configuration_versions_filter::from_version},
        utility::codec::field{"to_version",
                              &todays_market_configuration_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_todays_market_configurations_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_todays_market_configurations_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_todays_market_configurations_request::offset},
        utility::codec::field{"limit", &list_todays_market_configurations_request::limit},
        utility::codec::field{"order", &list_todays_market_configurations_request::order},
        utility::codec::field{"filter", &list_todays_market_configurations_request::filter},
        utility::codec::field{"as_of", &list_todays_market_configurations_request::as_of},
    };
}

/**
 * @brief The members of list_todays_market_configurations_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_todays_market_configurations_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_todays_market_configurations_response::result},
        utility::codec::field{"configurations",
                              &list_todays_market_configurations_response::configurations},
        utility::codec::field{"total", &list_todays_market_configurations_response::total},
    };
}

/**
 * @brief The members of get_todays_market_configuration_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_todays_market_configuration_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_todays_market_configuration_request::key},
    };
}

/**
 * @brief The members of get_todays_market_configuration_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_todays_market_configuration_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_todays_market_configuration_response::result},
        utility::codec::field{
            "todays_market_configuration",
            &get_todays_market_configuration_response::todays_market_configuration},
    };
}

/**
 * @brief The members of get_many_todays_market_configurations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_todays_market_configurations_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_todays_market_configurations_request::keys},
    };
}

/**
 * @brief The members of get_many_todays_market_configurations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_todays_market_configurations_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_todays_market_configurations_response::result},
        utility::codec::field{"entries", &get_many_todays_market_configurations_response::entries},
    };
}

/**
 * @brief The members of put_todays_market_configuration_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_todays_market_configuration_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_todays_market_configuration_request::change},
        utility::codec::field{"intent", &put_todays_market_configuration_request::intent},
    };
}

/**
 * @brief The members of put_todays_market_configuration_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_todays_market_configuration_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_todays_market_configuration_response::result},
        utility::codec::field{
            "todays_market_configuration",
            &put_todays_market_configuration_response::todays_market_configuration},
    };
}

/**
 * @brief The members of put_many_todays_market_configurations_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_todays_market_configurations_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_todays_market_configurations_request::changes},
        utility::codec::field{"intent", &put_many_todays_market_configurations_request::intent},
    };
}

/**
 * @brief The members of put_many_todays_market_configurations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_todays_market_configurations_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_todays_market_configurations_response::result},
        utility::codec::field{"configurations",
                              &put_many_todays_market_configurations_response::configurations},
    };
}

/**
 * @brief The members of delete_todays_market_configuration_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_todays_market_configuration_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_todays_market_configuration_request::removal},
        utility::codec::field{"intent", &delete_todays_market_configuration_request::intent},
    };
}

/**
 * @brief The members of delete_todays_market_configuration_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_todays_market_configuration_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_todays_market_configuration_response::result},
    };
}

/**
 * @brief The members of delete_many_todays_market_configurations_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_todays_market_configurations_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_todays_market_configurations_request::removals},
        utility::codec::field{"intent", &delete_many_todays_market_configurations_request::intent},
    };
}

/**
 * @brief The members of delete_many_todays_market_configurations_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_todays_market_configurations_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_todays_market_configurations_response::result},
    };
}

/**
 * @brief The members of list_by_todays_market_config_id_todays_market_configurations_request, named
 * as the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(
    std::type_identity<list_by_todays_market_config_id_todays_market_configurations_request>) {
    return std::tuple{
        utility::codec::field{
            "todays_market_config_id",
            &list_by_todays_market_config_id_todays_market_configurations_request::
                todays_market_config_id},
        utility::codec::field{
            "scope", &list_by_todays_market_config_id_todays_market_configurations_request::scope},
        utility::codec::field{
            "offset",
            &list_by_todays_market_config_id_todays_market_configurations_request::offset},
        utility::codec::field{
            "limit", &list_by_todays_market_config_id_todays_market_configurations_request::limit},
        utility::codec::field{
            "order", &list_by_todays_market_config_id_todays_market_configurations_request::order},
        utility::codec::field{
            "filter",
            &list_by_todays_market_config_id_todays_market_configurations_request::filter},
    };
}

/**
 * @brief The members of list_by_todays_market_config_id_todays_market_configurations_response,
 * named as the wire names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(
    std::type_identity<list_by_todays_market_config_id_todays_market_configurations_response>) {
    return std::tuple{
        utility::codec::field{
            "result",
            &list_by_todays_market_config_id_todays_market_configurations_response::result},
        utility::codec::field{
            "configurations",
            &list_by_todays_market_config_id_todays_market_configurations_response::configurations},
        utility::codec::field{
            "total", &list_by_todays_market_config_id_todays_market_configurations_response::total},
    };
}

/**
 * @brief The members of list_todays_market_configuration_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_todays_market_configuration_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_todays_market_configuration_versions_request::key},
        utility::codec::field{"offset", &list_todays_market_configuration_versions_request::offset},
        utility::codec::field{"limit", &list_todays_market_configuration_versions_request::limit},
        utility::codec::field{"order", &list_todays_market_configuration_versions_request::order},
        utility::codec::field{"filter", &list_todays_market_configuration_versions_request::filter},
    };
}

/**
 * @brief The members of list_todays_market_configuration_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_todays_market_configuration_versions_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_todays_market_configuration_versions_response::result},
        utility::codec::field{"versions",
                              &list_todays_market_configuration_versions_response::versions},
        utility::codec::field{"total", &list_todays_market_configuration_versions_response::total},
    };
}

/**
 * @brief The members of get_todays_market_configuration_version_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_todays_market_configuration_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_todays_market_configuration_version_request::key},
    };
}

/**
 * @brief The members of get_todays_market_configuration_version_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_todays_market_configuration_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_todays_market_configuration_version_response::result},
        utility::codec::field{"version",
                              &get_todays_market_configuration_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configuration_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configuration_key& v);
    static ores::analytics::messaging::todays_market_configuration_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configuration_key& v);
    static ores::analytics::messaging::todays_market_configuration_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configuration_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configuration_write& v);
    static ores::analytics::messaging::todays_market_configuration_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configuration_write& v);
    static ores::analytics::messaging::todays_market_configuration_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configuration_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configuration_change& v);
    static ores::analytics::messaging::todays_market_configuration_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configuration_change& v);
    static ores::analytics::messaging::todays_market_configuration_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configuration_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configuration_removal& v);
    static ores::analytics::messaging::todays_market_configuration_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configuration_removal& v);
    static ores::analytics::messaging::todays_market_configuration_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configuration_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configuration_lookup& v);
    static ores::analytics::messaging::todays_market_configuration_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configuration_lookup& v);
    static ores::analytics::messaging::todays_market_configuration_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configurations_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configurations_filter& v);
    static ores::analytics::messaging::todays_market_configurations_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configurations_filter& v);
    static ores::analytics::messaging::todays_market_configurations_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configuration_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configuration_event& v);
    static ores::analytics::messaging::todays_market_configuration_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configuration_event& v);
    static ores::analytics::messaging::todays_market_configuration_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configuration_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configuration_version_key& v);
    static ores::analytics::messaging::todays_market_configuration_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configuration_version_key& v);
    static ores::analytics::messaging::todays_market_configuration_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::todays_market_configuration_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::todays_market_configuration_versions_filter& v);
    static ores::analytics::messaging::todays_market_configuration_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::todays_market_configuration_versions_filter& v);
    static ores::analytics::messaging::todays_market_configuration_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_todays_market_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_todays_market_configurations_request& v);
    static ores::analytics::messaging::list_todays_market_configurations_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_todays_market_configurations_request& v);
    static ores::analytics::messaging::list_todays_market_configurations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_todays_market_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_todays_market_configurations_response& v);
    static ores::analytics::messaging::list_todays_market_configurations_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_todays_market_configurations_response& v);
    static ores::analytics::messaging::list_todays_market_configurations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_todays_market_configuration_request& v);
    static ores::analytics::messaging::get_todays_market_configuration_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_todays_market_configuration_request& v);
    static ores::analytics::messaging::get_todays_market_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_todays_market_configuration_response& v);
    static ores::analytics::messaging::get_todays_market_configuration_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_todays_market_configuration_response& v);
    static ores::analytics::messaging::get_todays_market_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_todays_market_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_todays_market_configurations_request& v);
    static ores::analytics::messaging::get_many_todays_market_configurations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::get_many_todays_market_configurations_request& v);
    static ores::analytics::messaging::get_many_todays_market_configurations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_todays_market_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_todays_market_configurations_response& v);
    static ores::analytics::messaging::get_many_todays_market_configurations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::get_many_todays_market_configurations_response& v);
    static ores::analytics::messaging::get_many_todays_market_configurations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_todays_market_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_todays_market_configuration_request& v);
    static ores::analytics::messaging::put_todays_market_configuration_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_todays_market_configuration_request& v);
    static ores::analytics::messaging::put_todays_market_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_todays_market_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_todays_market_configuration_response& v);
    static ores::analytics::messaging::put_todays_market_configuration_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_todays_market_configuration_response& v);
    static ores::analytics::messaging::put_todays_market_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_todays_market_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_todays_market_configurations_request& v);
    static ores::analytics::messaging::put_many_todays_market_configurations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::put_many_todays_market_configurations_request& v);
    static ores::analytics::messaging::put_many_todays_market_configurations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_todays_market_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_todays_market_configurations_response& v);
    static ores::analytics::messaging::put_many_todays_market_configurations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::put_many_todays_market_configurations_response& v);
    static ores::analytics::messaging::put_many_todays_market_configurations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_todays_market_configuration_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_todays_market_configuration_request& v);
    static ores::analytics::messaging::delete_todays_market_configuration_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_todays_market_configuration_request& v);
    static ores::analytics::messaging::delete_todays_market_configuration_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_todays_market_configuration_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_todays_market_configuration_response& v);
    static ores::analytics::messaging::delete_todays_market_configuration_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_todays_market_configuration_response& v);
    static ores::analytics::messaging::delete_todays_market_configuration_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_todays_market_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::analytics::messaging::delete_many_todays_market_configurations_request& v);
    static ores::analytics::messaging::delete_many_todays_market_configurations_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::delete_many_todays_market_configurations_request& v);
    static ores::analytics::messaging::delete_many_todays_market_configurations_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_todays_market_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::analytics::messaging::delete_many_todays_market_configurations_response& v);
    static ores::analytics::messaging::delete_many_todays_market_configurations_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::delete_many_todays_market_configurations_response& v);
    static ores::analytics::messaging::delete_many_todays_market_configurations_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::
                    list_by_todays_market_config_id_todays_market_configurations_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::
                   list_by_todays_market_config_id_todays_market_configurations_request& v);
    static ores::analytics::messaging::
        list_by_todays_market_config_id_todays_market_configurations_request
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::
                      list_by_todays_market_config_id_todays_market_configurations_request& v);
    static ores::analytics::messaging::
        list_by_todays_market_config_id_todays_market_configurations_request
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::
                    list_by_todays_market_config_id_todays_market_configurations_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::
                   list_by_todays_market_config_id_todays_market_configurations_response& v);
    static ores::analytics::messaging::
        list_by_todays_market_config_id_todays_market_configurations_response
        read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::
                      list_by_todays_market_config_id_todays_market_configurations_response& v);
    static ores::analytics::messaging::
        list_by_todays_market_config_id_todays_market_configurations_response
        read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_todays_market_configuration_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::analytics::messaging::list_todays_market_configuration_versions_request& v);
    static ores::analytics::messaging::list_todays_market_configuration_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::list_todays_market_configuration_versions_request& v);
    static ores::analytics::messaging::list_todays_market_configuration_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_todays_market_configuration_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::analytics::messaging::list_todays_market_configuration_versions_response& v);
    static ores::analytics::messaging::list_todays_market_configuration_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::list_todays_market_configuration_versions_response& v);
    static ores::analytics::messaging::list_todays_market_configuration_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_configuration_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::analytics::messaging::get_todays_market_configuration_version_request& v);
    static ores::analytics::messaging::get_todays_market_configuration_version_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::get_todays_market_configuration_version_request& v);
    static ores::analytics::messaging::get_todays_market_configuration_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_todays_market_configuration_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::analytics::messaging::get_todays_market_configuration_version_response& v);
    static ores::analytics::messaging::get_todays_market_configuration_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::get_todays_market_configuration_version_response& v);
    static ores::analytics::messaging::get_todays_market_configuration_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
