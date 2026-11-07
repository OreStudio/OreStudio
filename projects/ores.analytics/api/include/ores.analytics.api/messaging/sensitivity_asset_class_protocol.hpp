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
#ifndef ORES_ANALYTICS_API_MESSAGING_SENSITIVITY_ASSET_CLASS_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_SENSITIVITY_ASSET_CLASS_PROTOCOL_HPP

#include "ores.analytics.api/domain/sensitivity_asset_class_codec.hpp"
#include "ores.analytics.api/export.hpp"
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

namespace ores::analytics::messaging {

struct sensitivity_asset_class_key {
    std::string asset_class_code;

    friend bool operator==(const sensitivity_asset_class_key&,
                           const sensitivity_asset_class_key&) = default;
};

struct sensitivity_asset_class_write {
    std::string asset_class_code;
    std::string template_prefix;
    std::string name;
    std::string description;
    int display_order;

    friend bool operator==(const sensitivity_asset_class_write&,
                           const sensitivity_asset_class_write&) = default;
};

struct sensitivity_asset_class_change {
    sensitivity_asset_class_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const sensitivity_asset_class_change&,
                           const sensitivity_asset_class_change&) = default;
};

struct sensitivity_asset_class_removal {
    sensitivity_asset_class_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const sensitivity_asset_class_removal&,
                           const sensitivity_asset_class_removal&) = default;
};

struct sensitivity_asset_class_lookup {
    sensitivity_asset_class_key key;
    std::optional<ores::analytics::domain::sensitivity_asset_class> sensitivity_asset_class;

    friend bool operator==(const sensitivity_asset_class_lookup&,
                           const sensitivity_asset_class_lookup&) = default;
};

struct sensitivity_asset_classes_filter {
    std::optional<std::vector<std::string>> asset_class_code_one_of;

    friend bool operator==(const sensitivity_asset_classes_filter&,
                           const sensitivity_asset_classes_filter&) = default;
};

struct sensitivity_asset_class_event {
    boost::uuids::uuid event_id;
    sensitivity_asset_class_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const sensitivity_asset_class_event&,
                           const sensitivity_asset_class_event&) = default;
};

struct sensitivity_asset_class_version_key {
    sensitivity_asset_class_key sensitivity_asset_class;
    std::uint32_t version;

    friend bool operator==(const sensitivity_asset_class_version_key&,
                           const sensitivity_asset_class_version_key&) = default;
};

struct sensitivity_asset_class_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const sensitivity_asset_class_versions_filter&,
                           const sensitivity_asset_class_versions_filter&) = default;
};

struct list_sensitivity_asset_classes_request {
    using response_type = struct list_sensitivity_asset_classes_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_asset_classes.list";
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
    std::optional<sensitivity_asset_classes_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_sensitivity_asset_classes_request&,
                           const list_sensitivity_asset_classes_request&) = default;
};

struct list_sensitivity_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::sensitivity_asset_class> asset_classes;
    std::uint64_t total;

    friend bool operator==(const list_sensitivity_asset_classes_response&,
                           const list_sensitivity_asset_classes_response&) = default;
};

struct get_sensitivity_asset_class_request {
    using response_type = struct get_sensitivity_asset_class_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_asset_classes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_asset_class_key key;

    friend bool operator==(const get_sensitivity_asset_class_request&,
                           const get_sensitivity_asset_class_request&) = default;
};

struct get_sensitivity_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::sensitivity_asset_class> sensitivity_asset_class;

    friend bool operator==(const get_sensitivity_asset_class_response&,
                           const get_sensitivity_asset_class_response&) = default;
};

struct get_many_sensitivity_asset_classes_request {
    using response_type = struct get_many_sensitivity_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.sensitivity_asset_classes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sensitivity_asset_class_key> keys;

    friend bool operator==(const get_many_sensitivity_asset_classes_request&,
                           const get_many_sensitivity_asset_classes_request&) = default;
};

struct get_many_sensitivity_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<sensitivity_asset_class_lookup> entries;

    friend bool operator==(const get_many_sensitivity_asset_classes_response&,
                           const get_many_sensitivity_asset_classes_response&) = default;
};

struct put_sensitivity_asset_class_request {
    using response_type = struct put_sensitivity_asset_class_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_asset_classes.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_asset_class_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_sensitivity_asset_class_request&,
                           const put_sensitivity_asset_class_request&) = default;
};

struct put_sensitivity_asset_class_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::sensitivity_asset_class> sensitivity_asset_class;

    friend bool operator==(const put_sensitivity_asset_class_response&,
                           const put_sensitivity_asset_class_response&) = default;
};

struct put_many_sensitivity_asset_classes_request {
    using response_type = struct put_many_sensitivity_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.sensitivity_asset_classes.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sensitivity_asset_class_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_sensitivity_asset_classes_request&,
                           const put_many_sensitivity_asset_classes_request&) = default;
};

struct put_many_sensitivity_asset_classes_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::sensitivity_asset_class> asset_classes;

    friend bool operator==(const put_many_sensitivity_asset_classes_response&,
                           const put_many_sensitivity_asset_classes_response&) = default;
};

struct delete_sensitivity_asset_class_request {
    using response_type = struct delete_sensitivity_asset_class_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.sensitivity_asset_classes.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_asset_class_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_sensitivity_asset_class_request&,
                           const delete_sensitivity_asset_class_request&) = default;
};

struct delete_sensitivity_asset_class_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_sensitivity_asset_class_response&,
                           const delete_sensitivity_asset_class_response&) = default;
};

struct delete_many_sensitivity_asset_classes_request {
    using response_type = struct delete_many_sensitivity_asset_classes_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.sensitivity_asset_classes.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sensitivity_asset_class_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_sensitivity_asset_classes_request&,
                           const delete_many_sensitivity_asset_classes_request&) = default;
};

struct delete_many_sensitivity_asset_classes_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_sensitivity_asset_classes_response&,
                           const delete_many_sensitivity_asset_classes_response&) = default;
};

struct list_sensitivity_asset_class_versions_request {
    using response_type = struct list_sensitivity_asset_class_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.sensitivity_asset_classes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_asset_class_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sensitivity_asset_class_versions_filter> filter;

    friend bool operator==(const list_sensitivity_asset_class_versions_request&,
                           const list_sensitivity_asset_class_versions_request&) = default;
};

struct list_sensitivity_asset_class_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::sensitivity_asset_class> versions;
    std::uint64_t total;

    friend bool operator==(const list_sensitivity_asset_class_versions_response&,
                           const list_sensitivity_asset_class_versions_response&) = default;
};

struct get_sensitivity_asset_class_version_request {
    using response_type = struct get_sensitivity_asset_class_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.sensitivity_asset_classes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_asset_class_version_key key;

    friend bool operator==(const get_sensitivity_asset_class_version_request&,
                           const get_sensitivity_asset_class_version_request&) = default;
};

struct get_sensitivity_asset_class_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::sensitivity_asset_class> version;

    friend bool operator==(const get_sensitivity_asset_class_version_response&,
                           const get_sensitivity_asset_class_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace sensitivity_asset_class_event_subjects {
inline constexpr std::string_view created = "analytics.v1.events.sensitivity_asset_classes.created";
inline constexpr std::string_view updated = "analytics.v1.events.sensitivity_asset_classes.updated";
inline constexpr std::string_view deleted = "analytics.v1.events.sensitivity_asset_classes.deleted";
}
/**
 * @brief The members of sensitivity_asset_class_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_asset_class_key>) {
    return std::tuple{
        utility::codec::field{"asset_class_code", &sensitivity_asset_class_key::asset_class_code},
    };
}

/**
 * @brief The members of sensitivity_asset_class_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_asset_class_write>) {
    return std::tuple{
        utility::codec::field{"asset_class_code", &sensitivity_asset_class_write::asset_class_code},
        utility::codec::field{"template_prefix", &sensitivity_asset_class_write::template_prefix},
        utility::codec::field{"name", &sensitivity_asset_class_write::name},
        utility::codec::field{"description", &sensitivity_asset_class_write::description},
        utility::codec::field{"display_order", &sensitivity_asset_class_write::display_order},
    };
}

/**
 * @brief The members of sensitivity_asset_class_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_asset_class_change>) {
    return std::tuple{
        utility::codec::field{"write", &sensitivity_asset_class_change::write},
        utility::codec::field{"precondition", &sensitivity_asset_class_change::precondition},
    };
}

/**
 * @brief The members of sensitivity_asset_class_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_asset_class_removal>) {
    return std::tuple{
        utility::codec::field{"key", &sensitivity_asset_class_removal::key},
        utility::codec::field{"precondition", &sensitivity_asset_class_removal::precondition},
    };
}

/**
 * @brief The members of sensitivity_asset_class_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_asset_class_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &sensitivity_asset_class_lookup::key},
        utility::codec::field{"sensitivity_asset_class",
                              &sensitivity_asset_class_lookup::sensitivity_asset_class},
    };
}

/**
 * @brief The members of sensitivity_asset_classes_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_asset_classes_filter>) {
    return std::tuple{
        utility::codec::field{"asset_class_code_one_of",
                              &sensitivity_asset_classes_filter::asset_class_code_one_of},
    };
}

/**
 * @brief The members of sensitivity_asset_class_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_asset_class_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &sensitivity_asset_class_event::event_id},
        utility::codec::field{"key", &sensitivity_asset_class_event::key},
        utility::codec::field{"action", &sensitivity_asset_class_event::action},
        utility::codec::field{"version", &sensitivity_asset_class_event::version},
        utility::codec::field{"occurred_at", &sensitivity_asset_class_event::occurred_at},
        utility::codec::field{"correlation_id", &sensitivity_asset_class_event::correlation_id},
    };
}

/**
 * @brief The members of sensitivity_asset_class_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_asset_class_version_key>) {
    return std::tuple{
        utility::codec::field{"sensitivity_asset_class",
                              &sensitivity_asset_class_version_key::sensitivity_asset_class},
        utility::codec::field{"version", &sensitivity_asset_class_version_key::version},
    };
}

/**
 * @brief The members of sensitivity_asset_class_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<sensitivity_asset_class_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &sensitivity_asset_class_versions_filter::version},
        utility::codec::field{"from_version",
                              &sensitivity_asset_class_versions_filter::from_version},
        utility::codec::field{"to_version", &sensitivity_asset_class_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_sensitivity_asset_classes_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_sensitivity_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_sensitivity_asset_classes_request::offset},
        utility::codec::field{"limit", &list_sensitivity_asset_classes_request::limit},
        utility::codec::field{"order", &list_sensitivity_asset_classes_request::order},
        utility::codec::field{"filter", &list_sensitivity_asset_classes_request::filter},
        utility::codec::field{"as_of", &list_sensitivity_asset_classes_request::as_of},
    };
}

/**
 * @brief The members of list_sensitivity_asset_classes_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_sensitivity_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_sensitivity_asset_classes_response::result},
        utility::codec::field{"asset_classes",
                              &list_sensitivity_asset_classes_response::asset_classes},
        utility::codec::field{"total", &list_sensitivity_asset_classes_response::total},
    };
}

/**
 * @brief The members of get_sensitivity_asset_class_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_sensitivity_asset_class_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_sensitivity_asset_class_request::key},
    };
}

/**
 * @brief The members of get_sensitivity_asset_class_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_sensitivity_asset_class_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_sensitivity_asset_class_response::result},
        utility::codec::field{"sensitivity_asset_class",
                              &get_sensitivity_asset_class_response::sensitivity_asset_class},
    };
}

/**
 * @brief The members of get_many_sensitivity_asset_classes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_sensitivity_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_sensitivity_asset_classes_request::keys},
    };
}

/**
 * @brief The members of get_many_sensitivity_asset_classes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_sensitivity_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_sensitivity_asset_classes_response::result},
        utility::codec::field{"entries", &get_many_sensitivity_asset_classes_response::entries},
    };
}

/**
 * @brief The members of put_sensitivity_asset_class_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_sensitivity_asset_class_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_sensitivity_asset_class_request::change},
        utility::codec::field{"intent", &put_sensitivity_asset_class_request::intent},
    };
}

/**
 * @brief The members of put_sensitivity_asset_class_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_sensitivity_asset_class_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_sensitivity_asset_class_response::result},
        utility::codec::field{"sensitivity_asset_class",
                              &put_sensitivity_asset_class_response::sensitivity_asset_class},
    };
}

/**
 * @brief The members of put_many_sensitivity_asset_classes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_sensitivity_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_sensitivity_asset_classes_request::changes},
        utility::codec::field{"intent", &put_many_sensitivity_asset_classes_request::intent},
    };
}

/**
 * @brief The members of put_many_sensitivity_asset_classes_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_sensitivity_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_sensitivity_asset_classes_response::result},
        utility::codec::field{"asset_classes",
                              &put_many_sensitivity_asset_classes_response::asset_classes},
    };
}

/**
 * @brief The members of delete_sensitivity_asset_class_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_sensitivity_asset_class_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_sensitivity_asset_class_request::removal},
        utility::codec::field{"intent", &delete_sensitivity_asset_class_request::intent},
    };
}

/**
 * @brief The members of delete_sensitivity_asset_class_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_sensitivity_asset_class_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_sensitivity_asset_class_response::result},
    };
}

/**
 * @brief The members of delete_many_sensitivity_asset_classes_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_sensitivity_asset_classes_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_sensitivity_asset_classes_request::removals},
        utility::codec::field{"intent", &delete_many_sensitivity_asset_classes_request::intent},
    };
}

/**
 * @brief The members of delete_many_sensitivity_asset_classes_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_sensitivity_asset_classes_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_sensitivity_asset_classes_response::result},
    };
}

/**
 * @brief The members of list_sensitivity_asset_class_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_sensitivity_asset_class_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_sensitivity_asset_class_versions_request::key},
        utility::codec::field{"offset", &list_sensitivity_asset_class_versions_request::offset},
        utility::codec::field{"limit", &list_sensitivity_asset_class_versions_request::limit},
        utility::codec::field{"order", &list_sensitivity_asset_class_versions_request::order},
        utility::codec::field{"filter", &list_sensitivity_asset_class_versions_request::filter},
    };
}

/**
 * @brief The members of list_sensitivity_asset_class_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_sensitivity_asset_class_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_sensitivity_asset_class_versions_response::result},
        utility::codec::field{"versions",
                              &list_sensitivity_asset_class_versions_response::versions},
        utility::codec::field{"total", &list_sensitivity_asset_class_versions_response::total},
    };
}

/**
 * @brief The members of get_sensitivity_asset_class_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_sensitivity_asset_class_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_sensitivity_asset_class_version_request::key},
    };
}

/**
 * @brief The members of get_sensitivity_asset_class_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_sensitivity_asset_class_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_sensitivity_asset_class_version_response::result},
        utility::codec::field{"version", &get_sensitivity_asset_class_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_class_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::sensitivity_asset_class_key& v);
    static ores::analytics::messaging::sensitivity_asset_class_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_class_key& v);
    static ores::analytics::messaging::sensitivity_asset_class_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_class_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_asset_class_write& v);
    static ores::analytics::messaging::sensitivity_asset_class_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_class_write& v);
    static ores::analytics::messaging::sensitivity_asset_class_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_class_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_asset_class_change& v);
    static ores::analytics::messaging::sensitivity_asset_class_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_class_change& v);
    static ores::analytics::messaging::sensitivity_asset_class_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_class_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_asset_class_removal& v);
    static ores::analytics::messaging::sensitivity_asset_class_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_class_removal& v);
    static ores::analytics::messaging::sensitivity_asset_class_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_class_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_asset_class_lookup& v);
    static ores::analytics::messaging::sensitivity_asset_class_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_class_lookup& v);
    static ores::analytics::messaging::sensitivity_asset_class_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_classes_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_asset_classes_filter& v);
    static ores::analytics::messaging::sensitivity_asset_classes_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_classes_filter& v);
    static ores::analytics::messaging::sensitivity_asset_classes_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_class_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_asset_class_event& v);
    static ores::analytics::messaging::sensitivity_asset_class_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_class_event& v);
    static ores::analytics::messaging::sensitivity_asset_class_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_class_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_asset_class_version_key& v);
    static ores::analytics::messaging::sensitivity_asset_class_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_class_version_key& v);
    static ores::analytics::messaging::sensitivity_asset_class_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_asset_class_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_asset_class_versions_filter& v);
    static ores::analytics::messaging::sensitivity_asset_class_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_asset_class_versions_filter& v);
    static ores::analytics::messaging::sensitivity_asset_class_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_sensitivity_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_sensitivity_asset_classes_request& v);
    static ores::analytics::messaging::list_sensitivity_asset_classes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_sensitivity_asset_classes_request& v);
    static ores::analytics::messaging::list_sensitivity_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_sensitivity_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_sensitivity_asset_classes_response& v);
    static ores::analytics::messaging::list_sensitivity_asset_classes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_sensitivity_asset_classes_response& v);
    static ores::analytics::messaging::list_sensitivity_asset_classes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_sensitivity_asset_class_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_sensitivity_asset_class_request& v);
    static ores::analytics::messaging::get_sensitivity_asset_class_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_sensitivity_asset_class_request& v);
    static ores::analytics::messaging::get_sensitivity_asset_class_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_sensitivity_asset_class_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_sensitivity_asset_class_response& v);
    static ores::analytics::messaging::get_sensitivity_asset_class_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_sensitivity_asset_class_response& v);
    static ores::analytics::messaging::get_sensitivity_asset_class_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_sensitivity_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_sensitivity_asset_classes_request& v);
    static ores::analytics::messaging::get_many_sensitivity_asset_classes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_sensitivity_asset_classes_request& v);
    static ores::analytics::messaging::get_many_sensitivity_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_sensitivity_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_sensitivity_asset_classes_response& v);
    static ores::analytics::messaging::get_many_sensitivity_asset_classes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_sensitivity_asset_classes_response& v);
    static ores::analytics::messaging::get_many_sensitivity_asset_classes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_sensitivity_asset_class_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_sensitivity_asset_class_request& v);
    static ores::analytics::messaging::put_sensitivity_asset_class_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_sensitivity_asset_class_request& v);
    static ores::analytics::messaging::put_sensitivity_asset_class_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_sensitivity_asset_class_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_sensitivity_asset_class_response& v);
    static ores::analytics::messaging::put_sensitivity_asset_class_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_sensitivity_asset_class_response& v);
    static ores::analytics::messaging::put_sensitivity_asset_class_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_sensitivity_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_sensitivity_asset_classes_request& v);
    static ores::analytics::messaging::put_many_sensitivity_asset_classes_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_sensitivity_asset_classes_request& v);
    static ores::analytics::messaging::put_many_sensitivity_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_sensitivity_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_sensitivity_asset_classes_response& v);
    static ores::analytics::messaging::put_many_sensitivity_asset_classes_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_sensitivity_asset_classes_response& v);
    static ores::analytics::messaging::put_many_sensitivity_asset_classes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_sensitivity_asset_class_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_sensitivity_asset_class_request& v);
    static ores::analytics::messaging::delete_sensitivity_asset_class_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_sensitivity_asset_class_request& v);
    static ores::analytics::messaging::delete_sensitivity_asset_class_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_sensitivity_asset_class_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_sensitivity_asset_class_response& v);
    static ores::analytics::messaging::delete_sensitivity_asset_class_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_sensitivity_asset_class_response& v);
    static ores::analytics::messaging::delete_sensitivity_asset_class_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_sensitivity_asset_classes_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_sensitivity_asset_classes_request& v);
    static ores::analytics::messaging::delete_many_sensitivity_asset_classes_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::delete_many_sensitivity_asset_classes_request& v);
    static ores::analytics::messaging::delete_many_sensitivity_asset_classes_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_sensitivity_asset_classes_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_sensitivity_asset_classes_response& v);
    static ores::analytics::messaging::delete_many_sensitivity_asset_classes_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::delete_many_sensitivity_asset_classes_response& v);
    static ores::analytics::messaging::delete_many_sensitivity_asset_classes_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_sensitivity_asset_class_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_sensitivity_asset_class_versions_request& v);
    static ores::analytics::messaging::list_sensitivity_asset_class_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::list_sensitivity_asset_class_versions_request& v);
    static ores::analytics::messaging::list_sensitivity_asset_class_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_sensitivity_asset_class_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_sensitivity_asset_class_versions_response& v);
    static ores::analytics::messaging::list_sensitivity_asset_class_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::list_sensitivity_asset_class_versions_response& v);
    static ores::analytics::messaging::list_sensitivity_asset_class_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_sensitivity_asset_class_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_sensitivity_asset_class_version_request& v);
    static ores::analytics::messaging::get_sensitivity_asset_class_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_sensitivity_asset_class_version_request& v);
    static ores::analytics::messaging::get_sensitivity_asset_class_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_sensitivity_asset_class_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_sensitivity_asset_class_version_response& v);
    static ores::analytics::messaging::get_sensitivity_asset_class_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::analytics::messaging::get_sensitivity_asset_class_version_response& v);
    static ores::analytics::messaging::get_sensitivity_asset_class_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
