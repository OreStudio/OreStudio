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
#ifndef ORES_ANALYTICS_API_MESSAGING_SENSITIVITY_METHOD_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_SENSITIVITY_METHOD_PROTOCOL_HPP

#include "ores.analytics.api/domain/sensitivity_method.hpp"
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

struct sensitivity_method_key {
    std::string code;

    friend bool operator==(const sensitivity_method_key&, const sensitivity_method_key&) = default;
};

struct sensitivity_method_write {
    std::string code;
    std::string name;
    std::string description;
    bool needs_asset_class;
    int display_order;

    friend bool operator==(const sensitivity_method_write&,
                           const sensitivity_method_write&) = default;
};

struct sensitivity_method_change {
    sensitivity_method_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const sensitivity_method_change&,
                           const sensitivity_method_change&) = default;
};

struct sensitivity_method_removal {
    sensitivity_method_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const sensitivity_method_removal&,
                           const sensitivity_method_removal&) = default;
};

struct sensitivity_method_lookup {
    sensitivity_method_key key;
    std::optional<ores::analytics::domain::sensitivity_method> sensitivity_method;

    friend bool operator==(const sensitivity_method_lookup&,
                           const sensitivity_method_lookup&) = default;
};

struct sensitivity_methods_filter {
    std::optional<std::vector<std::string>> code_one_of;

    friend bool operator==(const sensitivity_methods_filter&,
                           const sensitivity_methods_filter&) = default;
};

struct sensitivity_method_event {
    boost::uuids::uuid event_id;
    sensitivity_method_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const sensitivity_method_event&,
                           const sensitivity_method_event&) = default;
};

struct sensitivity_method_version_key {
    sensitivity_method_key sensitivity_method;
    std::uint32_t version;

    friend bool operator==(const sensitivity_method_version_key&,
                           const sensitivity_method_version_key&) = default;
};

struct sensitivity_method_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const sensitivity_method_versions_filter&,
                           const sensitivity_method_versions_filter&) = default;
};

struct list_sensitivity_methods_request {
    using response_type = struct list_sensitivity_methods_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_methods.list";
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
    std::optional<sensitivity_methods_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_sensitivity_methods_request&,
                           const list_sensitivity_methods_request&) = default;
};

struct list_sensitivity_methods_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::sensitivity_method> methods;
    std::uint64_t total;

    friend bool operator==(const list_sensitivity_methods_response&,
                           const list_sensitivity_methods_response&) = default;
};

struct get_sensitivity_method_request {
    using response_type = struct get_sensitivity_method_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_methods.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_method_key key;

    friend bool operator==(const get_sensitivity_method_request&,
                           const get_sensitivity_method_request&) = default;
};

struct get_sensitivity_method_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::sensitivity_method> sensitivity_method;

    friend bool operator==(const get_sensitivity_method_response&,
                           const get_sensitivity_method_response&) = default;
};

struct get_many_sensitivity_methods_request {
    using response_type = struct get_many_sensitivity_methods_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_methods.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sensitivity_method_key> keys;

    friend bool operator==(const get_many_sensitivity_methods_request&,
                           const get_many_sensitivity_methods_request&) = default;
};

struct get_many_sensitivity_methods_response {
    ores::utility::domain::result result;
    std::vector<sensitivity_method_lookup> entries;

    friend bool operator==(const get_many_sensitivity_methods_response&,
                           const get_many_sensitivity_methods_response&) = default;
};

struct put_sensitivity_method_request {
    using response_type = struct put_sensitivity_method_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_methods.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_method_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_sensitivity_method_request&,
                           const put_sensitivity_method_request&) = default;
};

struct put_sensitivity_method_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::sensitivity_method> sensitivity_method;

    friend bool operator==(const put_sensitivity_method_response&,
                           const put_sensitivity_method_response&) = default;
};

struct put_many_sensitivity_methods_request {
    using response_type = struct put_many_sensitivity_methods_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_methods.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sensitivity_method_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_sensitivity_methods_request&,
                           const put_many_sensitivity_methods_request&) = default;
};

struct put_many_sensitivity_methods_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::sensitivity_method> methods;

    friend bool operator==(const put_many_sensitivity_methods_response&,
                           const put_many_sensitivity_methods_response&) = default;
};

struct delete_sensitivity_method_request {
    using response_type = struct delete_sensitivity_method_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_methods.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_method_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_sensitivity_method_request&,
                           const delete_sensitivity_method_request&) = default;
};

struct delete_sensitivity_method_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_sensitivity_method_response&,
                           const delete_sensitivity_method_response&) = default;
};

struct delete_many_sensitivity_methods_request {
    using response_type = struct delete_many_sensitivity_methods_response;
    static constexpr std::string_view nats_subject = "analytics.v1.sensitivity_methods.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<sensitivity_method_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_sensitivity_methods_request&,
                           const delete_many_sensitivity_methods_request&) = default;
};

struct delete_many_sensitivity_methods_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_sensitivity_methods_response&,
                           const delete_many_sensitivity_methods_response&) = default;
};

struct list_sensitivity_method_versions_request {
    using response_type = struct list_sensitivity_method_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.sensitivity_methods_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_method_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<sensitivity_method_versions_filter> filter;

    friend bool operator==(const list_sensitivity_method_versions_request&,
                           const list_sensitivity_method_versions_request&) = default;
};

struct list_sensitivity_method_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::sensitivity_method> versions;
    std::uint64_t total;

    friend bool operator==(const list_sensitivity_method_versions_response&,
                           const list_sensitivity_method_versions_response&) = default;
};

struct get_sensitivity_method_version_request {
    using response_type = struct get_sensitivity_method_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.sensitivity_methods_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    sensitivity_method_version_key key;

    friend bool operator==(const get_sensitivity_method_version_request&,
                           const get_sensitivity_method_version_request&) = default;
};

struct get_sensitivity_method_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::sensitivity_method> version;

    friend bool operator==(const get_sensitivity_method_version_response&,
                           const get_sensitivity_method_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace sensitivity_method_event_subjects {
inline constexpr std::string_view created = "analytics.v1.events.sensitivity_methods.created";
inline constexpr std::string_view updated = "analytics.v1.events.sensitivity_methods.updated";
inline constexpr std::string_view deleted = "analytics.v1.events.sensitivity_methods.deleted";
}
/**
 * @brief The members of sensitivity_method_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_method_key>) {
    return std::tuple{
        utility::codec::field{"code", &sensitivity_method_key::code},
    };
}

/**
 * @brief The members of sensitivity_method_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_method_write>) {
    return std::tuple{
        utility::codec::field{"code", &sensitivity_method_write::code},
        utility::codec::field{"name", &sensitivity_method_write::name},
        utility::codec::field{"description", &sensitivity_method_write::description},
        utility::codec::field{"needs_asset_class", &sensitivity_method_write::needs_asset_class},
        utility::codec::field{"display_order", &sensitivity_method_write::display_order},
    };
}

/**
 * @brief The members of sensitivity_method_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_method_change>) {
    return std::tuple{
        utility::codec::field{"write", &sensitivity_method_change::write},
        utility::codec::field{"precondition", &sensitivity_method_change::precondition},
    };
}

/**
 * @brief The members of sensitivity_method_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_method_removal>) {
    return std::tuple{
        utility::codec::field{"key", &sensitivity_method_removal::key},
        utility::codec::field{"precondition", &sensitivity_method_removal::precondition},
    };
}

/**
 * @brief The members of sensitivity_method_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_method_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &sensitivity_method_lookup::key},
        utility::codec::field{"sensitivity_method", &sensitivity_method_lookup::sensitivity_method},
    };
}

/**
 * @brief The members of sensitivity_methods_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_methods_filter>) {
    return std::tuple{
        utility::codec::field{"code_one_of", &sensitivity_methods_filter::code_one_of},
    };
}

/**
 * @brief The members of sensitivity_method_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_method_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &sensitivity_method_event::event_id},
        utility::codec::field{"key", &sensitivity_method_event::key},
        utility::codec::field{"action", &sensitivity_method_event::action},
        utility::codec::field{"version", &sensitivity_method_event::version},
        utility::codec::field{"occurred_at", &sensitivity_method_event::occurred_at},
        utility::codec::field{"correlation_id", &sensitivity_method_event::correlation_id},
    };
}

/**
 * @brief The members of sensitivity_method_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_method_version_key>) {
    return std::tuple{
        utility::codec::field{"sensitivity_method",
                              &sensitivity_method_version_key::sensitivity_method},
        utility::codec::field{"version", &sensitivity_method_version_key::version},
    };
}

/**
 * @brief The members of sensitivity_method_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<sensitivity_method_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &sensitivity_method_versions_filter::version},
        utility::codec::field{"from_version", &sensitivity_method_versions_filter::from_version},
        utility::codec::field{"to_version", &sensitivity_method_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_sensitivity_methods_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_sensitivity_methods_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_sensitivity_methods_request::offset},
        utility::codec::field{"limit", &list_sensitivity_methods_request::limit},
        utility::codec::field{"order", &list_sensitivity_methods_request::order},
        utility::codec::field{"filter", &list_sensitivity_methods_request::filter},
        utility::codec::field{"as_of", &list_sensitivity_methods_request::as_of},
    };
}

/**
 * @brief The members of list_sensitivity_methods_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_sensitivity_methods_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_sensitivity_methods_response::result},
        utility::codec::field{"methods", &list_sensitivity_methods_response::methods},
        utility::codec::field{"total", &list_sensitivity_methods_response::total},
    };
}

/**
 * @brief The members of get_sensitivity_method_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_sensitivity_method_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_sensitivity_method_request::key},
    };
}

/**
 * @brief The members of get_sensitivity_method_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_sensitivity_method_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_sensitivity_method_response::result},
        utility::codec::field{"sensitivity_method",
                              &get_sensitivity_method_response::sensitivity_method},
    };
}

/**
 * @brief The members of get_many_sensitivity_methods_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_sensitivity_methods_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_sensitivity_methods_request::keys},
    };
}

/**
 * @brief The members of get_many_sensitivity_methods_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_sensitivity_methods_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_sensitivity_methods_response::result},
        utility::codec::field{"entries", &get_many_sensitivity_methods_response::entries},
    };
}

/**
 * @brief The members of put_sensitivity_method_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_sensitivity_method_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_sensitivity_method_request::change},
        utility::codec::field{"intent", &put_sensitivity_method_request::intent},
    };
}

/**
 * @brief The members of put_sensitivity_method_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_sensitivity_method_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_sensitivity_method_response::result},
        utility::codec::field{"sensitivity_method",
                              &put_sensitivity_method_response::sensitivity_method},
    };
}

/**
 * @brief The members of put_many_sensitivity_methods_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_sensitivity_methods_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_sensitivity_methods_request::changes},
        utility::codec::field{"intent", &put_many_sensitivity_methods_request::intent},
    };
}

/**
 * @brief The members of put_many_sensitivity_methods_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_sensitivity_methods_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_sensitivity_methods_response::result},
        utility::codec::field{"methods", &put_many_sensitivity_methods_response::methods},
    };
}

/**
 * @brief The members of delete_sensitivity_method_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_sensitivity_method_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_sensitivity_method_request::removal},
        utility::codec::field{"intent", &delete_sensitivity_method_request::intent},
    };
}

/**
 * @brief The members of delete_sensitivity_method_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_sensitivity_method_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_sensitivity_method_response::result},
    };
}

/**
 * @brief The members of delete_many_sensitivity_methods_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_sensitivity_methods_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_sensitivity_methods_request::removals},
        utility::codec::field{"intent", &delete_many_sensitivity_methods_request::intent},
    };
}

/**
 * @brief The members of delete_many_sensitivity_methods_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_sensitivity_methods_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_sensitivity_methods_response::result},
    };
}

/**
 * @brief The members of list_sensitivity_method_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_sensitivity_method_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_sensitivity_method_versions_request::key},
        utility::codec::field{"offset", &list_sensitivity_method_versions_request::offset},
        utility::codec::field{"limit", &list_sensitivity_method_versions_request::limit},
        utility::codec::field{"order", &list_sensitivity_method_versions_request::order},
        utility::codec::field{"filter", &list_sensitivity_method_versions_request::filter},
    };
}

/**
 * @brief The members of list_sensitivity_method_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_sensitivity_method_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_sensitivity_method_versions_response::result},
        utility::codec::field{"versions", &list_sensitivity_method_versions_response::versions},
        utility::codec::field{"total", &list_sensitivity_method_versions_response::total},
    };
}

/**
 * @brief The members of get_sensitivity_method_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_sensitivity_method_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_sensitivity_method_version_request::key},
    };
}

/**
 * @brief The members of get_sensitivity_method_version_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_sensitivity_method_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_sensitivity_method_version_response::result},
        utility::codec::field{"version", &get_sensitivity_method_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::sensitivity_method_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::sensitivity_method_key& v);
    static ores::analytics::messaging::sensitivity_method_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::sensitivity_method_key& v);
    static ores::analytics::messaging::sensitivity_method_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::sensitivity_method_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::sensitivity_method_write& v);
    static ores::analytics::messaging::sensitivity_method_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::sensitivity_method_write& v);
    static ores::analytics::messaging::sensitivity_method_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_method_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::sensitivity_method_change& v);
    static ores::analytics::messaging::sensitivity_method_change read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_method_change& v);
    static ores::analytics::messaging::sensitivity_method_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_method_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::sensitivity_method_removal& v);
    static ores::analytics::messaging::sensitivity_method_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_method_removal& v);
    static ores::analytics::messaging::sensitivity_method_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_method_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::sensitivity_method_lookup& v);
    static ores::analytics::messaging::sensitivity_method_lookup read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_method_lookup& v);
    static ores::analytics::messaging::sensitivity_method_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_methods_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::sensitivity_methods_filter& v);
    static ores::analytics::messaging::sensitivity_methods_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_methods_filter& v);
    static ores::analytics::messaging::sensitivity_methods_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT wire_traits<ores::analytics::messaging::sensitivity_method_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::analytics::messaging::sensitivity_method_event& v);
    static ores::analytics::messaging::sensitivity_method_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::analytics::messaging::sensitivity_method_event& v);
    static ores::analytics::messaging::sensitivity_method_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_method_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_method_version_key& v);
    static ores::analytics::messaging::sensitivity_method_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_method_version_key& v);
    static ores::analytics::messaging::sensitivity_method_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::sensitivity_method_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::sensitivity_method_versions_filter& v);
    static ores::analytics::messaging::sensitivity_method_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::sensitivity_method_versions_filter& v);
    static ores::analytics::messaging::sensitivity_method_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_sensitivity_methods_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_sensitivity_methods_request& v);
    static ores::analytics::messaging::list_sensitivity_methods_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_sensitivity_methods_request& v);
    static ores::analytics::messaging::list_sensitivity_methods_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_sensitivity_methods_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_sensitivity_methods_response& v);
    static ores::analytics::messaging::list_sensitivity_methods_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_sensitivity_methods_response& v);
    static ores::analytics::messaging::list_sensitivity_methods_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_sensitivity_method_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_sensitivity_method_request& v);
    static ores::analytics::messaging::get_sensitivity_method_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_sensitivity_method_request& v);
    static ores::analytics::messaging::get_sensitivity_method_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_sensitivity_method_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_sensitivity_method_response& v);
    static ores::analytics::messaging::get_sensitivity_method_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_sensitivity_method_response& v);
    static ores::analytics::messaging::get_sensitivity_method_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_sensitivity_methods_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_sensitivity_methods_request& v);
    static ores::analytics::messaging::get_many_sensitivity_methods_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_sensitivity_methods_request& v);
    static ores::analytics::messaging::get_many_sensitivity_methods_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_many_sensitivity_methods_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_many_sensitivity_methods_response& v);
    static ores::analytics::messaging::get_many_sensitivity_methods_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_many_sensitivity_methods_response& v);
    static ores::analytics::messaging::get_many_sensitivity_methods_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_sensitivity_method_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_sensitivity_method_request& v);
    static ores::analytics::messaging::put_sensitivity_method_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_sensitivity_method_request& v);
    static ores::analytics::messaging::put_sensitivity_method_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_sensitivity_method_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_sensitivity_method_response& v);
    static ores::analytics::messaging::put_sensitivity_method_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_sensitivity_method_response& v);
    static ores::analytics::messaging::put_sensitivity_method_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_sensitivity_methods_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_sensitivity_methods_request& v);
    static ores::analytics::messaging::put_many_sensitivity_methods_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_sensitivity_methods_request& v);
    static ores::analytics::messaging::put_many_sensitivity_methods_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::put_many_sensitivity_methods_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::put_many_sensitivity_methods_response& v);
    static ores::analytics::messaging::put_many_sensitivity_methods_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::put_many_sensitivity_methods_response& v);
    static ores::analytics::messaging::put_many_sensitivity_methods_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_sensitivity_method_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_sensitivity_method_request& v);
    static ores::analytics::messaging::delete_sensitivity_method_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_sensitivity_method_request& v);
    static ores::analytics::messaging::delete_sensitivity_method_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_sensitivity_method_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_sensitivity_method_response& v);
    static ores::analytics::messaging::delete_sensitivity_method_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_sensitivity_method_response& v);
    static ores::analytics::messaging::delete_sensitivity_method_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_sensitivity_methods_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_sensitivity_methods_request& v);
    static ores::analytics::messaging::delete_many_sensitivity_methods_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_many_sensitivity_methods_request& v);
    static ores::analytics::messaging::delete_many_sensitivity_methods_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::delete_many_sensitivity_methods_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::delete_many_sensitivity_methods_response& v);
    static ores::analytics::messaging::delete_many_sensitivity_methods_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::delete_many_sensitivity_methods_response& v);
    static ores::analytics::messaging::delete_many_sensitivity_methods_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_sensitivity_method_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_sensitivity_method_versions_request& v);
    static ores::analytics::messaging::list_sensitivity_method_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_sensitivity_method_versions_request& v);
    static ores::analytics::messaging::list_sensitivity_method_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::list_sensitivity_method_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::list_sensitivity_method_versions_response& v);
    static ores::analytics::messaging::list_sensitivity_method_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::list_sensitivity_method_versions_response& v);
    static ores::analytics::messaging::list_sensitivity_method_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_sensitivity_method_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_sensitivity_method_version_request& v);
    static ores::analytics::messaging::get_sensitivity_method_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_sensitivity_method_version_request& v);
    static ores::analytics::messaging::get_sensitivity_method_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_ANALYTICS_API_EXPORT
    wire_traits<ores::analytics::messaging::get_sensitivity_method_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::analytics::messaging::get_sensitivity_method_version_response& v);
    static ores::analytics::messaging::get_sensitivity_method_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::analytics::messaging::get_sensitivity_method_version_response& v);
    static ores::analytics::messaging::get_sensitivity_method_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
