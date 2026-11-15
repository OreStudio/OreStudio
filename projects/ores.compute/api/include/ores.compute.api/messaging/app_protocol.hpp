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
#ifndef ORES_COMPUTE_API_MESSAGING_APP_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_APP_PROTOCOL_HPP

#include "ores.compute.api/domain/app_codec.hpp"
#include "ores.compute.api/export.hpp"
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

namespace ores::compute::messaging {

struct app_key {
    std::string name;

    friend bool operator==(const app_key&, const app_key&) = default;
};

struct app_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;

    friend bool operator==(const app_write&, const app_write&) = default;
};

struct app_change {
    app_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const app_change&, const app_change&) = default;
};

struct app_removal {
    app_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const app_removal&, const app_removal&) = default;
};

struct app_lookup {
    app_key key;
    std::optional<ores::compute::domain::app> app;

    friend bool operator==(const app_lookup&, const app_lookup&) = default;
};

struct apps_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const apps_filter&, const apps_filter&) = default;
};

struct app_event {
    boost::uuids::uuid event_id;
    app_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const app_event&, const app_event&) = default;
};

struct apps_version_key {
    app_key app;
    std::uint32_t version{};

    friend bool operator==(const apps_version_key&, const apps_version_key&) = default;
};

struct apps_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const apps_versions_filter&, const apps_versions_filter&) = default;
};

struct list_apps_request {
    using response_type = struct list_apps_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.list";
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
    std::optional<apps_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_apps_request&, const list_apps_request&) = default;
};

struct list_apps_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app> apps;
    std::uint64_t total{};

    friend bool operator==(const list_apps_response&, const list_apps_response&) = default;
};

struct get_app_request {
    using response_type = struct get_app_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_key key;

    friend bool operator==(const get_app_request&, const get_app_request&) = default;
};

struct get_app_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::app> app;

    friend bool operator==(const get_app_response&, const get_app_response&) = default;
};

struct get_many_apps_request {
    using response_type = struct get_many_apps_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_key> keys;

    friend bool operator==(const get_many_apps_request&, const get_many_apps_request&) = default;
};

struct get_many_apps_response {
    ores::utility::domain::result result;
    std::vector<app_lookup> entries;

    friend bool operator==(const get_many_apps_response&, const get_many_apps_response&) = default;
};

struct put_app_request {
    using response_type = struct put_app_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_app_request&, const put_app_request&) = default;
};

struct put_app_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::app> app;

    friend bool operator==(const put_app_response&, const put_app_response&) = default;
};

struct put_many_apps_request {
    using response_type = struct put_many_apps_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_apps_request&, const put_many_apps_request&) = default;
};

struct put_many_apps_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app> apps;

    friend bool operator==(const put_many_apps_response&, const put_many_apps_response&) = default;
};

struct delete_app_request {
    using response_type = struct delete_app_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_app_request&, const delete_app_request&) = default;
};

struct delete_app_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_app_response&, const delete_app_response&) = default;
};

struct delete_many_apps_request {
    using response_type = struct delete_many_apps_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_apps_request&,
                           const delete_many_apps_request&) = default;
};

struct delete_many_apps_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_apps_response&,
                           const delete_many_apps_response&) = default;
};

struct list_apps_versions_request {
    using response_type = struct list_apps_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<apps_versions_filter> filter;

    friend bool operator==(const list_apps_versions_request&,
                           const list_apps_versions_request&) = default;
};

struct list_apps_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app> versions;
    std::uint64_t total{};

    friend bool operator==(const list_apps_versions_response&,
                           const list_apps_versions_response&) = default;
};

struct get_apps_version_request {
    using response_type = struct get_apps_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    apps_version_key key;

    friend bool operator==(const get_apps_version_request&,
                           const get_apps_version_request&) = default;
};

struct get_apps_version_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::app> version;

    friend bool operator==(const get_apps_version_response&,
                           const get_apps_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace app_event_subjects {
inline constexpr std::string_view created = "compute.v1.events.apps.created";
inline constexpr std::string_view updated = "compute.v1.events.apps.updated";
inline constexpr std::string_view deleted = "compute.v1.events.apps.deleted";
}
/**
 * @brief The members of app_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"name", &app_key::name},
    };
}

/**
 * @brief The members of app_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &app_write::id},
        utility::codec::field{"name", &app_write::name},
        utility::codec::field{"description", &app_write::description},
    };
}

/**
 * @brief The members of app_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &app_change::write},
        utility::codec::field{"precondition", &app_change::precondition},
    };
}

/**
 * @brief The members of app_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &app_removal::key},
        utility::codec::field{"precondition", &app_removal::precondition},
    };
}

/**
 * @brief The members of app_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &app_lookup::key},
        utility::codec::field{"app", &app_lookup::app},
    };
}

/**
 * @brief The members of apps_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<apps_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id_one_of", &apps_filter::id_one_of},
    };
}

/**
 * @brief The members of app_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<app_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &app_event::event_id},
        utility::codec::field{"key", &app_event::key},
        utility::codec::field{"action", &app_event::action},
        utility::codec::field{"version", &app_event::version},
        utility::codec::field{"occurred_at", &app_event::occurred_at},
        utility::codec::field{"correlation_id", &app_event::correlation_id},
    };
}

/**
 * @brief The members of apps_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<apps_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"app", &apps_version_key::app},
        utility::codec::field{"version", &apps_version_key::version},
    };
}

/**
 * @brief The members of apps_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<apps_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &apps_versions_filter::version},
        utility::codec::field{"from_version", &apps_versions_filter::from_version},
        utility::codec::field{"to_version", &apps_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_apps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_apps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_apps_request::offset},
        utility::codec::field{"limit", &list_apps_request::limit},
        utility::codec::field{"order", &list_apps_request::order},
        utility::codec::field{"filter", &list_apps_request::filter},
        utility::codec::field{"as_of", &list_apps_request::as_of},
    };
}

/**
 * @brief The members of list_apps_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_apps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_apps_response::result},
        utility::codec::field{"apps", &list_apps_response::apps},
        utility::codec::field{"total", &list_apps_response::total},
    };
}

/**
 * @brief The members of get_app_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_app_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_app_request::key},
    };
}

/**
 * @brief The members of get_app_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_app_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_app_response::result},
        utility::codec::field{"app", &get_app_response::app},
    };
}

/**
 * @brief The members of get_many_apps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_apps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_apps_request::keys},
    };
}

/**
 * @brief The members of get_many_apps_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_apps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_apps_response::result},
        utility::codec::field{"entries", &get_many_apps_response::entries},
    };
}

/**
 * @brief The members of put_app_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_app_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_app_request::change},
        utility::codec::field{"intent", &put_app_request::intent},
    };
}

/**
 * @brief The members of put_app_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_app_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_app_response::result},
        utility::codec::field{"app", &put_app_response::app},
    };
}

/**
 * @brief The members of put_many_apps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_apps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_apps_request::changes},
        utility::codec::field{"intent", &put_many_apps_request::intent},
    };
}

/**
 * @brief The members of put_many_apps_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_apps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_apps_response::result},
        utility::codec::field{"apps", &put_many_apps_response::apps},
    };
}

/**
 * @brief The members of delete_app_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_app_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_app_request::removal},
        utility::codec::field{"intent", &delete_app_request::intent},
    };
}

/**
 * @brief The members of delete_app_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_app_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_app_response::result},
    };
}

/**
 * @brief The members of delete_many_apps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_apps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_apps_request::removals},
        utility::codec::field{"intent", &delete_many_apps_request::intent},
    };
}

/**
 * @brief The members of delete_many_apps_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_apps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_apps_response::result},
    };
}

/**
 * @brief The members of list_apps_versions_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_apps_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_apps_versions_request::key},
        utility::codec::field{"offset", &list_apps_versions_request::offset},
        utility::codec::field{"limit", &list_apps_versions_request::limit},
        utility::codec::field{"order", &list_apps_versions_request::order},
        utility::codec::field{"filter", &list_apps_versions_request::filter},
    };
}

/**
 * @brief The members of list_apps_versions_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_apps_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_apps_versions_response::result},
        utility::codec::field{"versions", &list_apps_versions_response::versions},
        utility::codec::field{"total", &list_apps_versions_response::total},
    };
}

/**
 * @brief The members of get_apps_version_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_apps_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_apps_version_request::key},
    };
}

/**
 * @brief The members of get_apps_version_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_apps_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_apps_version_response::result},
        utility::codec::field{"version", &get_apps_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_key& v);
    static ores::compute::messaging::app_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::app_key& v);
    static ores::compute::messaging::app_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_write& v);
    static ores::compute::messaging::app_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::app_write& v);
    static ores::compute::messaging::app_write read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_change& v);
    static ores::compute::messaging::app_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::app_change& v);
    static ores::compute::messaging::app_change read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_removal& v);
    static ores::compute::messaging::app_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::app_removal& v);
    static ores::compute::messaging::app_removal read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_lookup& v);
    static ores::compute::messaging::app_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::app_lookup& v);
    static ores::compute::messaging::app_lookup read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::apps_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::apps_filter& v);
    static ores::compute::messaging::apps_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::apps_filter& v);
    static ores::compute::messaging::apps_filter read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::app_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::app_event& v);
    static ores::compute::messaging::app_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::app_event& v);
    static ores::compute::messaging::app_event read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::apps_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::apps_version_key& v);
    static ores::compute::messaging::apps_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::apps_version_key& v);
    static ores::compute::messaging::apps_version_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::apps_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::apps_versions_filter& v);
    static ores::compute::messaging::apps_versions_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::apps_versions_filter& v);
    static ores::compute::messaging::apps_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_apps_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_apps_request& v);
    static ores::compute::messaging::list_apps_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_apps_request& v);
    static ores::compute::messaging::list_apps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_apps_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_apps_response& v);
    static ores::compute::messaging::list_apps_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_apps_response& v);
    static ores::compute::messaging::list_apps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_app_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_app_request& v);
    static ores::compute::messaging::get_app_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_app_request& v);
    static ores::compute::messaging::get_app_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_app_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_app_response& v);
    static ores::compute::messaging::get_app_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_app_response& v);
    static ores::compute::messaging::get_app_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_many_apps_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_many_apps_request& v);
    static ores::compute::messaging::get_many_apps_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_many_apps_request& v);
    static ores::compute::messaging::get_many_apps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_many_apps_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_many_apps_response& v);
    static ores::compute::messaging::get_many_apps_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_many_apps_response& v);
    static ores::compute::messaging::get_many_apps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_app_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_app_request& v);
    static ores::compute::messaging::put_app_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_app_request& v);
    static ores::compute::messaging::put_app_request read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_app_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_app_response& v);
    static ores::compute::messaging::put_app_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_app_response& v);
    static ores::compute::messaging::put_app_response read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_many_apps_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_many_apps_request& v);
    static ores::compute::messaging::put_many_apps_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_many_apps_request& v);
    static ores::compute::messaging::put_many_apps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::put_many_apps_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::put_many_apps_response& v);
    static ores::compute::messaging::put_many_apps_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::put_many_apps_response& v);
    static ores::compute::messaging::put_many_apps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_app_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_app_request& v);
    static ores::compute::messaging::delete_app_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_app_request& v);
    static ores::compute::messaging::delete_app_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_app_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_app_response& v);
    static ores::compute::messaging::delete_app_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_app_response& v);
    static ores::compute::messaging::delete_app_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_many_apps_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_many_apps_request& v);
    static ores::compute::messaging::delete_many_apps_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_many_apps_request& v);
    static ores::compute::messaging::delete_many_apps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::delete_many_apps_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::delete_many_apps_response& v);
    static ores::compute::messaging::delete_many_apps_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::delete_many_apps_response& v);
    static ores::compute::messaging::delete_many_apps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_apps_versions_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_apps_versions_request& v);
    static ores::compute::messaging::list_apps_versions_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::list_apps_versions_request& v);
    static ores::compute::messaging::list_apps_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::list_apps_versions_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::list_apps_versions_response& v);
    static ores::compute::messaging::list_apps_versions_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::compute::messaging::list_apps_versions_response& v);
    static ores::compute::messaging::list_apps_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_apps_version_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_apps_version_request& v);
    static ores::compute::messaging::get_apps_version_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_apps_version_request& v);
    static ores::compute::messaging::get_apps_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_COMPUTE_API_EXPORT wire_traits<ores::compute::messaging::get_apps_version_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::compute::messaging::get_apps_version_response& v);
    static ores::compute::messaging::get_apps_version_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::compute::messaging::get_apps_version_response& v);
    static ores::compute::messaging::get_apps_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
