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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_PLAN_DEPENDENCY_PROTOCOL_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_PLAN_DEPENDENCY_PROTOCOL_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.workflow.api/domain/workflow_plan_dependency_codec.hpp"
#include "ores.workflow.api/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::workflow::messaging {

struct workflow_plan_dependency_key {
    boost::uuids::uuid id;

    friend bool operator==(const workflow_plan_dependency_key&,
                           const workflow_plan_dependency_key&) = default;
};

struct workflow_plan_dependency_write {
    boost::uuids::uuid id;
    boost::uuids::uuid workflow_id;
    int consumer_step_index;
    int producer_step_index;

    friend bool operator==(const workflow_plan_dependency_write&,
                           const workflow_plan_dependency_write&) = default;
};

struct workflow_plan_dependency_change {
    workflow_plan_dependency_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const workflow_plan_dependency_change&,
                           const workflow_plan_dependency_change&) = default;
};

struct workflow_plan_dependency_removal {
    workflow_plan_dependency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const workflow_plan_dependency_removal&,
                           const workflow_plan_dependency_removal&) = default;
};

struct workflow_plan_dependency_lookup {
    workflow_plan_dependency_key key;
    std::optional<ores::workflow::domain::workflow_plan_dependency> workflow_plan_dependency;

    friend bool operator==(const workflow_plan_dependency_lookup&,
                           const workflow_plan_dependency_lookup&) = default;
};

struct workflow_plan_dependencies_filter {
    std::optional<boost::uuids::uuid> workflow_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> workflow_id_one_of;

    friend bool operator==(const workflow_plan_dependencies_filter&,
                           const workflow_plan_dependencies_filter&) = default;
};

struct workflow_plan_dependency_event {
    boost::uuids::uuid event_id;
    workflow_plan_dependency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const workflow_plan_dependency_event&,
                           const workflow_plan_dependency_event&) = default;
};

struct workflow_plan_dependency_version_key {
    workflow_plan_dependency_key workflow_plan_dependency;
    std::uint32_t version;

    friend bool operator==(const workflow_plan_dependency_version_key&,
                           const workflow_plan_dependency_version_key&) = default;
};

struct workflow_plan_dependency_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const workflow_plan_dependency_versions_filter&,
                           const workflow_plan_dependency_versions_filter&) = default;
};

struct list_workflow_plan_dependencies_request {
    using response_type = struct list_workflow_plan_dependencies_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_plan_dependencies.list";
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
    std::optional<workflow_plan_dependencies_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_workflow_plan_dependencies_request&,
                           const list_workflow_plan_dependencies_request&) = default;
};

struct list_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_plan_dependency> plan_dependencies;
    std::uint64_t total;

    friend bool operator==(const list_workflow_plan_dependencies_response&,
                           const list_workflow_plan_dependencies_response&) = default;
};

struct get_workflow_plan_dependency_request {
    using response_type = struct get_workflow_plan_dependency_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_plan_dependencies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_plan_dependency_key key;

    friend bool operator==(const get_workflow_plan_dependency_request&,
                           const get_workflow_plan_dependency_request&) = default;
};

struct get_workflow_plan_dependency_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_plan_dependency> workflow_plan_dependency;

    friend bool operator==(const get_workflow_plan_dependency_response&,
                           const get_workflow_plan_dependency_response&) = default;
};

struct get_many_workflow_plan_dependencies_request {
    using response_type = struct get_many_workflow_plan_dependencies_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.workflow_plan_dependencies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_plan_dependency_key> keys;

    friend bool operator==(const get_many_workflow_plan_dependencies_request&,
                           const get_many_workflow_plan_dependencies_request&) = default;
};

struct get_many_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
    std::vector<workflow_plan_dependency_lookup> entries;

    friend bool operator==(const get_many_workflow_plan_dependencies_response&,
                           const get_many_workflow_plan_dependencies_response&) = default;
};

struct put_workflow_plan_dependency_request {
    using response_type = struct put_workflow_plan_dependency_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_plan_dependencies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_plan_dependency_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_workflow_plan_dependency_request&,
                           const put_workflow_plan_dependency_request&) = default;
};

struct put_workflow_plan_dependency_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_plan_dependency> workflow_plan_dependency;

    friend bool operator==(const put_workflow_plan_dependency_response&,
                           const put_workflow_plan_dependency_response&) = default;
};

struct put_many_workflow_plan_dependencies_request {
    using response_type = struct put_many_workflow_plan_dependencies_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.workflow_plan_dependencies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_plan_dependency_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_workflow_plan_dependencies_request&,
                           const put_many_workflow_plan_dependencies_request&) = default;
};

struct put_many_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_plan_dependency> plan_dependencies;

    friend bool operator==(const put_many_workflow_plan_dependencies_response&,
                           const put_many_workflow_plan_dependencies_response&) = default;
};

struct delete_workflow_plan_dependency_request {
    using response_type = struct delete_workflow_plan_dependency_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.workflow_plan_dependencies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_plan_dependency_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_workflow_plan_dependency_request&,
                           const delete_workflow_plan_dependency_request&) = default;
};

struct delete_workflow_plan_dependency_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_workflow_plan_dependency_response&,
                           const delete_workflow_plan_dependency_response&) = default;
};

struct delete_many_workflow_plan_dependencies_request {
    using response_type = struct delete_many_workflow_plan_dependencies_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.workflow_plan_dependencies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_plan_dependency_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_workflow_plan_dependencies_request&,
                           const delete_many_workflow_plan_dependencies_request&) = default;
};

struct delete_many_workflow_plan_dependencies_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_workflow_plan_dependencies_response&,
                           const delete_many_workflow_plan_dependencies_response&) = default;
};

struct list_by_workflow_id_workflow_plan_dependencies_request {
    using response_type = struct list_by_workflow_id_workflow_plan_dependencies_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.workflow_plan_dependencies.list_by_workflow_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid workflow_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<workflow_plan_dependencies_filter> filter;

    friend bool operator==(const list_by_workflow_id_workflow_plan_dependencies_request&,
                           const list_by_workflow_id_workflow_plan_dependencies_request&) = default;
};

struct list_by_workflow_id_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_plan_dependency> plan_dependencies;
    std::uint64_t total;

    friend bool
    operator==(const list_by_workflow_id_workflow_plan_dependencies_response&,
               const list_by_workflow_id_workflow_plan_dependencies_response&) = default;
};

struct list_workflow_plan_dependency_versions_request {
    using response_type = struct list_workflow_plan_dependency_versions_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.workflow_plan_dependencies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_plan_dependency_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<workflow_plan_dependency_versions_filter> filter;

    friend bool operator==(const list_workflow_plan_dependency_versions_request&,
                           const list_workflow_plan_dependency_versions_request&) = default;
};

struct list_workflow_plan_dependency_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_plan_dependency> versions;
    std::uint64_t total;

    friend bool operator==(const list_workflow_plan_dependency_versions_response&,
                           const list_workflow_plan_dependency_versions_response&) = default;
};

struct get_workflow_plan_dependency_version_request {
    using response_type = struct get_workflow_plan_dependency_version_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.workflow_plan_dependencies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_plan_dependency_version_key key;

    friend bool operator==(const get_workflow_plan_dependency_version_request&,
                           const get_workflow_plan_dependency_version_request&) = default;
};

struct get_workflow_plan_dependency_version_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_plan_dependency> version;

    friend bool operator==(const get_workflow_plan_dependency_version_response&,
                           const get_workflow_plan_dependency_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace workflow_plan_dependency_event_subjects {
inline constexpr std::string_view created = "workflow.v1.events.workflow_plan_dependencies.created";
inline constexpr std::string_view updated = "workflow.v1.events.workflow_plan_dependencies.updated";
inline constexpr std::string_view deleted = "workflow.v1.events.workflow_plan_dependencies.deleted";
}
/**
 * @brief The members of workflow_plan_dependency_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_plan_dependency_key>) {
    return std::tuple{
        utility::codec::field{"id", &workflow_plan_dependency_key::id},
    };
}

/**
 * @brief The members of workflow_plan_dependency_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_plan_dependency_write>) {
    return std::tuple{
        utility::codec::field{"id", &workflow_plan_dependency_write::id},
        utility::codec::field{"workflow_id", &workflow_plan_dependency_write::workflow_id},
        utility::codec::field{"consumer_step_index",
                              &workflow_plan_dependency_write::consumer_step_index},
        utility::codec::field{"producer_step_index",
                              &workflow_plan_dependency_write::producer_step_index},
    };
}

/**
 * @brief The members of workflow_plan_dependency_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_plan_dependency_change>) {
    return std::tuple{
        utility::codec::field{"write", &workflow_plan_dependency_change::write},
        utility::codec::field{"precondition", &workflow_plan_dependency_change::precondition},
    };
}

/**
 * @brief The members of workflow_plan_dependency_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_plan_dependency_removal>) {
    return std::tuple{
        utility::codec::field{"key", &workflow_plan_dependency_removal::key},
        utility::codec::field{"precondition", &workflow_plan_dependency_removal::precondition},
    };
}

/**
 * @brief The members of workflow_plan_dependency_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_plan_dependency_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &workflow_plan_dependency_lookup::key},
        utility::codec::field{"workflow_plan_dependency",
                              &workflow_plan_dependency_lookup::workflow_plan_dependency},
    };
}

/**
 * @brief The members of workflow_plan_dependencies_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_plan_dependencies_filter>) {
    return std::tuple{
        utility::codec::field{"workflow_id", &workflow_plan_dependencies_filter::workflow_id},
        utility::codec::field{"id_one_of", &workflow_plan_dependencies_filter::id_one_of},
        utility::codec::field{"workflow_id_one_of",
                              &workflow_plan_dependencies_filter::workflow_id_one_of},
    };
}

/**
 * @brief The members of workflow_plan_dependency_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_plan_dependency_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &workflow_plan_dependency_event::event_id},
        utility::codec::field{"key", &workflow_plan_dependency_event::key},
        utility::codec::field{"action", &workflow_plan_dependency_event::action},
        utility::codec::field{"version", &workflow_plan_dependency_event::version},
        utility::codec::field{"occurred_at", &workflow_plan_dependency_event::occurred_at},
        utility::codec::field{"correlation_id", &workflow_plan_dependency_event::correlation_id},
    };
}

/**
 * @brief The members of workflow_plan_dependency_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_plan_dependency_version_key>) {
    return std::tuple{
        utility::codec::field{"workflow_plan_dependency",
                              &workflow_plan_dependency_version_key::workflow_plan_dependency},
        utility::codec::field{"version", &workflow_plan_dependency_version_key::version},
    };
}

/**
 * @brief The members of workflow_plan_dependency_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<workflow_plan_dependency_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &workflow_plan_dependency_versions_filter::version},
        utility::codec::field{"from_version",
                              &workflow_plan_dependency_versions_filter::from_version},
        utility::codec::field{"to_version", &workflow_plan_dependency_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_workflow_plan_dependencies_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_workflow_plan_dependencies_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_workflow_plan_dependencies_request::offset},
        utility::codec::field{"limit", &list_workflow_plan_dependencies_request::limit},
        utility::codec::field{"order", &list_workflow_plan_dependencies_request::order},
        utility::codec::field{"filter", &list_workflow_plan_dependencies_request::filter},
        utility::codec::field{"as_of", &list_workflow_plan_dependencies_request::as_of},
    };
}

/**
 * @brief The members of list_workflow_plan_dependencies_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_workflow_plan_dependencies_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_workflow_plan_dependencies_response::result},
        utility::codec::field{"plan_dependencies",
                              &list_workflow_plan_dependencies_response::plan_dependencies},
        utility::codec::field{"total", &list_workflow_plan_dependencies_response::total},
    };
}

/**
 * @brief The members of get_workflow_plan_dependency_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_workflow_plan_dependency_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_workflow_plan_dependency_request::key},
    };
}

/**
 * @brief The members of get_workflow_plan_dependency_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_workflow_plan_dependency_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_workflow_plan_dependency_response::result},
        utility::codec::field{"workflow_plan_dependency",
                              &get_workflow_plan_dependency_response::workflow_plan_dependency},
    };
}

/**
 * @brief The members of get_many_workflow_plan_dependencies_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_workflow_plan_dependencies_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_workflow_plan_dependencies_request::keys},
    };
}

/**
 * @brief The members of get_many_workflow_plan_dependencies_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_many_workflow_plan_dependencies_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_workflow_plan_dependencies_response::result},
        utility::codec::field{"entries", &get_many_workflow_plan_dependencies_response::entries},
    };
}

/**
 * @brief The members of put_workflow_plan_dependency_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_workflow_plan_dependency_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_workflow_plan_dependency_request::change},
        utility::codec::field{"intent", &put_workflow_plan_dependency_request::intent},
    };
}

/**
 * @brief The members of put_workflow_plan_dependency_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_workflow_plan_dependency_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_workflow_plan_dependency_response::result},
        utility::codec::field{"workflow_plan_dependency",
                              &put_workflow_plan_dependency_response::workflow_plan_dependency},
    };
}

/**
 * @brief The members of put_many_workflow_plan_dependencies_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_workflow_plan_dependencies_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_workflow_plan_dependencies_request::changes},
        utility::codec::field{"intent", &put_many_workflow_plan_dependencies_request::intent},
    };
}

/**
 * @brief The members of put_many_workflow_plan_dependencies_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<put_many_workflow_plan_dependencies_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_workflow_plan_dependencies_response::result},
        utility::codec::field{"plan_dependencies",
                              &put_many_workflow_plan_dependencies_response::plan_dependencies},
    };
}

/**
 * @brief The members of delete_workflow_plan_dependency_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_workflow_plan_dependency_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_workflow_plan_dependency_request::removal},
        utility::codec::field{"intent", &delete_workflow_plan_dependency_request::intent},
    };
}

/**
 * @brief The members of delete_workflow_plan_dependency_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_workflow_plan_dependency_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_workflow_plan_dependency_response::result},
    };
}

/**
 * @brief The members of delete_many_workflow_plan_dependencies_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_workflow_plan_dependencies_request>) {
    return std::tuple{
        utility::codec::field{"removals",
                              &delete_many_workflow_plan_dependencies_request::removals},
        utility::codec::field{"intent", &delete_many_workflow_plan_dependencies_request::intent},
    };
}

/**
 * @brief The members of delete_many_workflow_plan_dependencies_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_workflow_plan_dependencies_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_workflow_plan_dependencies_response::result},
    };
}

/**
 * @brief The members of list_by_workflow_id_workflow_plan_dependencies_request, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_workflow_id_workflow_plan_dependencies_request>) {
    return std::tuple{
        utility::codec::field{"workflow_id",
                              &list_by_workflow_id_workflow_plan_dependencies_request::workflow_id},
        utility::codec::field{"scope",
                              &list_by_workflow_id_workflow_plan_dependencies_request::scope},
        utility::codec::field{"offset",
                              &list_by_workflow_id_workflow_plan_dependencies_request::offset},
        utility::codec::field{"limit",
                              &list_by_workflow_id_workflow_plan_dependencies_request::limit},
        utility::codec::field{"order",
                              &list_by_workflow_id_workflow_plan_dependencies_request::order},
        utility::codec::field{"filter",
                              &list_by_workflow_id_workflow_plan_dependencies_request::filter},
    };
}

/**
 * @brief The members of list_by_workflow_id_workflow_plan_dependencies_response, named as the wire
 * names them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_workflow_id_workflow_plan_dependencies_response>) {
    return std::tuple{
        utility::codec::field{"result",
                              &list_by_workflow_id_workflow_plan_dependencies_response::result},
        utility::codec::field{
            "plan_dependencies",
            &list_by_workflow_id_workflow_plan_dependencies_response::plan_dependencies},
        utility::codec::field{"total",
                              &list_by_workflow_id_workflow_plan_dependencies_response::total},
    };
}

/**
 * @brief The members of list_workflow_plan_dependency_versions_request, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_workflow_plan_dependency_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_workflow_plan_dependency_versions_request::key},
        utility::codec::field{"offset", &list_workflow_plan_dependency_versions_request::offset},
        utility::codec::field{"limit", &list_workflow_plan_dependency_versions_request::limit},
        utility::codec::field{"order", &list_workflow_plan_dependency_versions_request::order},
        utility::codec::field{"filter", &list_workflow_plan_dependency_versions_request::filter},
    };
}

/**
 * @brief The members of list_workflow_plan_dependency_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_workflow_plan_dependency_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_workflow_plan_dependency_versions_response::result},
        utility::codec::field{"versions",
                              &list_workflow_plan_dependency_versions_response::versions},
        utility::codec::field{"total", &list_workflow_plan_dependency_versions_response::total},
    };
}

/**
 * @brief The members of get_workflow_plan_dependency_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_workflow_plan_dependency_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_workflow_plan_dependency_version_request::key},
    };
}

/**
 * @brief The members of get_workflow_plan_dependency_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_workflow_plan_dependency_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_workflow_plan_dependency_version_response::result},
        utility::codec::field{"version", &get_workflow_plan_dependency_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependency_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_plan_dependency_key& v);
    static ores::workflow::messaging::workflow_plan_dependency_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependency_key& v);
    static ores::workflow::messaging::workflow_plan_dependency_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependency_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_plan_dependency_write& v);
    static ores::workflow::messaging::workflow_plan_dependency_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependency_write& v);
    static ores::workflow::messaging::workflow_plan_dependency_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependency_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_plan_dependency_change& v);
    static ores::workflow::messaging::workflow_plan_dependency_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependency_change& v);
    static ores::workflow::messaging::workflow_plan_dependency_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependency_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_plan_dependency_removal& v);
    static ores::workflow::messaging::workflow_plan_dependency_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependency_removal& v);
    static ores::workflow::messaging::workflow_plan_dependency_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependency_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_plan_dependency_lookup& v);
    static ores::workflow::messaging::workflow_plan_dependency_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependency_lookup& v);
    static ores::workflow::messaging::workflow_plan_dependency_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependencies_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_plan_dependencies_filter& v);
    static ores::workflow::messaging::workflow_plan_dependencies_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependencies_filter& v);
    static ores::workflow::messaging::workflow_plan_dependencies_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependency_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_plan_dependency_event& v);
    static ores::workflow::messaging::workflow_plan_dependency_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependency_event& v);
    static ores::workflow::messaging::workflow_plan_dependency_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependency_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_plan_dependency_version_key& v);
    static ores::workflow::messaging::workflow_plan_dependency_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependency_version_key& v);
    static ores::workflow::messaging::workflow_plan_dependency_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_plan_dependency_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_plan_dependency_versions_filter& v);
    static ores::workflow::messaging::workflow_plan_dependency_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_plan_dependency_versions_filter& v);
    static ores::workflow::messaging::workflow_plan_dependency_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_plan_dependencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::list_workflow_plan_dependencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::list_workflow_plan_dependencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_plan_dependencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_plan_dependencies_response& v);
    static ores::workflow::messaging::list_workflow_plan_dependencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_plan_dependencies_response& v);
    static ores::workflow::messaging::list_workflow_plan_dependencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_plan_dependency_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_plan_dependency_request& v);
    static ores::workflow::messaging::get_workflow_plan_dependency_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_plan_dependency_request& v);
    static ores::workflow::messaging::get_workflow_plan_dependency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_plan_dependency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_plan_dependency_response& v);
    static ores::workflow::messaging::get_workflow_plan_dependency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_plan_dependency_response& v);
    static ores::workflow::messaging::get_workflow_plan_dependency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_many_workflow_plan_dependencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_many_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::get_many_workflow_plan_dependencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_many_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::get_many_workflow_plan_dependencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_many_workflow_plan_dependencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_many_workflow_plan_dependencies_response& v);
    static ores::workflow::messaging::get_many_workflow_plan_dependencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_many_workflow_plan_dependencies_response& v);
    static ores::workflow::messaging::get_many_workflow_plan_dependencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_workflow_plan_dependency_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_workflow_plan_dependency_request& v);
    static ores::workflow::messaging::put_workflow_plan_dependency_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_workflow_plan_dependency_request& v);
    static ores::workflow::messaging::put_workflow_plan_dependency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_workflow_plan_dependency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_workflow_plan_dependency_response& v);
    static ores::workflow::messaging::put_workflow_plan_dependency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_workflow_plan_dependency_response& v);
    static ores::workflow::messaging::put_workflow_plan_dependency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_many_workflow_plan_dependencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_many_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::put_many_workflow_plan_dependencies_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_many_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::put_many_workflow_plan_dependencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_many_workflow_plan_dependencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_many_workflow_plan_dependencies_response& v);
    static ores::workflow::messaging::put_many_workflow_plan_dependencies_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_many_workflow_plan_dependencies_response& v);
    static ores::workflow::messaging::put_many_workflow_plan_dependencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_workflow_plan_dependency_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_workflow_plan_dependency_request& v);
    static ores::workflow::messaging::delete_workflow_plan_dependency_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_workflow_plan_dependency_request& v);
    static ores::workflow::messaging::delete_workflow_plan_dependency_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_workflow_plan_dependency_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_workflow_plan_dependency_response& v);
    static ores::workflow::messaging::delete_workflow_plan_dependency_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_workflow_plan_dependency_response& v);
    static ores::workflow::messaging::delete_workflow_plan_dependency_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_many_workflow_plan_dependencies_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_many_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::delete_many_workflow_plan_dependencies_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::workflow::messaging::delete_many_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::delete_many_workflow_plan_dependencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_many_workflow_plan_dependencies_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_many_workflow_plan_dependencies_response& v);
    static ores::workflow::messaging::delete_many_workflow_plan_dependencies_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::workflow::messaging::delete_many_workflow_plan_dependencies_response& v);
    static ores::workflow::messaging::delete_many_workflow_plan_dependencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_request& v);
    static ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<
    ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(
        const ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_response&
            v);
    static ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_response&
            v);
    static ores::workflow::messaging::list_by_workflow_id_workflow_plan_dependencies_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_plan_dependency_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_plan_dependency_versions_request& v);
    static ores::workflow::messaging::list_workflow_plan_dependency_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::workflow::messaging::list_workflow_plan_dependency_versions_request& v);
    static ores::workflow::messaging::list_workflow_plan_dependency_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_plan_dependency_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_plan_dependency_versions_response& v);
    static ores::workflow::messaging::list_workflow_plan_dependency_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::workflow::messaging::list_workflow_plan_dependency_versions_response& v);
    static ores::workflow::messaging::list_workflow_plan_dependency_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_plan_dependency_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_plan_dependency_version_request& v);
    static ores::workflow::messaging::get_workflow_plan_dependency_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_plan_dependency_version_request& v);
    static ores::workflow::messaging::get_workflow_plan_dependency_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_plan_dependency_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_plan_dependency_version_response& v);
    static ores::workflow::messaging::get_workflow_plan_dependency_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::workflow::messaging::get_workflow_plan_dependency_version_response& v);
    static ores::workflow::messaging::get_workflow_plan_dependency_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
