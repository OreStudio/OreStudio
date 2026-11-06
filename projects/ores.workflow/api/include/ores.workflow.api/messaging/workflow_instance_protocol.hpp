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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_INSTANCE_PROTOCOL_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_INSTANCE_PROTOCOL_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.workflow.api/domain/workflow_instance.hpp"
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

struct workflow_instance_key {
    boost::uuids::uuid id;

    friend bool operator==(const workflow_instance_key&, const workflow_instance_key&) = default;
};

struct workflow_instance_write {
    boost::uuids::uuid id;
    std::string type;
    std::string target_kind;
    boost::uuids::uuid target_id;
    boost::uuids::uuid state_id;
    std::string request_json;
    std::string result_json;
    std::string error;
    std::string correlation_id;
    std::string created_by;
    int step_count;

    friend bool operator==(const workflow_instance_write&,
                           const workflow_instance_write&) = default;
};

struct workflow_instance_change {
    workflow_instance_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const workflow_instance_change&,
                           const workflow_instance_change&) = default;
};

struct workflow_instance_removal {
    workflow_instance_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const workflow_instance_removal&,
                           const workflow_instance_removal&) = default;
};

struct workflow_instance_lookup {
    workflow_instance_key key;
    std::optional<ores::workflow::domain::workflow_instance> workflow_instance;

    friend bool operator==(const workflow_instance_lookup&,
                           const workflow_instance_lookup&) = default;
};

struct workflow_instances_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;

    friend bool operator==(const workflow_instances_filter&,
                           const workflow_instances_filter&) = default;
};

struct workflow_instance_event {
    boost::uuids::uuid event_id;
    workflow_instance_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const workflow_instance_event&,
                           const workflow_instance_event&) = default;
};

struct workflow_instance_version_key {
    workflow_instance_key workflow_instance;
    std::uint32_t version;

    friend bool operator==(const workflow_instance_version_key&,
                           const workflow_instance_version_key&) = default;
};

struct workflow_instance_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const workflow_instance_versions_filter&,
                           const workflow_instance_versions_filter&) = default;
};

struct list_workflow_instances_request {
    using response_type = struct list_workflow_instances_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances.list";
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
    std::optional<workflow_instances_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_workflow_instances_request&,
                           const list_workflow_instances_request&) = default;
};

struct list_workflow_instances_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_instance> instances;
    std::uint64_t total;

    friend bool operator==(const list_workflow_instances_response&,
                           const list_workflow_instances_response&) = default;
};

struct get_workflow_instance_request {
    using response_type = struct get_workflow_instance_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_instance_key key;

    friend bool operator==(const get_workflow_instance_request&,
                           const get_workflow_instance_request&) = default;
};

struct get_workflow_instance_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_instance> workflow_instance;

    friend bool operator==(const get_workflow_instance_response&,
                           const get_workflow_instance_response&) = default;
};

struct get_many_workflow_instances_request {
    using response_type = struct get_many_workflow_instances_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_instance_key> keys;

    friend bool operator==(const get_many_workflow_instances_request&,
                           const get_many_workflow_instances_request&) = default;
};

struct get_many_workflow_instances_response {
    ores::utility::domain::result result;
    std::vector<workflow_instance_lookup> entries;

    friend bool operator==(const get_many_workflow_instances_response&,
                           const get_many_workflow_instances_response&) = default;
};

struct put_workflow_instance_request {
    using response_type = struct put_workflow_instance_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_instance_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_workflow_instance_request&,
                           const put_workflow_instance_request&) = default;
};

struct put_workflow_instance_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_instance> workflow_instance;

    friend bool operator==(const put_workflow_instance_response&,
                           const put_workflow_instance_response&) = default;
};

struct put_many_workflow_instances_request {
    using response_type = struct put_many_workflow_instances_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_instance_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_workflow_instances_request&,
                           const put_many_workflow_instances_request&) = default;
};

struct put_many_workflow_instances_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_instance> instances;

    friend bool operator==(const put_many_workflow_instances_response&,
                           const put_many_workflow_instances_response&) = default;
};

struct delete_workflow_instance_request {
    using response_type = struct delete_workflow_instance_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_instance_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_workflow_instance_request&,
                           const delete_workflow_instance_request&) = default;
};

struct delete_workflow_instance_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_workflow_instance_response&,
                           const delete_workflow_instance_response&) = default;
};

struct delete_many_workflow_instances_request {
    using response_type = struct delete_many_workflow_instances_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_instance_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_workflow_instances_request&,
                           const delete_many_workflow_instances_request&) = default;
};

struct delete_many_workflow_instances_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_workflow_instances_response&,
                           const delete_many_workflow_instances_response&) = default;
};

struct list_workflow_instance_versions_request {
    using response_type = struct list_workflow_instance_versions_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_instance_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<workflow_instance_versions_filter> filter;

    friend bool operator==(const list_workflow_instance_versions_request&,
                           const list_workflow_instance_versions_request&) = default;
};

struct list_workflow_instance_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_instance> versions;
    std::uint64_t total;

    friend bool operator==(const list_workflow_instance_versions_response&,
                           const list_workflow_instance_versions_response&) = default;
};

struct get_workflow_instance_version_request {
    using response_type = struct get_workflow_instance_version_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_instances_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_instance_version_key key;

    friend bool operator==(const get_workflow_instance_version_request&,
                           const get_workflow_instance_version_request&) = default;
};

struct get_workflow_instance_version_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_instance> version;

    friend bool operator==(const get_workflow_instance_version_response&,
                           const get_workflow_instance_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace workflow_instance_event_subjects {
inline constexpr std::string_view created = "workflow.v1.events.workflow_instances.created";
inline constexpr std::string_view updated = "workflow.v1.events.workflow_instances.updated";
inline constexpr std::string_view deleted = "workflow.v1.events.workflow_instances.deleted";
}
/**
 * @brief The members of workflow_instance_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance_key>) {
    return std::tuple{
        utility::codec::field{"id", &workflow_instance_key::id},
    };
}

/**
 * @brief The members of workflow_instance_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance_write>) {
    return std::tuple{
        utility::codec::field{"id", &workflow_instance_write::id},
        utility::codec::field{"type", &workflow_instance_write::type},
        utility::codec::field{"target_kind", &workflow_instance_write::target_kind},
        utility::codec::field{"target_id", &workflow_instance_write::target_id},
        utility::codec::field{"state_id", &workflow_instance_write::state_id},
        utility::codec::field{"request_json", &workflow_instance_write::request_json},
        utility::codec::field{"result_json", &workflow_instance_write::result_json},
        utility::codec::field{"error", &workflow_instance_write::error},
        utility::codec::field{"correlation_id", &workflow_instance_write::correlation_id},
        utility::codec::field{"created_by", &workflow_instance_write::created_by},
        utility::codec::field{"step_count", &workflow_instance_write::step_count},
    };
}

/**
 * @brief The members of workflow_instance_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance_change>) {
    return std::tuple{
        utility::codec::field{"write", &workflow_instance_change::write},
        utility::codec::field{"precondition", &workflow_instance_change::precondition},
    };
}

/**
 * @brief The members of workflow_instance_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance_removal>) {
    return std::tuple{
        utility::codec::field{"key", &workflow_instance_removal::key},
        utility::codec::field{"precondition", &workflow_instance_removal::precondition},
    };
}

/**
 * @brief The members of workflow_instance_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &workflow_instance_lookup::key},
        utility::codec::field{"workflow_instance", &workflow_instance_lookup::workflow_instance},
    };
}

/**
 * @brief The members of workflow_instances_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instances_filter>) {
    return std::tuple{
        utility::codec::field{"id_one_of", &workflow_instances_filter::id_one_of},
    };
}

/**
 * @brief The members of workflow_instance_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &workflow_instance_event::event_id},
        utility::codec::field{"key", &workflow_instance_event::key},
        utility::codec::field{"action", &workflow_instance_event::action},
        utility::codec::field{"version", &workflow_instance_event::version},
        utility::codec::field{"occurred_at", &workflow_instance_event::occurred_at},
        utility::codec::field{"correlation_id", &workflow_instance_event::correlation_id},
    };
}

/**
 * @brief The members of workflow_instance_version_key, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance_version_key>) {
    return std::tuple{
        utility::codec::field{"workflow_instance",
                              &workflow_instance_version_key::workflow_instance},
        utility::codec::field{"version", &workflow_instance_version_key::version},
    };
}

/**
 * @brief The members of workflow_instance_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_instance_versions_filter>) {
    return std::tuple{
        utility::codec::field{"version", &workflow_instance_versions_filter::version},
        utility::codec::field{"from_version", &workflow_instance_versions_filter::from_version},
        utility::codec::field{"to_version", &workflow_instance_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_workflow_instances_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_workflow_instances_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_workflow_instances_request::offset},
        utility::codec::field{"limit", &list_workflow_instances_request::limit},
        utility::codec::field{"order", &list_workflow_instances_request::order},
        utility::codec::field{"filter", &list_workflow_instances_request::filter},
        utility::codec::field{"as_of", &list_workflow_instances_request::as_of},
    };
}

/**
 * @brief The members of list_workflow_instances_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_workflow_instances_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_workflow_instances_response::result},
        utility::codec::field{"instances", &list_workflow_instances_response::instances},
        utility::codec::field{"total", &list_workflow_instances_response::total},
    };
}

/**
 * @brief The members of get_workflow_instance_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_workflow_instance_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_workflow_instance_request::key},
    };
}

/**
 * @brief The members of get_workflow_instance_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_workflow_instance_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_workflow_instance_response::result},
        utility::codec::field{"workflow_instance",
                              &get_workflow_instance_response::workflow_instance},
    };
}

/**
 * @brief The members of get_many_workflow_instances_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_workflow_instances_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_workflow_instances_request::keys},
    };
}

/**
 * @brief The members of get_many_workflow_instances_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_workflow_instances_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_workflow_instances_response::result},
        utility::codec::field{"entries", &get_many_workflow_instances_response::entries},
    };
}

/**
 * @brief The members of put_workflow_instance_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_workflow_instance_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_workflow_instance_request::change},
        utility::codec::field{"intent", &put_workflow_instance_request::intent},
    };
}

/**
 * @brief The members of put_workflow_instance_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_workflow_instance_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_workflow_instance_response::result},
        utility::codec::field{"workflow_instance",
                              &put_workflow_instance_response::workflow_instance},
    };
}

/**
 * @brief The members of put_many_workflow_instances_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_workflow_instances_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_workflow_instances_request::changes},
        utility::codec::field{"intent", &put_many_workflow_instances_request::intent},
    };
}

/**
 * @brief The members of put_many_workflow_instances_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_workflow_instances_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_workflow_instances_response::result},
        utility::codec::field{"instances", &put_many_workflow_instances_response::instances},
    };
}

/**
 * @brief The members of delete_workflow_instance_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_workflow_instance_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_workflow_instance_request::removal},
        utility::codec::field{"intent", &delete_workflow_instance_request::intent},
    };
}

/**
 * @brief The members of delete_workflow_instance_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_workflow_instance_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_workflow_instance_response::result},
    };
}

/**
 * @brief The members of delete_many_workflow_instances_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_workflow_instances_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_workflow_instances_request::removals},
        utility::codec::field{"intent", &delete_many_workflow_instances_request::intent},
    };
}

/**
 * @brief The members of delete_many_workflow_instances_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_workflow_instances_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_workflow_instances_response::result},
    };
}

/**
 * @brief The members of list_workflow_instance_versions_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_workflow_instance_versions_request>) {
    return std::tuple{
        utility::codec::field{"key", &list_workflow_instance_versions_request::key},
        utility::codec::field{"offset", &list_workflow_instance_versions_request::offset},
        utility::codec::field{"limit", &list_workflow_instance_versions_request::limit},
        utility::codec::field{"order", &list_workflow_instance_versions_request::order},
        utility::codec::field{"filter", &list_workflow_instance_versions_request::filter},
    };
}

/**
 * @brief The members of list_workflow_instance_versions_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_workflow_instance_versions_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_workflow_instance_versions_response::result},
        utility::codec::field{"versions", &list_workflow_instance_versions_response::versions},
        utility::codec::field{"total", &list_workflow_instance_versions_response::total},
    };
}

/**
 * @brief The members of get_workflow_instance_version_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_workflow_instance_version_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_workflow_instance_version_request::key},
    };
}

/**
 * @brief The members of get_workflow_instance_version_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<get_workflow_instance_version_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_workflow_instance_version_response::result},
        utility::codec::field{"version", &get_workflow_instance_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_instance_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_instance_key& v);
    static ores::workflow::messaging::workflow_instance_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_instance_key& v);
    static ores::workflow::messaging::workflow_instance_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_instance_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_instance_write& v);
    static ores::workflow::messaging::workflow_instance_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_instance_write& v);
    static ores::workflow::messaging::workflow_instance_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_instance_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_instance_change& v);
    static ores::workflow::messaging::workflow_instance_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_instance_change& v);
    static ores::workflow::messaging::workflow_instance_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_instance_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_instance_removal& v);
    static ores::workflow::messaging::workflow_instance_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_instance_removal& v);
    static ores::workflow::messaging::workflow_instance_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_instance_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_instance_lookup& v);
    static ores::workflow::messaging::workflow_instance_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_instance_lookup& v);
    static ores::workflow::messaging::workflow_instance_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_instances_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_instances_filter& v);
    static ores::workflow::messaging::workflow_instances_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_instances_filter& v);
    static ores::workflow::messaging::workflow_instances_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_instance_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_instance_event& v);
    static ores::workflow::messaging::workflow_instance_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_instance_event& v);
    static ores::workflow::messaging::workflow_instance_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_instance_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_instance_version_key& v);
    static ores::workflow::messaging::workflow_instance_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_instance_version_key& v);
    static ores::workflow::messaging::workflow_instance_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_instance_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_instance_versions_filter& v);
    static ores::workflow::messaging::workflow_instance_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_instance_versions_filter& v);
    static ores::workflow::messaging::workflow_instance_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_instances_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_instances_request& v);
    static ores::workflow::messaging::list_workflow_instances_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_instances_request& v);
    static ores::workflow::messaging::list_workflow_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_instances_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_instances_response& v);
    static ores::workflow::messaging::list_workflow_instances_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_instances_response& v);
    static ores::workflow::messaging::list_workflow_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_instance_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_instance_request& v);
    static ores::workflow::messaging::get_workflow_instance_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_instance_request& v);
    static ores::workflow::messaging::get_workflow_instance_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_instance_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_instance_response& v);
    static ores::workflow::messaging::get_workflow_instance_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_instance_response& v);
    static ores::workflow::messaging::get_workflow_instance_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_many_workflow_instances_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_many_workflow_instances_request& v);
    static ores::workflow::messaging::get_many_workflow_instances_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_many_workflow_instances_request& v);
    static ores::workflow::messaging::get_many_workflow_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_many_workflow_instances_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_many_workflow_instances_response& v);
    static ores::workflow::messaging::get_many_workflow_instances_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_many_workflow_instances_response& v);
    static ores::workflow::messaging::get_many_workflow_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_workflow_instance_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_workflow_instance_request& v);
    static ores::workflow::messaging::put_workflow_instance_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_workflow_instance_request& v);
    static ores::workflow::messaging::put_workflow_instance_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_workflow_instance_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_workflow_instance_response& v);
    static ores::workflow::messaging::put_workflow_instance_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_workflow_instance_response& v);
    static ores::workflow::messaging::put_workflow_instance_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_many_workflow_instances_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_many_workflow_instances_request& v);
    static ores::workflow::messaging::put_many_workflow_instances_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_many_workflow_instances_request& v);
    static ores::workflow::messaging::put_many_workflow_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_many_workflow_instances_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_many_workflow_instances_response& v);
    static ores::workflow::messaging::put_many_workflow_instances_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_many_workflow_instances_response& v);
    static ores::workflow::messaging::put_many_workflow_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_workflow_instance_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_workflow_instance_request& v);
    static ores::workflow::messaging::delete_workflow_instance_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_workflow_instance_request& v);
    static ores::workflow::messaging::delete_workflow_instance_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_workflow_instance_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_workflow_instance_response& v);
    static ores::workflow::messaging::delete_workflow_instance_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_workflow_instance_response& v);
    static ores::workflow::messaging::delete_workflow_instance_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_many_workflow_instances_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_many_workflow_instances_request& v);
    static ores::workflow::messaging::delete_many_workflow_instances_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_many_workflow_instances_request& v);
    static ores::workflow::messaging::delete_many_workflow_instances_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_many_workflow_instances_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_many_workflow_instances_response& v);
    static ores::workflow::messaging::delete_many_workflow_instances_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_many_workflow_instances_response& v);
    static ores::workflow::messaging::delete_many_workflow_instances_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_instance_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_instance_versions_request& v);
    static ores::workflow::messaging::list_workflow_instance_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_instance_versions_request& v);
    static ores::workflow::messaging::list_workflow_instance_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_instance_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_instance_versions_response& v);
    static ores::workflow::messaging::list_workflow_instance_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_instance_versions_response& v);
    static ores::workflow::messaging::list_workflow_instance_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_instance_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_instance_version_request& v);
    static ores::workflow::messaging::get_workflow_instance_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_instance_version_request& v);
    static ores::workflow::messaging::get_workflow_instance_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_instance_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_instance_version_response& v);
    static ores::workflow::messaging::get_workflow_instance_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_instance_version_response& v);
    static ores::workflow::messaging::get_workflow_instance_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
