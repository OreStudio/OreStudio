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
#ifndef ORES_WORKFLOW_API_MESSAGING_WORKFLOW_STEP_PROTOCOL_HPP
#define ORES_WORKFLOW_API_MESSAGING_WORKFLOW_STEP_PROTOCOL_HPP

#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include "ores.workflow.api/domain/workflow_step_codec.hpp"
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

struct workflow_step_key {
    boost::uuids::uuid id;

    friend bool operator==(const workflow_step_key&, const workflow_step_key&) = default;
};

struct workflow_step_write {
    boost::uuids::uuid id;
    boost::uuids::uuid workflow_id;
    int step_index{};
    std::string name;
    boost::uuids::uuid state_id;
    std::string request_json;
    std::string response_json;
    std::string error;
    std::string step_log_json;
    std::string command_subject;
    std::string command_json;
    std::string idempotency_key;
    std::string compensation_subject;
    std::string compensation_json;

    friend bool operator==(const workflow_step_write&, const workflow_step_write&) = default;
};

struct workflow_step_change {
    workflow_step_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const workflow_step_change&, const workflow_step_change&) = default;
};

struct workflow_step_removal {
    workflow_step_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const workflow_step_removal&, const workflow_step_removal&) = default;
};

struct workflow_step_lookup {
    workflow_step_key key;
    std::optional<ores::workflow::domain::workflow_step> workflow_step;

    friend bool operator==(const workflow_step_lookup&, const workflow_step_lookup&) = default;
};

struct workflow_steps_filter {
    std::optional<boost::uuids::uuid> workflow_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> workflow_id_one_of;

    friend bool operator==(const workflow_steps_filter&, const workflow_steps_filter&) = default;
};

struct workflow_step_event {
    boost::uuids::uuid event_id;
    workflow_step_key key;
    std::string action;
    std::uint32_t version{};
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const workflow_step_event&, const workflow_step_event&) = default;
};

struct workflow_step_version_key {
    workflow_step_key workflow_step;
    std::uint32_t version{};

    friend bool operator==(const workflow_step_version_key&,
                           const workflow_step_version_key&) = default;
};

struct workflow_step_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;

    friend bool operator==(const workflow_step_versions_filter&,
                           const workflow_step_versions_filter&) = default;
};

struct list_workflow_steps_request {
    using response_type = struct list_workflow_steps_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps.list";
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
    std::optional<workflow_steps_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_workflow_steps_request&,
                           const list_workflow_steps_request&) = default;
};

struct list_workflow_steps_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_step> steps;
    std::uint64_t total{};

    friend bool operator==(const list_workflow_steps_response&,
                           const list_workflow_steps_response&) = default;
};

struct get_workflow_step_request {
    using response_type = struct get_workflow_step_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_step_key key;

    friend bool operator==(const get_workflow_step_request&,
                           const get_workflow_step_request&) = default;
};

struct get_workflow_step_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_step> workflow_step;

    friend bool operator==(const get_workflow_step_response&,
                           const get_workflow_step_response&) = default;
};

struct get_many_workflow_steps_request {
    using response_type = struct get_many_workflow_steps_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_step_key> keys;

    friend bool operator==(const get_many_workflow_steps_request&,
                           const get_many_workflow_steps_request&) = default;
};

struct get_many_workflow_steps_response {
    ores::utility::domain::result result;
    std::vector<workflow_step_lookup> entries;

    friend bool operator==(const get_many_workflow_steps_response&,
                           const get_many_workflow_steps_response&) = default;
};

struct put_workflow_step_request {
    using response_type = struct put_workflow_step_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_step_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_workflow_step_request&,
                           const put_workflow_step_request&) = default;
};

struct put_workflow_step_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_step> workflow_step;

    friend bool operator==(const put_workflow_step_response&,
                           const put_workflow_step_response&) = default;
};

struct put_many_workflow_steps_request {
    using response_type = struct put_many_workflow_steps_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_step_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_workflow_steps_request&,
                           const put_many_workflow_steps_request&) = default;
};

struct put_many_workflow_steps_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_step> steps;

    friend bool operator==(const put_many_workflow_steps_response&,
                           const put_many_workflow_steps_response&) = default;
};

struct delete_workflow_step_request {
    using response_type = struct delete_workflow_step_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_step_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_workflow_step_request&,
                           const delete_workflow_step_request&) = default;
};

struct delete_workflow_step_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_workflow_step_response&,
                           const delete_workflow_step_response&) = default;
};

struct delete_many_workflow_steps_request {
    using response_type = struct delete_many_workflow_steps_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workflow_step_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_workflow_steps_request&,
                           const delete_many_workflow_steps_request&) = default;
};

struct delete_many_workflow_steps_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_workflow_steps_response&,
                           const delete_many_workflow_steps_response&) = default;
};

struct list_by_workflow_id_workflow_steps_request {
    using response_type = struct list_by_workflow_id_workflow_steps_response;
    static constexpr std::string_view nats_subject =
        "workflow.v1.workflow_steps.list_by_workflow_id";
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
    std::optional<workflow_steps_filter> filter;

    friend bool operator==(const list_by_workflow_id_workflow_steps_request&,
                           const list_by_workflow_id_workflow_steps_request&) = default;
};

struct list_by_workflow_id_workflow_steps_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_step> steps;
    std::uint64_t total{};

    friend bool operator==(const list_by_workflow_id_workflow_steps_response&,
                           const list_by_workflow_id_workflow_steps_response&) = default;
};

struct list_workflow_step_versions_request {
    using response_type = struct list_workflow_step_versions_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_step_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<workflow_step_versions_filter> filter;

    friend bool operator==(const list_workflow_step_versions_request&,
                           const list_workflow_step_versions_request&) = default;
};

struct list_workflow_step_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_step> versions;
    std::uint64_t total{};

    friend bool operator==(const list_workflow_step_versions_response&,
                           const list_workflow_step_versions_response&) = default;
};

struct get_workflow_step_version_request {
    using response_type = struct get_workflow_step_version_response;
    static constexpr std::string_view nats_subject = "workflow.v1.workflow_steps_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workflow_step_version_key key;

    friend bool operator==(const get_workflow_step_version_request&,
                           const get_workflow_step_version_request&) = default;
};

struct get_workflow_step_version_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_step> version;

    friend bool operator==(const get_workflow_step_version_response&,
                           const get_workflow_step_version_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace workflow_step_event_subjects {
inline constexpr std::string_view created = "workflow.v1.events.workflow_steps.created";
inline constexpr std::string_view updated = "workflow.v1.events.workflow_steps.updated";
inline constexpr std::string_view deleted = "workflow.v1.events.workflow_steps.deleted";
}
/**
 * @brief The members of workflow_step_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &workflow_step_key::id},
    };
}

/**
 * @brief The members of workflow_step_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step_write>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"id", &workflow_step_write::id},
        utility::codec::field{"workflow_id", &workflow_step_write::workflow_id},
        utility::codec::field{"step_index", &workflow_step_write::step_index},
        utility::codec::field{"name", &workflow_step_write::name},
        utility::codec::field{"state_id", &workflow_step_write::state_id},
        utility::codec::field{"request_json", &workflow_step_write::request_json},
        utility::codec::field{"response_json", &workflow_step_write::response_json},
        utility::codec::field{"error", &workflow_step_write::error},
        utility::codec::field{"step_log_json", &workflow_step_write::step_log_json},
        utility::codec::field{"command_subject", &workflow_step_write::command_subject},
        utility::codec::field{"command_json", &workflow_step_write::command_json},
        utility::codec::field{"idempotency_key", &workflow_step_write::idempotency_key},
        utility::codec::field{"compensation_subject", &workflow_step_write::compensation_subject},
        utility::codec::field{"compensation_json", &workflow_step_write::compensation_json},
    };
}

/**
 * @brief The members of workflow_step_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step_change>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"write", &workflow_step_change::write},
        utility::codec::field{"precondition", &workflow_step_change::precondition},
    };
}

/**
 * @brief The members of workflow_step_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step_removal>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &workflow_step_removal::key},
        utility::codec::field{"precondition", &workflow_step_removal::precondition},
    };
}

/**
 * @brief The members of workflow_step_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step_lookup>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &workflow_step_lookup::key},
        utility::codec::field{"workflow_step", &workflow_step_lookup::workflow_step},
    };
}

/**
 * @brief The members of workflow_steps_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_steps_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"workflow_id", &workflow_steps_filter::workflow_id},
        utility::codec::field{"id_one_of", &workflow_steps_filter::id_one_of},
        utility::codec::field{"workflow_id_one_of", &workflow_steps_filter::workflow_id_one_of},
    };
}

/**
 * @brief The members of workflow_step_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step_event>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"event_id", &workflow_step_event::event_id},
        utility::codec::field{"key", &workflow_step_event::key},
        utility::codec::field{"action", &workflow_step_event::action},
        utility::codec::field{"version", &workflow_step_event::version},
        utility::codec::field{"occurred_at", &workflow_step_event::occurred_at},
        utility::codec::field{"correlation_id", &workflow_step_event::correlation_id},
    };
}

/**
 * @brief The members of workflow_step_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step_version_key>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"workflow_step", &workflow_step_version_key::workflow_step},
        utility::codec::field{"version", &workflow_step_version_key::version},
    };
}

/**
 * @brief The members of workflow_step_versions_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<workflow_step_versions_filter>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"version", &workflow_step_versions_filter::version},
        utility::codec::field{"from_version", &workflow_step_versions_filter::from_version},
        utility::codec::field{"to_version", &workflow_step_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_workflow_steps_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_workflow_steps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"offset", &list_workflow_steps_request::offset},
        utility::codec::field{"limit", &list_workflow_steps_request::limit},
        utility::codec::field{"order", &list_workflow_steps_request::order},
        utility::codec::field{"filter", &list_workflow_steps_request::filter},
        utility::codec::field{"as_of", &list_workflow_steps_request::as_of},
    };
}

/**
 * @brief The members of list_workflow_steps_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_workflow_steps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_workflow_steps_response::result},
        utility::codec::field{"steps", &list_workflow_steps_response::steps},
        utility::codec::field{"total", &list_workflow_steps_response::total},
    };
}

/**
 * @brief The members of get_workflow_step_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_workflow_step_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_workflow_step_request::key},
    };
}

/**
 * @brief The members of get_workflow_step_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_workflow_step_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_workflow_step_response::result},
        utility::codec::field{"workflow_step", &get_workflow_step_response::workflow_step},
    };
}

/**
 * @brief The members of get_many_workflow_steps_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_workflow_steps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"keys", &get_many_workflow_steps_request::keys},
    };
}

/**
 * @brief The members of get_many_workflow_steps_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_workflow_steps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_many_workflow_steps_response::result},
        utility::codec::field{"entries", &get_many_workflow_steps_response::entries},
    };
}

/**
 * @brief The members of put_workflow_step_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_workflow_step_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"change", &put_workflow_step_request::change},
        utility::codec::field{"intent", &put_workflow_step_request::intent},
    };
}

/**
 * @brief The members of put_workflow_step_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_workflow_step_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_workflow_step_response::result},
        utility::codec::field{"workflow_step", &put_workflow_step_response::workflow_step},
    };
}

/**
 * @brief The members of put_many_workflow_steps_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_workflow_steps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"changes", &put_many_workflow_steps_request::changes},
        utility::codec::field{"intent", &put_many_workflow_steps_request::intent},
    };
}

/**
 * @brief The members of put_many_workflow_steps_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_workflow_steps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &put_many_workflow_steps_response::result},
        utility::codec::field{"steps", &put_many_workflow_steps_response::steps},
    };
}

/**
 * @brief The members of delete_workflow_step_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_workflow_step_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removal", &delete_workflow_step_request::removal},
        utility::codec::field{"intent", &delete_workflow_step_request::intent},
    };
}

/**
 * @brief The members of delete_workflow_step_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_workflow_step_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_workflow_step_response::result},
    };
}

/**
 * @brief The members of delete_many_workflow_steps_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_workflow_steps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"removals", &delete_many_workflow_steps_request::removals},
        utility::codec::field{"intent", &delete_many_workflow_steps_request::intent},
    };
}

/**
 * @brief The members of delete_many_workflow_steps_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_workflow_steps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &delete_many_workflow_steps_response::result},
    };
}

/**
 * @brief The members of list_by_workflow_id_workflow_steps_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_workflow_id_workflow_steps_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"workflow_id",
                              &list_by_workflow_id_workflow_steps_request::workflow_id},
        utility::codec::field{"scope", &list_by_workflow_id_workflow_steps_request::scope},
        utility::codec::field{"offset", &list_by_workflow_id_workflow_steps_request::offset},
        utility::codec::field{"limit", &list_by_workflow_id_workflow_steps_request::limit},
        utility::codec::field{"order", &list_by_workflow_id_workflow_steps_request::order},
        utility::codec::field{"filter", &list_by_workflow_id_workflow_steps_request::filter},
    };
}

/**
 * @brief The members of list_by_workflow_id_workflow_steps_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_by_workflow_id_workflow_steps_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_by_workflow_id_workflow_steps_response::result},
        utility::codec::field{"steps", &list_by_workflow_id_workflow_steps_response::steps},
        utility::codec::field{"total", &list_by_workflow_id_workflow_steps_response::total},
    };
}

/**
 * @brief The members of list_workflow_step_versions_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_workflow_step_versions_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &list_workflow_step_versions_request::key},
        utility::codec::field{"offset", &list_workflow_step_versions_request::offset},
        utility::codec::field{"limit", &list_workflow_step_versions_request::limit},
        utility::codec::field{"order", &list_workflow_step_versions_request::order},
        utility::codec::field{"filter", &list_workflow_step_versions_request::filter},
    };
}

/**
 * @brief The members of list_workflow_step_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_workflow_step_versions_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &list_workflow_step_versions_response::result},
        utility::codec::field{"versions", &list_workflow_step_versions_response::versions},
        utility::codec::field{"total", &list_workflow_step_versions_response::total},
    };
}

/**
 * @brief The members of get_workflow_step_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_workflow_step_version_request>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"key", &get_workflow_step_version_request::key},
    };
}

/**
 * @brief The members of get_workflow_step_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_workflow_step_version_response>) {
    return ::ores::utility::codec::field_list{
        utility::codec::field{"result", &get_workflow_step_version_response::result},
        utility::codec::field{"version", &get_workflow_step_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_step_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_step_key& v);
    static ores::workflow::messaging::workflow_step_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_step_key& v);
    static ores::workflow::messaging::workflow_step_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_step_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_step_write& v);
    static ores::workflow::messaging::workflow_step_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_step_write& v);
    static ores::workflow::messaging::workflow_step_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_step_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_step_change& v);
    static ores::workflow::messaging::workflow_step_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_step_change& v);
    static ores::workflow::messaging::workflow_step_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_step_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_step_removal& v);
    static ores::workflow::messaging::workflow_step_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_step_removal& v);
    static ores::workflow::messaging::workflow_step_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_step_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_step_lookup& v);
    static ores::workflow::messaging::workflow_step_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_step_lookup& v);
    static ores::workflow::messaging::workflow_step_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_steps_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_steps_filter& v);
    static ores::workflow::messaging::workflow_steps_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_steps_filter& v);
    static ores::workflow::messaging::workflow_steps_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_step_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_step_event& v);
    static ores::workflow::messaging::workflow_step_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_step_event& v);
    static ores::workflow::messaging::workflow_step_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::workflow_step_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::workflow_step_version_key& v);
    static ores::workflow::messaging::workflow_step_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::workflow_step_version_key& v);
    static ores::workflow::messaging::workflow_step_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::workflow_step_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::workflow_step_versions_filter& v);
    static ores::workflow::messaging::workflow_step_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::workflow_step_versions_filter& v);
    static ores::workflow::messaging::workflow_step_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_steps_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::list_workflow_steps_request& v);
    static ores::workflow::messaging::list_workflow_steps_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_steps_request& v);
    static ores::workflow::messaging::list_workflow_steps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_steps_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::list_workflow_steps_response& v);
    static ores::workflow::messaging::list_workflow_steps_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_steps_response& v);
    static ores::workflow::messaging::list_workflow_steps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::get_workflow_step_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::get_workflow_step_request& v);
    static ores::workflow::messaging::get_workflow_step_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::get_workflow_step_request& v);
    static ores::workflow::messaging::get_workflow_step_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::get_workflow_step_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::get_workflow_step_response& v);
    static ores::workflow::messaging::get_workflow_step_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_step_response& v);
    static ores::workflow::messaging::get_workflow_step_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_many_workflow_steps_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_many_workflow_steps_request& v);
    static ores::workflow::messaging::get_many_workflow_steps_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_many_workflow_steps_request& v);
    static ores::workflow::messaging::get_many_workflow_steps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_many_workflow_steps_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_many_workflow_steps_response& v);
    static ores::workflow::messaging::get_many_workflow_steps_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_many_workflow_steps_response& v);
    static ores::workflow::messaging::get_many_workflow_steps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::put_workflow_step_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::put_workflow_step_request& v);
    static ores::workflow::messaging::put_workflow_step_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::workflow::messaging::put_workflow_step_request& v);
    static ores::workflow::messaging::put_workflow_step_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT wire_traits<ores::workflow::messaging::put_workflow_step_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::put_workflow_step_response& v);
    static ores::workflow::messaging::put_workflow_step_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_workflow_step_response& v);
    static ores::workflow::messaging::put_workflow_step_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_many_workflow_steps_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_many_workflow_steps_request& v);
    static ores::workflow::messaging::put_many_workflow_steps_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_many_workflow_steps_request& v);
    static ores::workflow::messaging::put_many_workflow_steps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::put_many_workflow_steps_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::put_many_workflow_steps_response& v);
    static ores::workflow::messaging::put_many_workflow_steps_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::put_many_workflow_steps_response& v);
    static ores::workflow::messaging::put_many_workflow_steps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_workflow_step_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::workflow::messaging::delete_workflow_step_request& v);
    static ores::workflow::messaging::delete_workflow_step_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_workflow_step_request& v);
    static ores::workflow::messaging::delete_workflow_step_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_workflow_step_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_workflow_step_response& v);
    static ores::workflow::messaging::delete_workflow_step_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_workflow_step_response& v);
    static ores::workflow::messaging::delete_workflow_step_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_many_workflow_steps_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_many_workflow_steps_request& v);
    static ores::workflow::messaging::delete_many_workflow_steps_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_many_workflow_steps_request& v);
    static ores::workflow::messaging::delete_many_workflow_steps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::delete_many_workflow_steps_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::delete_many_workflow_steps_response& v);
    static ores::workflow::messaging::delete_many_workflow_steps_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::delete_many_workflow_steps_response& v);
    static ores::workflow::messaging::delete_many_workflow_steps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_by_workflow_id_workflow_steps_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_by_workflow_id_workflow_steps_request& v);
    static ores::workflow::messaging::list_by_workflow_id_workflow_steps_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_by_workflow_id_workflow_steps_request& v);
    static ores::workflow::messaging::list_by_workflow_id_workflow_steps_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_by_workflow_id_workflow_steps_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_by_workflow_id_workflow_steps_response& v);
    static ores::workflow::messaging::list_by_workflow_id_workflow_steps_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_by_workflow_id_workflow_steps_response& v);
    static ores::workflow::messaging::list_by_workflow_id_workflow_steps_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_step_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_step_versions_request& v);
    static ores::workflow::messaging::list_workflow_step_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_step_versions_request& v);
    static ores::workflow::messaging::list_workflow_step_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::list_workflow_step_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::list_workflow_step_versions_response& v);
    static ores::workflow::messaging::list_workflow_step_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::list_workflow_step_versions_response& v);
    static ores::workflow::messaging::list_workflow_step_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_step_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_step_version_request& v);
    static ores::workflow::messaging::get_workflow_step_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_step_version_request& v);
    static ores::workflow::messaging::get_workflow_step_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_WORKFLOW_API_EXPORT
    wire_traits<ores::workflow::messaging::get_workflow_step_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::workflow::messaging::get_workflow_step_version_response& v);
    static ores::workflow::messaging::get_workflow_step_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::workflow::messaging::get_workflow_step_version_response& v);
    static ores::workflow::messaging::get_workflow_step_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
