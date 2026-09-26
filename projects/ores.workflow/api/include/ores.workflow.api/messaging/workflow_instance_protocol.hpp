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

#include "ores.utility/domain/protocol.hpp"
#include "ores.workflow.api/domain/workflow_instance.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::workflow::messaging {

struct workflow_instance_key {
    boost::uuids::uuid id;
};

struct workflow_instance_write {
    boost::uuids::uuid id;
    std::string type;
    boost::uuids::uuid state_id;
    std::string request_json;
    std::string result_json;
    std::string error;
    std::string correlation_id;
    std::string created_by;
    int current_step_index;
    int step_count;
    std::string materialised_steps_json;
    std::optional<std::chrono::system_clock::time_point> completed_at;
    std::optional<std::chrono::system_clock::time_point> last_event_at;
};

struct workflow_instance_change {
    workflow_instance_write write;
    ores::utility::domain::precondition precondition;
};

struct workflow_instance_removal {
    workflow_instance_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct workflow_instance_lookup {
    workflow_instance_key key;
    std::optional<ores::workflow::domain::workflow_instance> workflow_instance;
};

struct workflow_instance_event {
    boost::uuids::uuid event_id;
    workflow_instance_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct workflow_instance_version_key {
    workflow_instance_key workflow_instance;
    std::uint32_t version;
};

struct workflow_instance_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
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
};

struct list_workflow_instances_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_instance> instances;
    std::uint64_t total;
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
};

struct get_workflow_instance_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_instance> workflow_instance;
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
};

struct get_many_workflow_instances_response {
    ores::utility::domain::result result;
    std::vector<workflow_instance_lookup> entries;
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
};

struct put_workflow_instance_response {
    ores::utility::domain::result result;
    ores::workflow::domain::workflow_instance workflow_instance;
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
};

struct put_many_workflow_instances_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_instance> instances;
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
};

struct delete_workflow_instance_response {
    ores::utility::domain::result result;
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
};

struct delete_many_workflow_instances_response {
    ores::utility::domain::result result;
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
};

struct list_workflow_instance_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_instance> versions;
    std::uint64_t total;
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
};

struct get_workflow_instance_version_response {
    ores::utility::domain::result result;
    ores::workflow::domain::workflow_instance version;
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
inline constexpr std::string_view created = "workflow.v1.workflow_instances_events.created";
inline constexpr std::string_view updated = "workflow.v1.workflow_instances_events.updated";
inline constexpr std::string_view deleted = "workflow.v1.workflow_instances_events.deleted";
}

}

#endif
