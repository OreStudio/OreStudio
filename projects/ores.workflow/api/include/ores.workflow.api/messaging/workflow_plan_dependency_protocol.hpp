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

#include "ores.utility/domain/protocol.hpp"
#include "ores.workflow.api/domain/workflow_plan_dependency.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::workflow::messaging {

struct workflow_plan_dependency_key {
    boost::uuids::uuid id;
};

struct workflow_plan_dependency_write {
    boost::uuids::uuid id;
    boost::uuids::uuid workflow_id;
    int consumer_step_index;
    int producer_step_index;
};

struct workflow_plan_dependency_change {
    workflow_plan_dependency_write write;
    ores::utility::domain::precondition precondition;
};

struct workflow_plan_dependency_removal {
    workflow_plan_dependency_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct workflow_plan_dependency_lookup {
    workflow_plan_dependency_key key;
    std::optional<ores::workflow::domain::workflow_plan_dependency> workflow_plan_dependency;
};

struct workflow_plan_dependencies_filter {
    std::optional<boost::uuids::uuid> workflow_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> workflow_id_one_of;
};

struct workflow_plan_dependency_event {
    boost::uuids::uuid event_id;
    workflow_plan_dependency_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct workflow_plan_dependency_version_key {
    workflow_plan_dependency_key workflow_plan_dependency;
    std::uint32_t version;
};

struct workflow_plan_dependency_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
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
};

struct list_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_plan_dependency> plan_dependencies;
    std::uint64_t total;
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
};

struct get_workflow_plan_dependency_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_plan_dependency> workflow_plan_dependency;
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
};

struct get_many_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
    std::vector<workflow_plan_dependency_lookup> entries;
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
};

struct put_workflow_plan_dependency_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_plan_dependency> workflow_plan_dependency;
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
};

struct put_many_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_plan_dependency> plan_dependencies;
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
};

struct delete_workflow_plan_dependency_response {
    ores::utility::domain::result result;
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
};

struct delete_many_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
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
};

struct list_by_workflow_id_workflow_plan_dependencies_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_plan_dependency> plan_dependencies;
    std::uint64_t total;
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
};

struct list_workflow_plan_dependency_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::workflow::domain::workflow_plan_dependency> versions;
    std::uint64_t total;
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
};

struct get_workflow_plan_dependency_version_response {
    ores::utility::domain::result result;
    std::optional<ores::workflow::domain::workflow_plan_dependency> version;
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
inline constexpr std::string_view created = "workflow.v1.workflow_plan_dependencies_events.created";
inline constexpr std::string_view updated = "workflow.v1.workflow_plan_dependencies_events.updated";
inline constexpr std::string_view deleted = "workflow.v1.workflow_plan_dependencies_events.deleted";
}

}

#endif
