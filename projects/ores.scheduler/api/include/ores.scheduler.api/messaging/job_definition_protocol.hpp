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
#ifndef ORES_SCHEDULER_API_MESSAGING_JOB_DEFINITION_PROTOCOL_HPP
#define ORES_SCHEDULER_API_MESSAGING_JOB_DEFINITION_PROTOCOL_HPP

#include "ores.scheduler.api/domain/job_definition.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::scheduler::messaging {

struct job_definition_key {
    boost::uuids::uuid id;
};

struct job_definition_write {
    boost::uuids::uuid id;
    std::string job_name;
    std::string description;
    std::string command;
    domain::cron_expression schedule_expression;
    std::string action_type;
    std::string action_payload;
    bool is_active;
};

struct job_definition_change {
    job_definition_write write;
    ores::utility::domain::precondition precondition;
};

struct job_definition_removal {
    job_definition_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct job_definition_lookup {
    job_definition_key key;
    std::optional<ores::scheduler::domain::job_definition> job_definition;
};

struct job_definition_event {
    boost::uuids::uuid event_id;
    job_definition_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct job_definition_version_key {
    job_definition_key job_definition;
    std::uint32_t version;
};

struct job_definition_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_job_definitions_request {
    using response_type = struct list_job_definitions_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions.list";
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

struct list_job_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::job_definition> definitions;
    std::uint64_t total;
};

struct get_job_definition_request {
    using response_type = struct get_job_definition_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    job_definition_key key;
};

struct get_job_definition_response {
    ores::utility::domain::result result;
    std::optional<ores::scheduler::domain::job_definition> job_definition;
};

struct get_many_job_definitions_request {
    using response_type = struct get_many_job_definitions_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<job_definition_key> keys;
};

struct get_many_job_definitions_response {
    ores::utility::domain::result result;
    std::vector<job_definition_lookup> entries;
};

struct put_job_definition_request {
    using response_type = struct put_job_definition_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    job_definition_change change;
    ores::utility::domain::change_intent intent;
};

struct put_job_definition_response {
    ores::utility::domain::result result;
    ores::scheduler::domain::job_definition job_definition;
};

struct put_many_job_definitions_request {
    using response_type = struct put_many_job_definitions_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<job_definition_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_job_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::job_definition> definitions;
};

struct delete_job_definition_request {
    using response_type = struct delete_job_definition_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    job_definition_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_job_definition_response {
    ores::utility::domain::result result;
};

struct delete_many_job_definitions_request {
    using response_type = struct delete_many_job_definitions_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<job_definition_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_job_definitions_response {
    ores::utility::domain::result result;
};

struct list_job_definition_versions_request {
    using response_type = struct list_job_definition_versions_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    job_definition_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<job_definition_versions_filter> filter;
};

struct list_job_definition_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::scheduler::domain::job_definition> versions;
    std::uint64_t total;
};

struct get_job_definition_version_request {
    using response_type = struct get_job_definition_version_response;
    static constexpr std::string_view nats_subject = "scheduler.v1.job_definitions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    job_definition_version_key key;
};

struct get_job_definition_version_response {
    ores::utility::domain::result result;
    ores::scheduler::domain::job_definition version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace job_definition_event_subjects {
inline constexpr std::string_view created = "scheduler.v1.job_definitions_events.created";
inline constexpr std::string_view updated = "scheduler.v1.job_definitions_events.updated";
inline constexpr std::string_view deleted = "scheduler.v1.job_definitions_events.deleted";
}

}

#endif
