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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_INSTANCE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_INSTANCE_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_instance.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct report_instance_key {
    std::string name;
};

struct report_instance_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
    boost::uuids::uuid definition_id;
    std::string kind;
    std::optional<boost::uuids::uuid> run_grant_id;
    std::optional<boost::uuids::uuid> fsm_state_id;
    boost::uuids::uuid trigger_id;
    std::string output_message;
    std::string output_storage_key;
};

struct report_instance_change {
    report_instance_write write;
    ores::utility::domain::precondition precondition;
};

struct report_instance_removal {
    report_instance_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct report_instance_lookup {
    report_instance_key key;
    std::optional<ores::reporting::domain::report_instance> report_instance;
};

struct report_instances_filter {
    std::optional<boost::uuids::uuid> definition_id;
    std::optional<std::string> kind;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> definition_id_one_of;
    std::optional<std::vector<std::string>> kind_one_of;
};

struct report_instance_event {
    boost::uuids::uuid event_id;
    report_instance_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct report_instance_version_key {
    report_instance_key report_instance;
    std::uint32_t version;
};

struct report_instance_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_report_instances_request {
    using response_type = struct list_report_instances_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.list";
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
    std::optional<report_instances_filter> filter;
    std::optional<std::string> as_of;
};

struct list_report_instances_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_instance> instances;
    std::uint64_t total;
};

struct get_report_instance_request {
    using response_type = struct get_report_instance_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_key key;
};

struct get_report_instance_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_instance> report_instance;
};

struct get_many_report_instances_request {
    using response_type = struct get_many_report_instances_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_instance_key> keys;
};

struct get_many_report_instances_response {
    ores::utility::domain::result result;
    std::vector<report_instance_lookup> entries;
};

struct put_report_instance_request {
    using response_type = struct put_report_instance_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_change change;
    ores::utility::domain::change_intent intent;
};

struct put_report_instance_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_instance> report_instance;
};

struct put_many_report_instances_request {
    using response_type = struct put_many_report_instances_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_instance_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_report_instances_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_instance> instances;
};

struct delete_report_instance_request {
    using response_type = struct delete_report_instance_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_report_instance_response {
    ores::utility::domain::result result;
};

struct delete_many_report_instances_request {
    using response_type = struct delete_many_report_instances_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_instance_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_report_instances_response {
    ores::utility::domain::result result;
};

struct list_report_instance_versions_request {
    using response_type = struct list_report_instance_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_instance_versions_filter> filter;
};

struct list_report_instance_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_instance> versions;
    std::uint64_t total;
};

struct get_report_instance_version_request {
    using response_type = struct get_report_instance_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_instances_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_instance_version_key key;
};

struct get_report_instance_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_instance> version;
};

/**
 * @brief Reads the run of each named definition that started last.
 *
 * The caller asks once with the ids a read of the definitions returned, so it
 * pays for one row per definition. The request names no party: the session
 * scopes the answer through the row-level security on the table.
 */
struct get_latest_report_instances_by_definition_request {
    using response_type = struct get_latest_report_instances_by_definition_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ops.get_latest_report_instances_by_definition";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The definitions to answer for, in any order. */
    std::vector<boost::uuids::uuid> definition_ids;
};

struct get_latest_report_instances_by_definition_response {
    ores::utility::domain::result result;
    /**
     * @brief At most one run per definition asked for, and no entry for a
     * definition that has never run.
     */
    std::vector<ores::reporting::domain::report_instance> instances;
};

struct report_instance_health {
    /** @brief The run the word is of. */
    boost::uuids::uuid report_instance_id;
    /**
     * @brief The name of the run's lifecycle state, as the store holds it.
     *
     * Absent when the run has no state, or when the session cannot read the
     * state: the word is then absent too, and the screen is told what the word
     * was derived from rather than left guessing.
     */
    std::string state_name;
    /**
     * @brief One of =failed=, =completed with errors=, =running= or
     * =completed clean=, or empty when the state has no word.
     *
     * =never run= is not a word here: a definition that never ran has no run,
     * so it has no entry in the reply.
     */
    std::string health;
};

/**
 * @brief Reads the health word of each named run.
 *
 * The caller asks once with the runs a read of the latest runs returned, so it
 * pays for one row per run. The request names no party: the session scopes the
 * answer through the row-level security on the tables.
 */
struct get_report_instance_health_request {
    using response_type = struct get_report_instance_health_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ops.get_report_instance_health";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** @brief The runs to state the health of, in any order. */
    std::vector<boost::uuids::uuid> instance_ids;
};

struct get_report_instance_health_response {
    ores::utility::domain::result result;
    /**
     * @brief One entry per run the session can read, and no entry for a run
     * that does not exist or is not the caller's to read.
     */
    std::vector<report_instance_health> health;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_instance_event_subjects {
inline constexpr std::string_view created = "reporting.v1.report_instances_events.created";
inline constexpr std::string_view updated = "reporting.v1.report_instances_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.report_instances_events.deleted";
}

}

#endif
