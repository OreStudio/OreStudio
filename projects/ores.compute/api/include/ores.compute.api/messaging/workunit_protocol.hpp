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
#ifndef ORES_COMPUTE_API_MESSAGING_WORKUNIT_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_WORKUNIT_PROTOCOL_HPP

#include "ores.compute.api/domain/workunit.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct workunit_key {
    boost::uuids::uuid id;
};

struct workunit_write {
    boost::uuids::uuid id;
    boost::uuids::uuid batch_id;
    boost::uuids::uuid app_version_id;
    std::string input_uri;
    std::string config_uri;
    int priority;
    int target_redundancy;
    boost::uuids::uuid canonical_result_id;
};

struct workunit_change {
    workunit_write write;
    ores::utility::domain::precondition precondition;
};

struct workunit_removal {
    workunit_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct workunit_lookup {
    workunit_key key;
    std::optional<ores::compute::domain::workunit> workunit;
};

struct workunits_filter {
    std::optional<boost::uuids::uuid> batch_id;
};

struct workunit_event {
    boost::uuids::uuid event_id;
    workunit_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct workunit_version_key {
    workunit_key workunit;
    std::uint32_t version;
};

struct workunit_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_workunits_request {
    using response_type = struct list_workunits_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.list";
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
    std::optional<workunits_filter> filter;
};

struct list_workunits_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::workunit> workunits;
    std::uint64_t total;
};

struct get_workunit_request {
    using response_type = struct get_workunit_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workunit_key key;
};

struct get_workunit_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::workunit> workunit;
};

struct get_many_workunits_request {
    using response_type = struct get_many_workunits_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workunit_key> keys;
};

struct get_many_workunits_response {
    ores::utility::domain::result result;
    std::vector<workunit_lookup> entries;
};

struct put_workunit_request {
    using response_type = struct put_workunit_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workunit_change change;
    ores::utility::domain::change_intent intent;
};

struct put_workunit_response {
    ores::utility::domain::result result;
    ores::compute::domain::workunit workunit;
};

struct put_many_workunits_request {
    using response_type = struct put_many_workunits_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workunit_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_workunits_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::workunit> workunits;
};

struct delete_workunit_request {
    using response_type = struct delete_workunit_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workunit_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_workunit_response {
    ores::utility::domain::result result;
};

struct delete_many_workunits_request {
    using response_type = struct delete_many_workunits_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workunit_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_workunits_response {
    ores::utility::domain::result result;
};

struct list_by_batch_id_workunits_request {
    using response_type = struct list_by_batch_id_workunits_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits.list_by_batch_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid batch_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<workunits_filter> filter;
};

struct list_by_batch_id_workunits_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::workunit> workunits;
    std::uint64_t total;
};

struct list_workunit_versions_request {
    using response_type = struct list_workunit_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workunit_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<workunit_versions_filter> filter;
};

struct list_workunit_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::workunit> versions;
    std::uint64_t total;
};

struct get_workunit_version_request {
    using response_type = struct get_workunit_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.workunits_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workunit_version_key key;
};

struct get_workunit_version_response {
    ores::utility::domain::result result;
    ores::compute::domain::workunit version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace workunit_event_subjects {
inline constexpr std::string_view created = "compute.v1.workunits_events.created";
inline constexpr std::string_view updated = "compute.v1.workunits_events.updated";
inline constexpr std::string_view deleted = "compute.v1.workunits_events.deleted";
}

}

#endif
