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
#ifndef ORES_REPORTING_API_MESSAGING_CONCURRENCY_POLICY_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CONCURRENCY_POLICY_PROTOCOL_HPP

#include "ores.reporting.api/domain/concurrency_policy.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct concurrency_policy_key {
    std::string code;
};

struct concurrency_policy_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
};

struct concurrency_policy_change {
    concurrency_policy_write write;
    ores::utility::domain::precondition precondition;
};

struct concurrency_policy_removal {
    concurrency_policy_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct concurrency_policy_lookup {
    concurrency_policy_key key;
    std::optional<ores::reporting::domain::concurrency_policy> concurrency_policy;
};

struct concurrency_policies_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct concurrency_policy_event {
    boost::uuids::uuid event_id;
    concurrency_policy_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct concurrency_policy_version_key {
    concurrency_policy_key concurrency_policy;
    std::uint32_t version;
};

struct concurrency_policy_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_concurrency_policies_request {
    using response_type = struct list_concurrency_policies_response;
    static constexpr std::string_view nats_subject = "reporting.v1.concurrency_policies.list";
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
    std::optional<concurrency_policies_filter> filter;
    std::optional<std::string> as_of;
};

struct list_concurrency_policies_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::concurrency_policy> policies;
    std::uint64_t total;
};

struct get_concurrency_policy_request {
    using response_type = struct get_concurrency_policy_response;
    static constexpr std::string_view nats_subject = "reporting.v1.concurrency_policies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    concurrency_policy_key key;
};

struct get_concurrency_policy_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::concurrency_policy> concurrency_policy;
};

struct get_many_concurrency_policies_request {
    using response_type = struct get_many_concurrency_policies_response;
    static constexpr std::string_view nats_subject = "reporting.v1.concurrency_policies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<concurrency_policy_key> keys;
};

struct get_many_concurrency_policies_response {
    ores::utility::domain::result result;
    std::vector<concurrency_policy_lookup> entries;
};

struct put_concurrency_policy_request {
    using response_type = struct put_concurrency_policy_response;
    static constexpr std::string_view nats_subject = "reporting.v1.concurrency_policies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    concurrency_policy_change change;
    ores::utility::domain::change_intent intent;
};

struct put_concurrency_policy_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::concurrency_policy> concurrency_policy;
};

struct put_many_concurrency_policies_request {
    using response_type = struct put_many_concurrency_policies_response;
    static constexpr std::string_view nats_subject = "reporting.v1.concurrency_policies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<concurrency_policy_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_concurrency_policies_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::concurrency_policy> policies;
};

struct delete_concurrency_policy_request {
    using response_type = struct delete_concurrency_policy_response;
    static constexpr std::string_view nats_subject = "reporting.v1.concurrency_policies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    concurrency_policy_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_concurrency_policy_response {
    ores::utility::domain::result result;
};

struct delete_many_concurrency_policies_request {
    using response_type = struct delete_many_concurrency_policies_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.concurrency_policies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<concurrency_policy_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_concurrency_policies_response {
    ores::utility::domain::result result;
};

struct list_concurrency_policy_versions_request {
    using response_type = struct list_concurrency_policy_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.concurrency_policies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    concurrency_policy_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<concurrency_policy_versions_filter> filter;
};

struct list_concurrency_policy_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::concurrency_policy> versions;
    std::uint64_t total;
};

struct get_concurrency_policy_version_request {
    using response_type = struct get_concurrency_policy_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.concurrency_policies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    concurrency_policy_version_key key;
};

struct get_concurrency_policy_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::concurrency_policy> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace concurrency_policy_event_subjects {
inline constexpr std::string_view created = "reporting.v1.concurrency_policies_events.created";
inline constexpr std::string_view updated = "reporting.v1.concurrency_policies_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.concurrency_policies_events.deleted";
}

}

#endif
