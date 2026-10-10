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
#ifndef ORES_INBOX_API_MESSAGING_APPROVAL_POLICY_PROTOCOL_HPP
#define ORES_INBOX_API_MESSAGING_APPROVAL_POLICY_PROTOCOL_HPP

#include "ores.inbox.api/domain/approval_policy.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::inbox::messaging {

struct approval_policy_key {
    std::string code;
};

struct approval_policy_write {
    std::string code;
    std::string name;
    std::string description;
    std::string entity_type;
    std::string operation;
    std::optional<std::string> field_name;
    std::string part_code;
    int display_order;
};

struct approval_policy_change {
    approval_policy_write write;
    ores::utility::domain::precondition precondition;
};

struct approval_policy_removal {
    approval_policy_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct approval_policy_lookup {
    approval_policy_key key;
    std::optional<ores::inbox::domain::approval_policy> approval_policy;
};

struct approval_policies_filter {
    std::optional<std::vector<std::string>> code_one_of;
};

struct approval_policy_event {
    boost::uuids::uuid event_id;
    approval_policy_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct approval_policy_version_key {
    approval_policy_key approval_policy;
    std::uint32_t version;
};

struct approval_policy_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_approval_policies_request {
    using response_type = struct list_approval_policies_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies.list";
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
    std::optional<approval_policies_filter> filter;
    std::optional<std::string> as_of;
};

struct list_approval_policies_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_policy> policies;
    std::uint64_t total;
};

struct get_approval_policy_request {
    using response_type = struct get_approval_policy_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_policy_key key;
};

struct get_approval_policy_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_policy> approval_policy;
};

struct get_many_approval_policies_request {
    using response_type = struct get_many_approval_policies_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_policy_key> keys;
};

struct get_many_approval_policies_response {
    ores::utility::domain::result result;
    std::vector<approval_policy_lookup> entries;
};

struct put_approval_policy_request {
    using response_type = struct put_approval_policy_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_policy_change change;
    ores::utility::domain::change_intent intent;
};

struct put_approval_policy_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_policy> approval_policy;
};

struct put_many_approval_policies_request {
    using response_type = struct put_many_approval_policies_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_policy_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_approval_policies_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_policy> policies;
};

struct delete_approval_policy_request {
    using response_type = struct delete_approval_policy_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_policy_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_approval_policy_response {
    ores::utility::domain::result result;
};

struct delete_many_approval_policies_request {
    using response_type = struct delete_many_approval_policies_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<approval_policy_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_approval_policies_response {
    ores::utility::domain::result result;
};

struct list_approval_policy_versions_request {
    using response_type = struct list_approval_policy_versions_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_policy_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<approval_policy_versions_filter> filter;
};

struct list_approval_policy_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::inbox::domain::approval_policy> versions;
    std::uint64_t total;
};

struct get_approval_policy_version_request {
    using response_type = struct get_approval_policy_version_response;
    static constexpr std::string_view nats_subject = "inbox.v1.approval_policies_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    approval_policy_version_key key;
};

struct get_approval_policy_version_response {
    ores::utility::domain::result result;
    std::optional<ores::inbox::domain::approval_policy> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace approval_policy_event_subjects {
inline constexpr std::string_view created = "inbox.v1.approval_policies_events.created";
inline constexpr std::string_view updated = "inbox.v1.approval_policies_events.updated";
inline constexpr std::string_view deleted = "inbox.v1.approval_policies_events.deleted";
}

}

#endif
