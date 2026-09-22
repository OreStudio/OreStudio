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
#ifndef ORES_IAM_API_MESSAGING_ROLE_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ROLE_PROTOCOL_HPP

#include "ores.iam.api/domain/role.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct role_key {
    std::string name;
};

struct role_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
};

struct role_change {
    role_write write;
    ores::utility::domain::precondition precondition;
};

struct role_removal {
    role_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct role_lookup {
    role_key key;
    std::optional<ores::iam::domain::role> role;
};

struct role_event {
    boost::uuids::uuid event_id;
    role_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct role_version_key {
    role_key role;
    std::uint32_t version;
};

struct role_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_roles_request {
    using response_type = struct list_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.list";
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

struct list_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> roles;
    std::uint64_t total;
};

struct get_role_request {
    using response_type = struct get_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_key key;
};

struct get_role_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::role> role;
};

struct get_many_roles_request {
    using response_type = struct get_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_key> keys;
};

struct get_many_roles_response {
    ores::utility::domain::result result;
    std::vector<role_lookup> entries;
};

struct put_role_request {
    using response_type = struct put_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_change change;
    ores::utility::domain::change_intent intent;
};

struct put_role_response {
    ores::utility::domain::result result;
    ores::iam::domain::role role;
};

struct put_many_roles_request {
    using response_type = struct put_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_roles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> roles;
};

struct delete_role_request {
    using response_type = struct delete_role_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_role_response {
    ores::utility::domain::result result;
};

struct delete_many_roles_request {
    using response_type = struct delete_many_roles_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<role_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_roles_response {
    ores::utility::domain::result result;
};

struct list_role_versions_request {
    using response_type = struct list_role_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<role_versions_filter> filter;
};

struct list_role_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::role> versions;
    std::uint64_t total;
};

struct get_role_version_request {
    using response_type = struct get_role_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.roles_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    role_version_key key;
};

struct get_role_version_response {
    ores::utility::domain::result result;
    ores::iam::domain::role version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace role_event_subjects {
inline constexpr std::string_view created = "iam.v1.roles_events.created";
inline constexpr std::string_view updated = "iam.v1.roles_events.updated";
inline constexpr std::string_view deleted = "iam.v1.roles_events.deleted";
}

}

#endif
