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
#ifndef ORES_WORKSPACE_API_MESSAGING_WORKSPACE_PROTOCOL_HPP
#define ORES_WORKSPACE_API_MESSAGING_WORKSPACE_PROTOCOL_HPP

#include "ores.utility/domain/protocol.hpp"
#include "ores.workspace.api/domain/workspace.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::workspace::messaging {

struct workspace_key {
    boost::uuids::uuid id;
};

struct workspace_write {
    boost::uuids::uuid id;
    std::string name;
    boost::uuids::uuid party_id;
    boost::uuids::uuid owner_id;
    std::string description;
    std::string source_path;
    std::optional<boost::uuids::uuid> parent_workspace_id;
    std::optional<boost::uuids::uuid> scope_portfolio_id;
    std::string status_code;
};

struct workspace_change {
    workspace_write write;
    ores::utility::domain::precondition precondition;
};

struct workspace_removal {
    workspace_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct workspace_lookup {
    workspace_key key;
    std::optional<ores::workspace::domain::workspace> workspace;
};

struct workspaces_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct workspace_event {
    boost::uuids::uuid event_id;
    workspace_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct workspace_version_key {
    workspace_key workspace;
    std::uint32_t version;
};

struct workspace_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_workspaces_request {
    using response_type = struct list_workspaces_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.list";
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
    std::optional<workspaces_filter> filter;
};

struct list_workspaces_response {
    ores::utility::domain::result result;
    std::vector<ores::workspace::domain::workspace> workspaces;
    std::uint64_t total;
};

struct get_workspace_request {
    using response_type = struct get_workspace_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workspace_key key;
};

struct get_workspace_response {
    ores::utility::domain::result result;
    std::optional<ores::workspace::domain::workspace> workspace;
};

struct get_many_workspaces_request {
    using response_type = struct get_many_workspaces_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workspace_key> keys;
};

struct get_many_workspaces_response {
    ores::utility::domain::result result;
    std::vector<workspace_lookup> entries;
};

struct put_workspace_request {
    using response_type = struct put_workspace_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workspace_change change;
    ores::utility::domain::change_intent intent;
};

struct put_workspace_response {
    ores::utility::domain::result result;
    std::optional<ores::workspace::domain::workspace> workspace;
};

struct put_many_workspaces_request {
    using response_type = struct put_many_workspaces_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workspace_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_workspaces_response {
    ores::utility::domain::result result;
    std::vector<ores::workspace::domain::workspace> workspaces;
};

struct delete_workspace_request {
    using response_type = struct delete_workspace_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workspace_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_workspace_response {
    ores::utility::domain::result result;
};

struct delete_many_workspaces_request {
    using response_type = struct delete_many_workspaces_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<workspace_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_workspaces_response {
    ores::utility::domain::result result;
};

struct list_workspace_versions_request {
    using response_type = struct list_workspace_versions_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workspace_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<workspace_versions_filter> filter;
};

struct list_workspace_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::workspace::domain::workspace> versions;
    std::uint64_t total;
};

struct get_workspace_version_request {
    using response_type = struct get_workspace_version_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    workspace_version_key key;
};

struct get_workspace_version_response {
    ores::utility::domain::result result;
    std::optional<ores::workspace::domain::workspace> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace workspace_event_subjects {
inline constexpr std::string_view created = "workspace.v1.workspaces_events.created";
inline constexpr std::string_view updated = "workspace.v1.workspaces_events.updated";
inline constexpr std::string_view deleted = "workspace.v1.workspaces_events.deleted";
}

}

#endif
