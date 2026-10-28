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
#ifndef ORES_REPORTING_API_MESSAGING_ENGINE_SETTINGS_CHANGE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_ENGINE_SETTINGS_CHANGE_PROTOCOL_HPP

#include "ores.reporting.api/domain/engine_settings_change.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct engine_settings_change_key {
    boost::uuids::uuid id;
};

struct engine_settings_change_lookup {
    engine_settings_change_key key;
    std::optional<ores::reporting::domain::engine_settings_change> engine_settings_change;
};

struct engine_settings_changes_filter {
    std::optional<boost::uuids::uuid> request_id;
    std::optional<boost::uuids::uuid> engine_settings_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> request_id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> engine_settings_id_one_of;
};

struct engine_settings_change_event {
    boost::uuids::uuid event_id;
    engine_settings_change_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct engine_settings_change_version_key {
    engine_settings_change_key engine_settings_change;
    std::uint32_t version;
};

struct engine_settings_change_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_engine_settings_changes_request {
    using response_type = struct list_engine_settings_changes_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_changes.list";
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
    std::optional<engine_settings_changes_filter> filter;
    std::optional<std::string> as_of;
};

struct list_engine_settings_changes_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_change> engine_settings_changes;
    std::uint64_t total;
};

struct get_engine_settings_change_request {
    using response_type = struct get_engine_settings_change_response;
    static constexpr std::string_view nats_subject = "reporting.v1.engine_settings_changes.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_change_key key;
};

struct get_engine_settings_change_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::engine_settings_change> engine_settings_change;
};

struct get_many_engine_settings_changes_request {
    using response_type = struct get_many_engine_settings_changes_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_changes.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<engine_settings_change_key> keys;
};

struct get_many_engine_settings_changes_response {
    ores::utility::domain::result result;
    std::vector<engine_settings_change_lookup> entries;
};

struct list_by_request_id_engine_settings_changes_request {
    using response_type = struct list_by_request_id_engine_settings_changes_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_changes.list_by_request_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid request_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<engine_settings_changes_filter> filter;
};

struct list_by_request_id_engine_settings_changes_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_change> engine_settings_changes;
    std::uint64_t total;
};

struct list_by_engine_settings_id_engine_settings_changes_request {
    using response_type = struct list_by_engine_settings_id_engine_settings_changes_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_changes.list_by_engine_settings_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid engine_settings_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<engine_settings_changes_filter> filter;
};

struct list_by_engine_settings_id_engine_settings_changes_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_change> engine_settings_changes;
    std::uint64_t total;
};

struct list_engine_settings_change_versions_request {
    using response_type = struct list_engine_settings_change_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_changes_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_change_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<engine_settings_change_versions_filter> filter;
};

struct list_engine_settings_change_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::engine_settings_change> versions;
    std::uint64_t total;
};

struct get_engine_settings_change_version_request {
    using response_type = struct get_engine_settings_change_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.engine_settings_changes_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    engine_settings_change_version_key key;
};

struct get_engine_settings_change_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::engine_settings_change> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace engine_settings_change_event_subjects {
inline constexpr std::string_view created = "reporting.v1.engine_settings_changes_events.created";
inline constexpr std::string_view updated = "reporting.v1.engine_settings_changes_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.engine_settings_changes_events.deleted";
}

}

#endif
