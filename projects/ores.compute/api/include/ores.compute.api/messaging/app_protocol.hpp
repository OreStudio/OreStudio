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
#ifndef ORES_COMPUTE_API_MESSAGING_APP_PROTOCOL_HPP
#define ORES_COMPUTE_API_MESSAGING_APP_PROTOCOL_HPP

#include "ores.compute.api/domain/app.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::compute::messaging {

struct app_key {
    std::string name;
};

struct app_write {
    boost::uuids::uuid id;
    std::string name;
    std::string description;
};

struct app_change {
    app_write write;
    ores::utility::domain::precondition precondition;
};

struct app_removal {
    app_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct app_lookup {
    app_key key;
    std::optional<ores::compute::domain::app> app;
};

struct app_event {
    boost::uuids::uuid event_id;
    app_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct apps_version_key {
    app_key app;
    std::uint32_t version;
};

struct apps_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_apps_request {
    using response_type = struct list_apps_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.list";
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

struct list_apps_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app> apps;
    std::uint64_t total;
};

struct get_app_request {
    using response_type = struct get_app_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_key key;
};

struct get_app_response {
    ores::utility::domain::result result;
    std::optional<ores::compute::domain::app> app;
};

struct get_many_apps_request {
    using response_type = struct get_many_apps_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_key> keys;
};

struct get_many_apps_response {
    ores::utility::domain::result result;
    std::vector<app_lookup> entries;
};

struct put_app_request {
    using response_type = struct put_app_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_change change;
    ores::utility::domain::change_intent intent;
};

struct put_app_response {
    ores::utility::domain::result result;
    ores::compute::domain::app app;
};

struct put_many_apps_request {
    using response_type = struct put_many_apps_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_apps_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app> apps;
};

struct delete_app_request {
    using response_type = struct delete_app_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_app_response {
    ores::utility::domain::result result;
};

struct delete_many_apps_request {
    using response_type = struct delete_many_apps_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<app_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_apps_response {
    ores::utility::domain::result result;
};

struct list_apps_versions_request {
    using response_type = struct list_apps_versions_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    app_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<apps_versions_filter> filter;
};

struct list_apps_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::compute::domain::app> versions;
    std::uint64_t total;
};

struct get_apps_version_request {
    using response_type = struct get_apps_version_response;
    static constexpr std::string_view nats_subject = "compute.v1.apps_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    apps_version_key key;
};

struct get_apps_version_response {
    ores::utility::domain::result result;
    ores::compute::domain::app version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace app_event_subjects {
inline constexpr std::string_view created = "compute.v1.apps_events.created";
inline constexpr std::string_view updated = "compute.v1.apps_events.updated";
inline constexpr std::string_view deleted = "compute.v1.apps_events.deleted";
}

}

#endif
