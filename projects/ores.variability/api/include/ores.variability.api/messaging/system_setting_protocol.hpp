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
#ifndef ORES_VARIABILITY_API_MESSAGING_SYSTEM_SETTING_PROTOCOL_HPP
#define ORES_VARIABILITY_API_MESSAGING_SYSTEM_SETTING_PROTOCOL_HPP

#include "ores.utility/domain/protocol.hpp"
#include "ores.variability.api/domain/system_setting.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::variability::messaging {

struct system_setting_key {
    std::string name;
};

struct system_setting_write {
    boost::uuids::uuid id;
    std::string name;
    boost::uuids::uuid party_id;
    std::string value;
    std::string data_type;
    std::string description;
};

struct system_setting_change {
    system_setting_write write;
    ores::utility::domain::precondition precondition;
};

struct system_setting_removal {
    system_setting_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct system_setting_lookup {
    system_setting_key key;
    std::optional<ores::variability::domain::system_setting> system_setting;
};

struct system_settings_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct system_setting_event {
    boost::uuids::uuid event_id;
    system_setting_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct system_setting_version_key {
    system_setting_key system_setting;
    std::uint32_t version;
};

struct system_setting_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_system_settings_request {
    using response_type = struct list_system_settings_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.list";
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
    std::optional<system_settings_filter> filter;
    std::optional<std::string> as_of;
};

struct list_system_settings_response {
    ores::utility::domain::result result;
    std::vector<ores::variability::domain::system_setting> settings;
    std::uint64_t total;
};

struct get_system_setting_request {
    using response_type = struct get_system_setting_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_key key;
};

struct get_system_setting_response {
    ores::utility::domain::result result;
    std::optional<ores::variability::domain::system_setting> system_setting;
};

struct get_many_system_settings_request {
    using response_type = struct get_many_system_settings_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<system_setting_key> keys;
};

struct get_many_system_settings_response {
    ores::utility::domain::result result;
    std::vector<system_setting_lookup> entries;
};

struct put_system_setting_request {
    using response_type = struct put_system_setting_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_change change;
    ores::utility::domain::change_intent intent;
};

struct put_system_setting_response {
    ores::utility::domain::result result;
    std::optional<ores::variability::domain::system_setting> system_setting;
};

struct put_many_system_settings_request {
    using response_type = struct put_many_system_settings_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<system_setting_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_system_settings_response {
    ores::utility::domain::result result;
    std::vector<ores::variability::domain::system_setting> settings;
};

struct delete_system_setting_request {
    using response_type = struct delete_system_setting_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_system_setting_response {
    ores::utility::domain::result result;
};

struct delete_many_system_settings_request {
    using response_type = struct delete_many_system_settings_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<system_setting_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_system_settings_response {
    ores::utility::domain::result result;
};

struct list_system_setting_versions_request {
    using response_type = struct list_system_setting_versions_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<system_setting_versions_filter> filter;
};

struct list_system_setting_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::variability::domain::system_setting> versions;
    std::uint64_t total;
};

struct get_system_setting_version_request {
    using response_type = struct get_system_setting_version_response;
    static constexpr std::string_view nats_subject = "variability.v1.system_settings_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    system_setting_version_key key;
};

struct get_system_setting_version_response {
    ores::utility::domain::result result;
    std::optional<ores::variability::domain::system_setting> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace system_setting_event_subjects {
inline constexpr std::string_view created = "variability.v1.system_settings_events.created";
inline constexpr std::string_view updated = "variability.v1.system_settings_events.updated";
inline constexpr std::string_view deleted = "variability.v1.system_settings_events.deleted";
}

}

#endif
