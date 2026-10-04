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
#ifndef ORES_REPORTING_API_MESSAGING_CONFIGURATION_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CONFIGURATION_PROTOCOL_HPP

#include "ores.reporting.api/domain/configuration.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct configuration_key {
    std::string name;
};

struct configuration_write {
    boost::uuids::uuid id;
    std::string name;
    std::string configuration_type_code;
    std::string owning_component;
};

struct configuration_change {
    configuration_write write;
    ores::utility::domain::precondition precondition;
};

struct configuration_removal {
    configuration_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct configuration_lookup {
    configuration_key key;
    std::optional<ores::reporting::domain::configuration> configuration;
};

struct configurations_filter {
    std::optional<std::string> configuration_type_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> configuration_type_code_one_of;
};

struct configuration_event {
    boost::uuids::uuid event_id;
    configuration_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct configuration_version_key {
    configuration_key configuration;
    std::uint32_t version;
};

struct configuration_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_configurations_request {
    using response_type = struct list_configurations_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations.list";
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
    std::optional<configurations_filter> filter;
};

struct list_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration> configurations;
    std::uint64_t total;
};

struct get_configuration_request {
    using response_type = struct get_configuration_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_key key;
};

struct get_configuration_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration> configuration;
};

struct get_many_configurations_request {
    using response_type = struct get_many_configurations_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_key> keys;
};

struct get_many_configurations_response {
    ores::utility::domain::result result;
    std::vector<configuration_lookup> entries;
};

struct put_configuration_request {
    using response_type = struct put_configuration_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_change change;
    ores::utility::domain::change_intent intent;
};

struct put_configuration_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration> configuration;
};

struct put_many_configurations_request {
    using response_type = struct put_many_configurations_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration> configurations;
};

struct delete_configuration_request {
    using response_type = struct delete_configuration_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_configuration_response {
    ores::utility::domain::result result;
};

struct delete_many_configurations_request {
    using response_type = struct delete_many_configurations_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_configurations_response {
    ores::utility::domain::result result;
};

struct list_by_configuration_type_code_configurations_request {
    using response_type = struct list_by_configuration_type_code_configurations_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configurations.list_by_configuration_type_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string configuration_type_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configurations_filter> filter;
};

struct list_by_configuration_type_code_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration> configurations;
    std::uint64_t total;
};

struct list_configuration_versions_request {
    using response_type = struct list_configuration_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configuration_versions_filter> filter;
};

struct list_configuration_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration> versions;
    std::uint64_t total;
};

struct get_configuration_version_request {
    using response_type = struct get_configuration_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configurations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_version_key key;
};

struct get_configuration_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace configuration_event_subjects {
inline constexpr std::string_view created = "reporting.v1.configurations_events.created";
inline constexpr std::string_view updated = "reporting.v1.configurations_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.configurations_events.deleted";
}

}

#endif
