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
#ifndef ORES_REPORTING_API_MESSAGING_CONFIGURATION_PARAMETER_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_CONFIGURATION_PARAMETER_PROTOCOL_HPP

#include "ores.reporting.api/domain/configuration_parameter.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct configuration_parameter_key {
    std::string value;
};

struct configuration_parameter_write {
    boost::uuids::uuid id;
    boost::uuids::uuid configuration_id;
    boost::uuids::uuid parameter_definition_id;
    std::string value;
    int position;
};

struct configuration_parameter_change {
    configuration_parameter_write write;
    ores::utility::domain::precondition precondition;
};

struct configuration_parameter_removal {
    configuration_parameter_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct configuration_parameter_lookup {
    configuration_parameter_key key;
    std::optional<ores::reporting::domain::configuration_parameter> configuration_parameter;
};

struct configuration_parameters_filter {
    std::optional<boost::uuids::uuid> configuration_id;
    std::optional<boost::uuids::uuid> parameter_definition_id;
};

struct configuration_parameter_event {
    boost::uuids::uuid event_id;
    configuration_parameter_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct configuration_parameter_version_key {
    configuration_parameter_key configuration_parameter;
    std::uint32_t version;
};

struct configuration_parameter_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_configuration_parameters_request {
    using response_type = struct list_configuration_parameters_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configuration_parameters.list";
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
    std::optional<configuration_parameters_filter> filter;
};

struct list_configuration_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_parameter> parameter_values;
    std::uint64_t total;
};

struct get_configuration_parameter_request {
    using response_type = struct get_configuration_parameter_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configuration_parameters.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_parameter_key key;
};

struct get_configuration_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_parameter> configuration_parameter;
};

struct get_many_configuration_parameters_request {
    using response_type = struct get_many_configuration_parameters_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_parameters.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_parameter_key> keys;
};

struct get_many_configuration_parameters_response {
    ores::utility::domain::result result;
    std::vector<configuration_parameter_lookup> entries;
};

struct put_configuration_parameter_request {
    using response_type = struct put_configuration_parameter_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configuration_parameters.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_parameter_change change;
    ores::utility::domain::change_intent intent;
};

struct put_configuration_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_parameter> configuration_parameter;
};

struct put_many_configuration_parameters_request {
    using response_type = struct put_many_configuration_parameters_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_parameters.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_parameter_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_configuration_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_parameter> parameter_values;
};

struct delete_configuration_parameter_request {
    using response_type = struct delete_configuration_parameter_response;
    static constexpr std::string_view nats_subject = "reporting.v1.configuration_parameters.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_parameter_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_configuration_parameter_response {
    ores::utility::domain::result result;
};

struct delete_many_configuration_parameters_request {
    using response_type = struct delete_many_configuration_parameters_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_parameters.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<configuration_parameter_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_configuration_parameters_response {
    ores::utility::domain::result result;
};

struct list_by_configuration_id_configuration_parameters_request {
    using response_type = struct list_by_configuration_id_configuration_parameters_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_parameters.list_by_configuration_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid configuration_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configuration_parameters_filter> filter;
};

struct list_by_configuration_id_configuration_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_parameter> parameter_values;
    std::uint64_t total;
};

struct list_by_parameter_definition_id_configuration_parameters_request {
    using response_type = struct list_by_parameter_definition_id_configuration_parameters_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_parameters.list_by_parameter_definition_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid parameter_definition_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configuration_parameters_filter> filter;
};

struct list_by_parameter_definition_id_configuration_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_parameter> parameter_values;
    std::uint64_t total;
};

struct list_configuration_parameter_versions_request {
    using response_type = struct list_configuration_parameter_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_parameters_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_parameter_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<configuration_parameter_versions_filter> filter;
};

struct list_configuration_parameter_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::configuration_parameter> versions;
    std::uint64_t total;
};

struct get_configuration_parameter_version_request {
    using response_type = struct get_configuration_parameter_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.configuration_parameters_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    configuration_parameter_version_key key;
};

struct get_configuration_parameter_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::configuration_parameter> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace configuration_parameter_event_subjects {
inline constexpr std::string_view created = "reporting.v1.configuration_parameters_events.created";
inline constexpr std::string_view updated = "reporting.v1.configuration_parameters_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.configuration_parameters_events.deleted";
}

}

#endif
