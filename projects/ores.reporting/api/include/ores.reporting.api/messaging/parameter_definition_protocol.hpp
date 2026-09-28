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
#ifndef ORES_REPORTING_API_MESSAGING_PARAMETER_DEFINITION_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_PARAMETER_DEFINITION_PROTOCOL_HPP

#include "ores.reporting.api/domain/parameter_definition.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct parameter_definition_key {
    std::string name;
};

struct parameter_definition_write {
    boost::uuids::uuid id;
    std::string scope;
    std::string subtype;
    std::string name;
    int position;
    std::string parameter_value_domain_code;
    bool is_required;
};

struct parameter_definition_change {
    parameter_definition_write write;
    ores::utility::domain::precondition precondition;
};

struct parameter_definition_removal {
    parameter_definition_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct parameter_definition_lookup {
    parameter_definition_key key;
    std::optional<ores::reporting::domain::parameter_definition> parameter_definition;
};

struct parameter_definitions_filter {
    std::optional<std::string> parameter_value_domain_code;
};

struct parameter_definition_event {
    boost::uuids::uuid event_id;
    parameter_definition_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct parameter_definition_version_key {
    parameter_definition_key parameter_definition;
    std::uint32_t version;
};

struct parameter_definition_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_parameter_definitions_request {
    using response_type = struct list_parameter_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.parameter_definitions.list";
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
    std::optional<parameter_definitions_filter> filter;
};

struct list_parameter_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::parameter_definition> parameters;
    std::uint64_t total;
};

struct get_parameter_definition_request {
    using response_type = struct get_parameter_definition_response;
    static constexpr std::string_view nats_subject = "reporting.v1.parameter_definitions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    parameter_definition_key key;
};

struct get_parameter_definition_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::parameter_definition> parameter_definition;
};

struct get_many_parameter_definitions_request {
    using response_type = struct get_many_parameter_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.parameter_definitions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<parameter_definition_key> keys;
};

struct get_many_parameter_definitions_response {
    ores::utility::domain::result result;
    std::vector<parameter_definition_lookup> entries;
};

struct put_parameter_definition_request {
    using response_type = struct put_parameter_definition_response;
    static constexpr std::string_view nats_subject = "reporting.v1.parameter_definitions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    parameter_definition_change change;
    ores::utility::domain::change_intent intent;
};

struct put_parameter_definition_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::parameter_definition> parameter_definition;
};

struct put_many_parameter_definitions_request {
    using response_type = struct put_many_parameter_definitions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.parameter_definitions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<parameter_definition_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_parameter_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::parameter_definition> parameters;
};

struct delete_parameter_definition_request {
    using response_type = struct delete_parameter_definition_response;
    static constexpr std::string_view nats_subject = "reporting.v1.parameter_definitions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    parameter_definition_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_parameter_definition_response {
    ores::utility::domain::result result;
};

struct delete_many_parameter_definitions_request {
    using response_type = struct delete_many_parameter_definitions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.parameter_definitions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<parameter_definition_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_parameter_definitions_response {
    ores::utility::domain::result result;
};

struct list_by_parameter_value_domain_code_parameter_definitions_request {
    using response_type = struct list_by_parameter_value_domain_code_parameter_definitions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.parameter_definitions.list_by_parameter_value_domain_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string parameter_value_domain_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<parameter_definitions_filter> filter;
};

struct list_by_parameter_value_domain_code_parameter_definitions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::parameter_definition> parameters;
    std::uint64_t total;
};

struct list_parameter_definition_versions_request {
    using response_type = struct list_parameter_definition_versions_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.parameter_definitions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    parameter_definition_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<parameter_definition_versions_filter> filter;
};

struct list_parameter_definition_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::parameter_definition> versions;
    std::uint64_t total;
};

struct get_parameter_definition_version_request {
    using response_type = struct get_parameter_definition_version_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.parameter_definitions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    parameter_definition_version_key key;
};

struct get_parameter_definition_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::parameter_definition> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace parameter_definition_event_subjects {
inline constexpr std::string_view created = "reporting.v1.parameter_definitions_events.created";
inline constexpr std::string_view updated = "reporting.v1.parameter_definitions_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.parameter_definitions_events.deleted";
}

}

#endif
