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
#ifndef ORES_REPORTING_API_MESSAGING_ORE_PARAMETER_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_ORE_PARAMETER_PROTOCOL_HPP

#include "ores.reporting.api/domain/ore_parameter.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::reporting::messaging {

struct ore_parameter_key {
    std::string name;
};

struct ore_parameter_write {
    boost::uuids::uuid id;
    std::string scope;
    std::string subtype;
    std::string name;
    std::string schema_name;
    std::string xpath;
    int position;
    std::string ore_parameter_value_domain_code;
    bool is_required;
    std::string label;
    std::string help;
    std::string description;
    std::string default_text;
    std::string format;
    std::string introduced_in;
    std::string removed_in;
};

struct ore_parameter_change {
    ore_parameter_write write;
    ores::utility::domain::precondition precondition;
};

struct ore_parameter_removal {
    ore_parameter_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct ore_parameter_lookup {
    ore_parameter_key key;
    std::optional<ores::reporting::domain::ore_parameter> ore_parameter;
};

struct ore_parameters_filter {
    std::optional<std::string> ore_parameter_value_domain_code;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<std::string>> ore_parameter_value_domain_code_one_of;
};

struct ore_parameter_event {
    boost::uuids::uuid event_id;
    ore_parameter_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct ore_parameter_version_key {
    ore_parameter_key ore_parameter;
    std::uint32_t version;
};

struct ore_parameter_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_ore_parameters_request {
    using response_type = struct list_ore_parameters_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters.list";
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
    std::optional<ore_parameters_filter> filter;
    std::optional<std::string> as_of;
};

struct list_ore_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_parameter> parameters;
    std::uint64_t total;
};

struct get_ore_parameter_request {
    using response_type = struct get_ore_parameter_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_key key;
};

struct get_ore_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_parameter> ore_parameter;
};

struct get_many_ore_parameters_request {
    using response_type = struct get_many_ore_parameters_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_parameter_key> keys;
};

struct get_many_ore_parameters_response {
    ores::utility::domain::result result;
    std::vector<ore_parameter_lookup> entries;
};

struct put_ore_parameter_request {
    using response_type = struct put_ore_parameter_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_change change;
    ores::utility::domain::change_intent intent;
};

struct put_ore_parameter_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_parameter> ore_parameter;
};

struct put_many_ore_parameters_request {
    using response_type = struct put_many_ore_parameters_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_parameter_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_ore_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_parameter> parameters;
};

struct delete_ore_parameter_request {
    using response_type = struct delete_ore_parameter_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_ore_parameter_response {
    ores::utility::domain::result result;
};

struct delete_many_ore_parameters_request {
    using response_type = struct delete_many_ore_parameters_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ore_parameter_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_ore_parameters_response {
    ores::utility::domain::result result;
};

struct list_by_ore_parameter_value_domain_code_ore_parameters_request {
    using response_type = struct list_by_ore_parameter_value_domain_code_ore_parameters_response;
    static constexpr std::string_view nats_subject =
        "reporting.v1.ore_parameters.list_by_ore_parameter_value_domain_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string ore_parameter_value_domain_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ore_parameters_filter> filter;
};

struct list_by_ore_parameter_value_domain_code_ore_parameters_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_parameter> parameters;
    std::uint64_t total;
};

struct list_ore_parameter_versions_request {
    using response_type = struct list_ore_parameter_versions_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ore_parameter_versions_filter> filter;
};

struct list_ore_parameter_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::ore_parameter> versions;
    std::uint64_t total;
};

struct get_ore_parameter_version_request {
    using response_type = struct get_ore_parameter_version_response;
    static constexpr std::string_view nats_subject = "reporting.v1.ore_parameters_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ore_parameter_version_key key;
};

struct get_ore_parameter_version_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::ore_parameter> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace ore_parameter_event_subjects {
inline constexpr std::string_view created = "reporting.v1.ore_parameters_events.created";
inline constexpr std::string_view updated = "reporting.v1.ore_parameters_events.updated";
inline constexpr std::string_view deleted = "reporting.v1.ore_parameters_events.deleted";
}

}

#endif
