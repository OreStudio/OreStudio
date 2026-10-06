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
#ifndef ORES_REFDATA_API_MESSAGING_DEFAULT_CURVE_CONFIGURATION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_DEFAULT_CURVE_CONFIGURATION_PROTOCOL_HPP

#include "ores.refdata.api/domain/default_curve_configuration.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct default_curve_configuration_key {
    boost::uuids::uuid id;
};

struct default_curve_configuration_write {
    boost::uuids::uuid id;
    boost::uuids::uuid curve_definition_id;
    bool is_inline;
    std::optional<int> priority;
    std::optional<std::string> default_curve_type;
    std::optional<std::string> discount_curve;
    std::optional<std::string> day_counter;
    std::optional<std::string> recovery_rate;
    std::optional<std::string> start_date;
    bool has_quotes;
    std::optional<std::string> benchmark_curve;
    std::optional<std::string> reinterpreted_yield_curve;
    std::optional<std::string> source_curve;
    std::optional<std::string> pillars;
    std::optional<int> spot_lag;
    std::optional<std::string> calendar;
    std::optional<std::string> conventions;
    std::optional<std::string> extrapolation;
    std::optional<double> running_spread;
    std::optional<std::string> index_term;
    std::optional<std::string> imply_default_from_market;
    std::optional<std::string> allow_negative_rates;
    std::optional<std::string> price_is_upfront;
    std::optional<std::string> initial_state;
    std::optional<std::string> states;
    int position;
};

struct default_curve_configuration_change {
    default_curve_configuration_write write;
    ores::utility::domain::precondition precondition;
};

struct default_curve_configuration_removal {
    default_curve_configuration_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct default_curve_configuration_lookup {
    default_curve_configuration_key key;
    std::optional<ores::refdata::domain::default_curve_configuration> default_curve_configuration;
};

struct default_curve_configurations_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct default_curve_configuration_event {
    boost::uuids::uuid event_id;
    default_curve_configuration_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct default_curve_configuration_version_key {
    default_curve_configuration_key default_curve_configuration;
    std::uint32_t version;
};

struct default_curve_configuration_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_default_curve_configurations_request {
    using response_type = struct list_default_curve_configurations_response;
    static constexpr std::string_view nats_subject = "refdata.v1.default_curve_configurations.list";
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
    std::optional<default_curve_configurations_filter> filter;
    std::optional<std::string> as_of;
};

struct list_default_curve_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::default_curve_configuration> configurations;
    std::uint64_t total;
};

struct get_default_curve_configuration_request {
    using response_type = struct get_default_curve_configuration_response;
    static constexpr std::string_view nats_subject = "refdata.v1.default_curve_configurations.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    default_curve_configuration_key key;
};

struct get_default_curve_configuration_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::default_curve_configuration> default_curve_configuration;
};

struct get_many_default_curve_configurations_request {
    using response_type = struct get_many_default_curve_configurations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.default_curve_configurations.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<default_curve_configuration_key> keys;
};

struct get_many_default_curve_configurations_response {
    ores::utility::domain::result result;
    std::vector<default_curve_configuration_lookup> entries;
};

struct put_default_curve_configuration_request {
    using response_type = struct put_default_curve_configuration_response;
    static constexpr std::string_view nats_subject = "refdata.v1.default_curve_configurations.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    default_curve_configuration_change change;
    ores::utility::domain::change_intent intent;
};

struct put_default_curve_configuration_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::default_curve_configuration> default_curve_configuration;
};

struct put_many_default_curve_configurations_request {
    using response_type = struct put_many_default_curve_configurations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.default_curve_configurations.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<default_curve_configuration_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_default_curve_configurations_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::default_curve_configuration> configurations;
};

struct delete_default_curve_configuration_request {
    using response_type = struct delete_default_curve_configuration_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.default_curve_configurations.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    default_curve_configuration_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_default_curve_configuration_response {
    ores::utility::domain::result result;
};

struct delete_many_default_curve_configurations_request {
    using response_type = struct delete_many_default_curve_configurations_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.default_curve_configurations.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<default_curve_configuration_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_default_curve_configurations_response {
    ores::utility::domain::result result;
};

struct list_default_curve_configuration_versions_request {
    using response_type = struct list_default_curve_configuration_versions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.default_curve_configurations_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    default_curve_configuration_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<default_curve_configuration_versions_filter> filter;
};

struct list_default_curve_configuration_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::default_curve_configuration> versions;
    std::uint64_t total;
};

struct get_default_curve_configuration_version_request {
    using response_type = struct get_default_curve_configuration_version_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.default_curve_configurations_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    default_curve_configuration_version_key key;
};

struct get_default_curve_configuration_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::default_curve_configuration> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace default_curve_configuration_event_subjects {
inline constexpr std::string_view created =
    "refdata.v1.default_curve_configurations_events.created";
inline constexpr std::string_view updated =
    "refdata.v1.default_curve_configurations_events.updated";
inline constexpr std::string_view deleted =
    "refdata.v1.default_curve_configurations_events.deleted";
}

}

#endif
