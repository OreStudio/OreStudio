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
#ifndef ORES_ANALYTICS_API_MESSAGING_CREDIT_SIMULATION_MATRIX_ROW_CONFIG_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_CREDIT_SIMULATION_MATRIX_ROW_CONFIG_PROTOCOL_HPP

#include "ores.analytics.api/domain/credit_simulation_matrix_row_config.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::messaging {

struct credit_simulation_matrix_row_config_key {
    std::string from_rating;
};

struct credit_simulation_matrix_row_config_write {
    boost::uuids::uuid id;
    boost::uuids::uuid transition_matrix_id;
    std::string from_rating;
    double p_aaa;
    double p_aa;
    double p_a;
    double p_baa;
    double p_ba;
    double p_b;
    double p_c;
    double p_default;
};

struct credit_simulation_matrix_row_config_change {
    credit_simulation_matrix_row_config_write write;
    ores::utility::domain::precondition precondition;
};

struct credit_simulation_matrix_row_config_removal {
    credit_simulation_matrix_row_config_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct credit_simulation_matrix_row_config_lookup {
    credit_simulation_matrix_row_config_key key;
    std::optional<ores::analytics::domain::credit_simulation_matrix_row_config>
        credit_simulation_matrix_row_config;
};

struct credit_simulation_matrix_row_configs_filter {
    std::optional<boost::uuids::uuid> transition_matrix_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> transition_matrix_id_one_of;
};

struct credit_simulation_matrix_row_config_event {
    boost::uuids::uuid event_id;
    credit_simulation_matrix_row_config_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct credit_simulation_matrix_row_config_version_key {
    credit_simulation_matrix_row_config_key credit_simulation_matrix_row_config;
    std::uint32_t version;
};

struct credit_simulation_matrix_row_config_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_credit_simulation_matrix_row_configs_request {
    using response_type = struct list_credit_simulation_matrix_row_configs_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs.list";
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
    std::optional<credit_simulation_matrix_row_configs_filter> filter;
};

struct list_credit_simulation_matrix_row_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::credit_simulation_matrix_row_config> rows;
    std::uint64_t total;
};

struct get_credit_simulation_matrix_row_config_request {
    using response_type = struct get_credit_simulation_matrix_row_config_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_simulation_matrix_row_config_key key;
};

struct get_credit_simulation_matrix_row_config_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::credit_simulation_matrix_row_config>
        credit_simulation_matrix_row_config;
};

struct get_many_credit_simulation_matrix_row_configs_request {
    using response_type = struct get_many_credit_simulation_matrix_row_configs_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_simulation_matrix_row_config_key> keys;
};

struct get_many_credit_simulation_matrix_row_configs_response {
    ores::utility::domain::result result;
    std::vector<credit_simulation_matrix_row_config_lookup> entries;
};

struct put_credit_simulation_matrix_row_config_request {
    using response_type = struct put_credit_simulation_matrix_row_config_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_simulation_matrix_row_config_change change;
    ores::utility::domain::change_intent intent;
};

struct put_credit_simulation_matrix_row_config_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::credit_simulation_matrix_row_config>
        credit_simulation_matrix_row_config;
};

struct put_many_credit_simulation_matrix_row_configs_request {
    using response_type = struct put_many_credit_simulation_matrix_row_configs_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_simulation_matrix_row_config_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_credit_simulation_matrix_row_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::credit_simulation_matrix_row_config> rows;
};

struct delete_credit_simulation_matrix_row_config_request {
    using response_type = struct delete_credit_simulation_matrix_row_config_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_simulation_matrix_row_config_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_credit_simulation_matrix_row_config_response {
    ores::utility::domain::result result;
};

struct delete_many_credit_simulation_matrix_row_configs_request {
    using response_type = struct delete_many_credit_simulation_matrix_row_configs_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<credit_simulation_matrix_row_config_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_credit_simulation_matrix_row_configs_response {
    ores::utility::domain::result result;
};

struct list_by_transition_matrix_id_credit_simulation_matrix_row_configs_request {
    using response_type =
        struct list_by_transition_matrix_id_credit_simulation_matrix_row_configs_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs.list_by_transition_matrix_id";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid transition_matrix_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<credit_simulation_matrix_row_configs_filter> filter;
};

struct list_by_transition_matrix_id_credit_simulation_matrix_row_configs_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::credit_simulation_matrix_row_config> rows;
    std::uint64_t total;
};

struct list_credit_simulation_matrix_row_config_versions_request {
    using response_type = struct list_credit_simulation_matrix_row_config_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_simulation_matrix_row_config_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<credit_simulation_matrix_row_config_versions_filter> filter;
};

struct list_credit_simulation_matrix_row_config_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::credit_simulation_matrix_row_config> versions;
    std::uint64_t total;
};

struct get_credit_simulation_matrix_row_config_version_request {
    using response_type = struct get_credit_simulation_matrix_row_config_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.credit_simulation_matrix_row_configs_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    credit_simulation_matrix_row_config_version_key key;
};

struct get_credit_simulation_matrix_row_config_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::credit_simulation_matrix_row_config> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace credit_simulation_matrix_row_config_event_subjects {
inline constexpr std::string_view created =
    "analytics.v1.credit_simulation_matrix_row_configs_events.created";
inline constexpr std::string_view updated =
    "analytics.v1.credit_simulation_matrix_row_configs_events.updated";
inline constexpr std::string_view deleted =
    "analytics.v1.credit_simulation_matrix_row_configs_events.deleted";
}

}

#endif
