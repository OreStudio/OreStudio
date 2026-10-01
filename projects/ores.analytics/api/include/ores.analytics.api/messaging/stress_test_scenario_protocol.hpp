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
#ifndef ORES_ANALYTICS_API_MESSAGING_STRESS_TEST_SCENARIO_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_STRESS_TEST_SCENARIO_PROTOCOL_HPP

#include "ores.analytics.api/domain/stress_test_scenario.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::messaging {

struct stress_test_scenario_key {
    boost::uuids::uuid id;
};

struct stress_test_scenario_write {
    boost::uuids::uuid id;
    boost::uuids::uuid stress_test_library_id;
    std::string name;
    std::optional<std::string> date;
    int position;
};

struct stress_test_scenario_change {
    stress_test_scenario_write write;
    ores::utility::domain::precondition precondition;
};

struct stress_test_scenario_removal {
    stress_test_scenario_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct stress_test_scenario_lookup {
    stress_test_scenario_key key;
    std::optional<ores::analytics::domain::stress_test_scenario> stress_test_scenario;
};

struct stress_test_scenario_event {
    boost::uuids::uuid event_id;
    stress_test_scenario_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct stress_test_scenario_version_key {
    stress_test_scenario_key stress_test_scenario;
    std::uint32_t version;
};

struct stress_test_scenario_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_stress_test_scenarios_request {
    using response_type = struct list_stress_test_scenarios_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_scenarios.list";
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

struct list_stress_test_scenarios_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_scenario> stress_test_scenarios;
    std::uint64_t total;
};

struct get_stress_test_scenario_request {
    using response_type = struct get_stress_test_scenario_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_scenarios.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_scenario_key key;
};

struct get_stress_test_scenario_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_scenario> stress_test_scenario;
};

struct get_many_stress_test_scenarios_request {
    using response_type = struct get_many_stress_test_scenarios_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_scenarios.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_scenario_key> keys;
};

struct get_many_stress_test_scenarios_response {
    ores::utility::domain::result result;
    std::vector<stress_test_scenario_lookup> entries;
};

struct put_stress_test_scenario_request {
    using response_type = struct put_stress_test_scenario_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_scenarios.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_scenario_change change;
    ores::utility::domain::change_intent intent;
};

struct put_stress_test_scenario_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_scenario> stress_test_scenario;
};

struct put_many_stress_test_scenarios_request {
    using response_type = struct put_many_stress_test_scenarios_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_scenarios.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_scenario_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_stress_test_scenarios_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_scenario> stress_test_scenarios;
};

struct delete_stress_test_scenario_request {
    using response_type = struct delete_stress_test_scenario_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_scenarios.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_scenario_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_stress_test_scenario_response {
    ores::utility::domain::result result;
};

struct delete_many_stress_test_scenarios_request {
    using response_type = struct delete_many_stress_test_scenarios_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.stress_test_scenarios.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_scenario_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_stress_test_scenarios_response {
    ores::utility::domain::result result;
};

struct list_stress_test_scenario_versions_request {
    using response_type = struct list_stress_test_scenario_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.stress_test_scenarios_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_scenario_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<stress_test_scenario_versions_filter> filter;
};

struct list_stress_test_scenario_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_scenario> versions;
    std::uint64_t total;
};

struct get_stress_test_scenario_version_request {
    using response_type = struct get_stress_test_scenario_version_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.stress_test_scenarios_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_scenario_version_key key;
};

struct get_stress_test_scenario_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_scenario> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace stress_test_scenario_event_subjects {
inline constexpr std::string_view created = "analytics.v1.stress_test_scenarios_events.created";
inline constexpr std::string_view updated = "analytics.v1.stress_test_scenarios_events.updated";
inline constexpr std::string_view deleted = "analytics.v1.stress_test_scenarios_events.deleted";
}

}

#endif
