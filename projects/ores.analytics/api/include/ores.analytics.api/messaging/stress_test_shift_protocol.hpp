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
#ifndef ORES_ANALYTICS_API_MESSAGING_STRESS_TEST_SHIFT_PROTOCOL_HPP
#define ORES_ANALYTICS_API_MESSAGING_STRESS_TEST_SHIFT_PROTOCOL_HPP

#include "ores.analytics.api/domain/stress_test_shift.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::analytics::messaging {

struct stress_test_shift_key {
    boost::uuids::uuid id;
};

struct stress_test_shift_write {
    boost::uuids::uuid id;
    boost::uuids::uuid stress_test_scenario_id;
    std::string family;
    std::string object_key;
    std::optional<std::string> shift_type;
    std::optional<std::string> shifts;
    std::optional<std::string> shift_tenors;
    std::optional<std::string> shift_expiries;
    std::optional<std::string> extras;
    int position;
};

struct stress_test_shift_change {
    stress_test_shift_write write;
    ores::utility::domain::precondition precondition;
};

struct stress_test_shift_removal {
    stress_test_shift_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct stress_test_shift_lookup {
    stress_test_shift_key key;
    std::optional<ores::analytics::domain::stress_test_shift> stress_test_shift;
};

struct stress_test_shifts_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct stress_test_shift_event {
    boost::uuids::uuid event_id;
    stress_test_shift_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct stress_test_shift_version_key {
    stress_test_shift_key stress_test_shift;
    std::uint32_t version;
};

struct stress_test_shift_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_stress_test_shifts_request {
    using response_type = struct list_stress_test_shifts_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.list";
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
    std::optional<stress_test_shifts_filter> filter;
    std::optional<std::string> as_of;
};

struct list_stress_test_shifts_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_shift> stress_test_shifts;
    std::uint64_t total;
};

struct get_stress_test_shift_request {
    using response_type = struct get_stress_test_shift_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_key key;
};

struct get_stress_test_shift_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_shift> stress_test_shift;
};

struct get_many_stress_test_shifts_request {
    using response_type = struct get_many_stress_test_shifts_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_shift_key> keys;
};

struct get_many_stress_test_shifts_response {
    ores::utility::domain::result result;
    std::vector<stress_test_shift_lookup> entries;
};

struct put_stress_test_shift_request {
    using response_type = struct put_stress_test_shift_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_change change;
    ores::utility::domain::change_intent intent;
};

struct put_stress_test_shift_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_shift> stress_test_shift;
};

struct put_many_stress_test_shifts_request {
    using response_type = struct put_many_stress_test_shifts_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_shift_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_stress_test_shifts_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_shift> stress_test_shifts;
};

struct delete_stress_test_shift_request {
    using response_type = struct delete_stress_test_shift_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_stress_test_shift_response {
    ores::utility::domain::result result;
};

struct delete_many_stress_test_shifts_request {
    using response_type = struct delete_many_stress_test_shifts_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<stress_test_shift_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_stress_test_shifts_response {
    ores::utility::domain::result result;
};

struct list_stress_test_shift_versions_request {
    using response_type = struct list_stress_test_shift_versions_response;
    static constexpr std::string_view nats_subject =
        "analytics.v1.stress_test_shifts_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<stress_test_shift_versions_filter> filter;
};

struct list_stress_test_shift_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::analytics::domain::stress_test_shift> versions;
    std::uint64_t total;
};

struct get_stress_test_shift_version_request {
    using response_type = struct get_stress_test_shift_version_response;
    static constexpr std::string_view nats_subject = "analytics.v1.stress_test_shifts_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    stress_test_shift_version_key key;
};

struct get_stress_test_shift_version_response {
    ores::utility::domain::result result;
    std::optional<ores::analytics::domain::stress_test_shift> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace stress_test_shift_event_subjects {
inline constexpr std::string_view created = "analytics.v1.stress_test_shifts_events.created";
inline constexpr std::string_view updated = "analytics.v1.stress_test_shifts_events.updated";
inline constexpr std::string_view deleted = "analytics.v1.stress_test_shifts_events.deleted";
}

}

#endif
