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
#ifndef ORES_REFDATA_API_MESSAGING_TENOR_SCHEDULE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_TENOR_SCHEDULE_PROTOCOL_HPP

#include "ores.refdata.api/domain/tenor_schedule.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct tenor_schedule_key {
    std::string code;
};

struct tenor_schedule_write {
    std::string code;
    std::string name;
    std::string description;
    int display_order;
    std::string schedule_source;
    std::optional<std::string> calendar_code;
    std::optional<std::string> diary_entry_type;
};

struct tenor_schedule_change {
    tenor_schedule_write write;
    ores::utility::domain::precondition precondition;
};

struct tenor_schedule_removal {
    tenor_schedule_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct tenor_schedule_lookup {
    tenor_schedule_key key;
    std::optional<ores::refdata::domain::tenor_schedule> tenor_schedule;
};

struct tenor_schedules_filter {
    std::optional<std::optional<std::string>> calendar_code;
    std::optional<std::optional<std::string>> diary_entry_type;
};

struct tenor_schedule_event {
    boost::uuids::uuid event_id;
    tenor_schedule_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct tenor_schedule_version_key {
    tenor_schedule_key tenor_schedule;
    std::uint32_t version;
};

struct tenor_schedule_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_tenor_schedules_request {
    using response_type = struct list_tenor_schedules_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules.list";
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
    std::optional<tenor_schedules_filter> filter;
};

struct list_tenor_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_schedule> schedules;
    std::uint64_t total;
};

struct get_tenor_schedule_request {
    using response_type = struct get_tenor_schedule_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_schedule_key key;
};

struct get_tenor_schedule_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::tenor_schedule> tenor_schedule;
};

struct get_many_tenor_schedules_request {
    using response_type = struct get_many_tenor_schedules_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_schedule_key> keys;
};

struct get_many_tenor_schedules_response {
    ores::utility::domain::result result;
    std::vector<tenor_schedule_lookup> entries;
};

struct put_tenor_schedule_request {
    using response_type = struct put_tenor_schedule_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_schedule_change change;
    ores::utility::domain::change_intent intent;
};

struct put_tenor_schedule_response {
    ores::utility::domain::result result;
    ores::refdata::domain::tenor_schedule tenor_schedule;
};

struct put_many_tenor_schedules_request {
    using response_type = struct put_many_tenor_schedules_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_schedule_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_tenor_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_schedule> schedules;
};

struct delete_tenor_schedule_request {
    using response_type = struct delete_tenor_schedule_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_schedule_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_tenor_schedule_response {
    ores::utility::domain::result result;
};

struct delete_many_tenor_schedules_request {
    using response_type = struct delete_many_tenor_schedules_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<tenor_schedule_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_tenor_schedules_response {
    ores::utility::domain::result result;
};

struct list_by_calendar_code_tenor_schedules_request {
    using response_type = struct list_by_calendar_code_tenor_schedules_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_schedules.list_by_calendar_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::optional<std::string> calendar_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_schedules_filter> filter;
};

struct list_by_calendar_code_tenor_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_schedule> schedules;
    std::uint64_t total;
};

struct list_by_diary_entry_type_tenor_schedules_request {
    using response_type = struct list_by_diary_entry_type_tenor_schedules_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.tenor_schedules.list_by_diary_entry_type";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::optional<std::string> diary_entry_type;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_schedules_filter> filter;
};

struct list_by_diary_entry_type_tenor_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_schedule> schedules;
    std::uint64_t total;
};

struct list_tenor_schedule_versions_request {
    using response_type = struct list_tenor_schedule_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_schedule_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<tenor_schedule_versions_filter> filter;
};

struct list_tenor_schedule_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::tenor_schedule> versions;
    std::uint64_t total;
};

struct get_tenor_schedule_version_request {
    using response_type = struct get_tenor_schedule_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.tenor_schedules_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    tenor_schedule_version_key key;
};

struct get_tenor_schedule_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::tenor_schedule version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace tenor_schedule_event_subjects {
inline constexpr std::string_view created = "refdata.v1.tenor_schedules_events.created";
inline constexpr std::string_view updated = "refdata.v1.tenor_schedules_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.tenor_schedules_events.deleted";
}

}

#endif
