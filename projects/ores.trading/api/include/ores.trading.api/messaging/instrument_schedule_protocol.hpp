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
#ifndef ORES_TRADING_API_MESSAGING_INSTRUMENT_SCHEDULE_PROTOCOL_HPP
#define ORES_TRADING_API_MESSAGING_INSTRUMENT_SCHEDULE_PROTOCOL_HPP

#include "ores.trading.api/domain/instrument_schedule.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::trading::messaging {

struct instrument_schedule_key {
    boost::uuids::uuid trade_id;
    std::string owner_role;
    int owner_number;
    std::string schedule_role;
    int sequence_number;
};

struct instrument_schedule_write {
    boost::uuids::uuid trade_id;
    std::string owner_role;
    int owner_number;
    std::string schedule_role;
    int sequence_number;
    boost::uuids::uuid trade_activity_id;
    std::string schedule_kind;
    std::optional<std::chrono::year_month_day> start_date;
    std::optional<std::chrono::year_month_day> end_date;
    std::optional<std::string> adjust_end_date_to_previous_month_end;
    std::optional<std::string> tenor;
    std::optional<std::string> calendar;
    std::optional<std::string> convention;
    std::optional<std::string> term_convention;
    std::optional<std::string> rule;
    std::optional<std::string> end_of_month;
    std::optional<std::string> end_of_month_convention;
    std::optional<std::chrono::year_month_day> first_date;
    std::optional<std::chrono::year_month_day> last_date;
    std::optional<bool> remove_first_date;
    std::optional<bool> remove_last_date;
    std::optional<std::string> include_duplicate_dates;
};

struct instrument_schedule_change {
    instrument_schedule_write write;
    ores::utility::domain::precondition precondition;
};

struct instrument_schedule_removal {
    instrument_schedule_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct instrument_schedule_lookup {
    instrument_schedule_key key;
    std::optional<ores::trading::domain::instrument_schedule> instrument_schedule;
};

struct instrument_schedule_event {
    boost::uuids::uuid event_id;
    instrument_schedule_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct instrument_schedule_version_key {
    instrument_schedule_key instrument_schedule;
    std::uint32_t version;
};

struct instrument_schedule_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_instrument_schedules_request {
    using response_type = struct list_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.list";
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
    std::optional<std::string> as_of;
};

struct list_instrument_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_schedule> instrument_schedules;
    std::uint64_t total;
};

struct get_instrument_schedule_request {
    using response_type = struct get_instrument_schedule_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_key key;
};

struct get_instrument_schedule_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_schedule> instrument_schedule;
};

struct get_many_instrument_schedules_request {
    using response_type = struct get_many_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_schedule_key> keys;
};

struct get_many_instrument_schedules_response {
    ores::utility::domain::result result;
    std::vector<instrument_schedule_lookup> entries;
};

struct put_instrument_schedule_request {
    using response_type = struct put_instrument_schedule_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_change change;
    ores::utility::domain::change_intent intent;
};

struct put_instrument_schedule_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_schedule> instrument_schedule;
};

struct put_many_instrument_schedules_request {
    using response_type = struct put_many_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_schedule_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_instrument_schedules_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_schedule> instrument_schedules;
};

struct delete_instrument_schedule_request {
    using response_type = struct delete_instrument_schedule_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_instrument_schedule_response {
    ores::utility::domain::result result;
};

struct delete_many_instrument_schedules_request {
    using response_type = struct delete_many_instrument_schedules_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<instrument_schedule_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_instrument_schedules_response {
    ores::utility::domain::result result;
};

struct list_instrument_schedule_versions_request {
    using response_type = struct list_instrument_schedule_versions_response;
    static constexpr std::string_view nats_subject =
        "trading.v1.instrument_schedules_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<instrument_schedule_versions_filter> filter;
};

struct list_instrument_schedule_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::trading::domain::instrument_schedule> versions;
    std::uint64_t total;
};

struct get_instrument_schedule_version_request {
    using response_type = struct get_instrument_schedule_version_response;
    static constexpr std::string_view nats_subject = "trading.v1.instrument_schedules_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    instrument_schedule_version_key key;
};

struct get_instrument_schedule_version_response {
    ores::utility::domain::result result;
    std::optional<ores::trading::domain::instrument_schedule> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace instrument_schedule_event_subjects {
inline constexpr std::string_view created = "trading.v1.instrument_schedules_events.created";
inline constexpr std::string_view updated = "trading.v1.instrument_schedules_events.updated";
inline constexpr std::string_view deleted = "trading.v1.instrument_schedules_events.deleted";
}

}

#endif
