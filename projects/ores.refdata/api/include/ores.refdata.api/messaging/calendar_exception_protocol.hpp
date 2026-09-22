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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_EXCEPTION_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_EXCEPTION_PROTOCOL_HPP

#include "ores.refdata.api/domain/calendar_exception.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct calendar_exception_key {
    boost::uuids::uuid id;
};

struct calendar_exception_write {
    boost::uuids::uuid id;
    std::string calendar_code;
    std::chrono::year_month_day exception_date;
    bool is_business_day;
    std::optional<std::string> description;
};

struct calendar_exception_change {
    calendar_exception_write write;
    ores::utility::domain::precondition precondition;
};

struct calendar_exception_removal {
    calendar_exception_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct calendar_exception_lookup {
    calendar_exception_key key;
    std::optional<ores::refdata::domain::calendar_exception> calendar_exception;
};

struct calendar_exceptions_filter {
    std::optional<std::string> calendar_code;
};

struct calendar_exception_event {
    boost::uuids::uuid event_id;
    calendar_exception_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct calendar_exception_version_key {
    calendar_exception_key calendar_exception;
    std::uint32_t version;
};

struct calendar_exception_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_calendar_exceptions_request {
    using response_type = struct list_calendar_exceptions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.list";
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
    std::optional<calendar_exceptions_filter> filter;
};

struct list_calendar_exceptions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_exception> calendar_exceptions;
    std::uint64_t total;
};

struct get_calendar_exception_request {
    using response_type = struct get_calendar_exception_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_key key;
};

struct get_calendar_exception_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_exception> calendar_exception;
};

struct get_many_calendar_exceptions_request {
    using response_type = struct get_many_calendar_exceptions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_exception_key> keys;
};

struct get_many_calendar_exceptions_response {
    ores::utility::domain::result result;
    std::vector<calendar_exception_lookup> entries;
};

struct put_calendar_exception_request {
    using response_type = struct put_calendar_exception_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_change change;
    ores::utility::domain::change_intent intent;
};

struct put_calendar_exception_response {
    ores::utility::domain::result result;
    ores::refdata::domain::calendar_exception calendar_exception;
};

struct put_many_calendar_exceptions_request {
    using response_type = struct put_many_calendar_exceptions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_exception_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_calendar_exceptions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_exception> calendar_exceptions;
};

struct delete_calendar_exception_request {
    using response_type = struct delete_calendar_exception_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_calendar_exception_response {
    ores::utility::domain::result result;
};

struct delete_many_calendar_exceptions_request {
    using response_type = struct delete_many_calendar_exceptions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_exception_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_calendar_exceptions_response {
    ores::utility::domain::result result;
};

struct list_by_calendar_code_calendar_exceptions_request {
    using response_type = struct list_by_calendar_code_calendar_exceptions_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.calendar_exceptions.list_by_calendar_code";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string calendar_code;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calendar_exceptions_filter> filter;
};

struct list_by_calendar_code_calendar_exceptions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_exception> calendar_exceptions;
    std::uint64_t total;
};

struct list_calendar_exception_versions_request {
    using response_type = struct list_calendar_exception_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<calendar_exception_versions_filter> filter;
};

struct list_calendar_exception_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_exception> versions;
    std::uint64_t total;
};

struct get_calendar_exception_version_request {
    using response_type = struct get_calendar_exception_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_exceptions_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_exception_version_key key;
};

struct get_calendar_exception_version_response {
    ores::utility::domain::result result;
    ores::refdata::domain::calendar_exception version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace calendar_exception_event_subjects {
inline constexpr std::string_view created = "refdata.v1.calendar_exceptions_events.created";
inline constexpr std::string_view updated = "refdata.v1.calendar_exceptions_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.calendar_exceptions_events.deleted";
}

}

#endif
