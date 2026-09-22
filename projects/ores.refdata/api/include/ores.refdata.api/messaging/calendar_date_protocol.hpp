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
#ifndef ORES_REFDATA_API_MESSAGING_CALENDAR_DATE_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CALENDAR_DATE_PROTOCOL_HPP

#include "ores.refdata.api/domain/calendar_date.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

struct calendar_date_key {
    std::string calendar_code;
    std::chrono::year_month_day date;
};

struct calendar_date_lookup {
    calendar_date_key key;
    std::optional<ores::refdata::domain::calendar_date> calendar_date;
};

struct calendar_dates_filter {
    std::optional<std::string> calendar_code;
};

struct calendar_date_event {
    boost::uuids::uuid event_id;
    calendar_date_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct list_calendar_dates_request {
    using response_type = struct list_calendar_dates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.list";
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
    std::optional<calendar_dates_filter> filter;
};

struct list_calendar_dates_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_date> calendar_dates;
    std::uint64_t total;
};

struct get_calendar_date_request {
    using response_type = struct get_calendar_date_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    calendar_date_key key;
};

struct get_calendar_date_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::calendar_date> calendar_date;
};

struct get_many_calendar_dates_request {
    using response_type = struct get_many_calendar_dates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<calendar_date_key> keys;
};

struct get_many_calendar_dates_response {
    ores::utility::domain::result result;
    std::vector<calendar_date_lookup> entries;
};

struct list_by_calendar_code_calendar_dates_request {
    using response_type = struct list_by_calendar_code_calendar_dates_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.calendar_dates.list_by_calendar_code";
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
    std::optional<calendar_dates_filter> filter;
};

struct list_by_calendar_code_calendar_dates_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::calendar_date> calendar_dates;
    std::uint64_t total;
};

}

#endif
