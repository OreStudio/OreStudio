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
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief The calendar date row enriched with the joined row's
 * display fields, so a screen needs one request for the whole set rather
 * than one per row. The by-side read returns this view.
 */
struct calendar_date_view {
    ores::refdata::domain::calendar_date calendar_date;
};

struct get_calendar_dates_request {
    using response_type = struct get_calendar_dates_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_calendar_dates_response {
    std::vector<ores::refdata::domain::calendar_date> calendar_dates;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct get_calendar_dates_by_calendar_request {
    using response_type = struct get_calendar_dates_by_calendar_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.calendar_dates.list_by_calendar_code";
    std::string calendar_code;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_calendar_dates_by_calendar_response {
    std::vector<calendar_date_view> calendar_dates;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct count_calendar_dates_by_calendar_request {
    using response_type = struct count_calendar_dates_by_calendar_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.calendar_dates.count_by_calendar_code";
    std::string calendar_code;
};

struct count_calendar_dates_by_calendar_response {
    int total_available_count = 0;
};

struct count_calendar_dates_by_date_request {
    using response_type = struct count_calendar_dates_by_date_response;
    static constexpr std::string_view nats_subject = "refdata.v1.calendar_dates.count_by_date";
    std::string date;
};

struct count_calendar_dates_by_date_response {
    int total_available_count = 0;
};
}

#endif
