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
#ifndef ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CONVENTION_CALENDAR_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_CURRENCY_PAIR_CONVENTION_CALENDAR_PROTOCOL_HPP

#include "ores.refdata.api/domain/currency_pair_convention_calendar.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::refdata::messaging {

/**
 * @brief The currency pair convention calendar row enriched with the joined row's
 * display fields, so a screen needs one request for the whole set rather
 * than one per row. The by-side read returns this view.
 */
struct currency_pair_convention_calendar_view {
    ores::refdata::domain::currency_pair_convention_calendar currency_pair_convention_calendar;
};

struct get_currency_pair_convention_calendars_request {
    using response_type = struct get_currency_pair_convention_calendars_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_currency_pair_convention_calendars_response {
    std::vector<ores::refdata::domain::currency_pair_convention_calendar>
        currency_pair_convention_calendars;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct get_currency_pair_convention_calendars_by_pair_request {
    using response_type = struct get_currency_pair_convention_calendars_by_pair_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.list_by_pair_code";
    std::string pair_code;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
};

struct get_currency_pair_convention_calendars_by_pair_response {
    std::vector<currency_pair_convention_calendar_view> currency_pair_convention_calendars;
    int total_available_count = 0;
    bool success = false;
    std::string message;
};

struct save_currency_pair_convention_calendar_request {
    using response_type = struct save_currency_pair_convention_calendar_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.save";
    std::vector<ores::refdata::domain::currency_pair_convention_calendar>
        currency_pair_convention_calendars;

    static save_currency_pair_convention_calendar_request
    from(std::vector<ores::refdata::domain::currency_pair_convention_calendar> v) {
        return {.currency_pair_convention_calendars = std::move(v)};
    }
};

struct save_currency_pair_convention_calendar_response {
    bool success = false;
    std::string message;
};

struct delete_currency_pair_convention_calendar_request {
    using response_type = struct delete_currency_pair_convention_calendar_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.delete";
    std::vector<std::string> pair_codes;
    std::vector<std::string> calendar_codes;
};

struct delete_currency_pair_convention_calendar_response {
    bool success = false;
    std::string message;
};

struct count_currency_pair_convention_calendars_by_pair_request {
    using response_type = struct count_currency_pair_convention_calendars_by_pair_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.count_by_pair_code";
    std::string pair_code;
};

struct count_currency_pair_convention_calendars_by_pair_response {
    int total_available_count = 0;
};

struct count_currency_pair_convention_calendars_by_calendar_request {
    using response_type = struct count_currency_pair_convention_calendars_by_calendar_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.currency_pair_convention_calendars.count_by_calendar_code";
    std::string calendar_code;
};

struct count_currency_pair_convention_calendars_by_calendar_response {
    int total_available_count = 0;
};
}

#endif
