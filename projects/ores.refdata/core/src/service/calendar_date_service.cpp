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
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/service/calendar_date_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <stdexcept>
#include <utility>

namespace ores::refdata::service {

using namespace ores::logging;
using ores::service::messaging::stamp;


calendar_date_service::calendar_date_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx_) {}

std::vector<domain::calendar_date> calendar_date_service::list_calendar_dates() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all calendar dates";
    return repo_.read_latest();
}

std::vector<domain::calendar_date> calendar_date_service::list_calendar_dates(std::uint32_t offset,
                                                                              std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all calendar dates with offset: " << offset
                               << " limit: " << limit;
    return repo_.read_latest(offset, limit);
}

std::uint32_t calendar_date_service::get_total_calendar_date_count() {
    return repo_.get_total_calendar_date_count();
}

std::vector<domain::calendar_date>
calendar_date_service::list_calendar_dates_by_calendar(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendar dates for calendar: " << calendar_code;
    return repo_.read_latest_by_calendar(calendar_code);
}

std::vector<domain::calendar_date> calendar_date_service::list_calendar_dates_by_calendar(
    const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendar dates for calendar: " << calendar_code
                               << " offset: " << offset << " limit: " << limit;
    return repo_.read_latest_by_calendar(calendar_code, offset, limit);
}

std::uint32_t
calendar_date_service::get_total_calendar_date_count_by_calendar(const std::string& calendar_code) {
    return repo_.get_total_calendar_date_count_by_calendar(calendar_code);
}

std::uint32_t
calendar_date_service::get_total_calendar_date_count_by_date(const std::string& date) {
    return repo_.get_total_calendar_date_count_by_date(date);
}

}
