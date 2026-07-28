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
#include "ores.refdata.core/service/calendar_date_service.hpp"
#include <stdexcept>

namespace ores::refdata::service {

using namespace ores::logging;

calendar_date_service::calendar_date_service(context ctx)
    : repo_(ctx) {}

std::vector<domain::calendar_date> calendar_date_service::list_calendar_dates() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all calendar dates";
    return repo_.read_latest();
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

void calendar_date_service::save_calendar_date(const domain::calendar_date& calendar_date) {
    if (calendar_date.calendar_code.empty()) {
        throw std::invalid_argument("Calendar cannot be empty.");
    }
    if (!calendar_date.date.ok()) {
        throw std::invalid_argument("Date cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving calendar date: " << calendar_date.calendar_code << "/"
                               << calendar_date.date;
    repo_.write(calendar_date);
    BOOST_LOG_SEV(lg(), info) << "Saved calendar date: " << calendar_date.calendar_code << "/"
                              << calendar_date.date;
}

void calendar_date_service::remove_calendar_date(const std::string& calendar_code,
                                                 const std::string& date) {
    BOOST_LOG_SEV(lg(), debug) << "Removing calendar date: " << calendar_code << "/" << date;
    repo_.remove(calendar_code, date);
    BOOST_LOG_SEV(lg(), info) << "Removed calendar date: " << calendar_code << "/" << date;
}

}
