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
#include "ores.refdata.core/service/currency_pair_convention_calendar_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <stdexcept>
#include <utility>

namespace ores::refdata::service {

using namespace ores::logging;
using ores::service::messaging::stamp;


currency_pair_convention_calendar_service::currency_pair_convention_calendar_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx_) {}

std::vector<domain::currency_pair_convention_calendar>
currency_pair_convention_calendar_service::list_pair_convention_calendars() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency pair convention calendars";
    return repo_.read_latest();
}

std::vector<domain::currency_pair_convention_calendar>
currency_pair_convention_calendar_service::list_pair_convention_calendars(std::uint32_t offset,
                                                                          std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all currency pair convention calendars with offset: "
                               << offset << " limit: " << limit;
    return repo_.read_latest(offset, limit);
}

std::uint32_t
currency_pair_convention_calendar_service::get_total_pair_convention_calendar_count() {
    return repo_.get_total_pair_convention_calendar_count();
}

std::vector<domain::currency_pair_convention_calendar>
currency_pair_convention_calendar_service::list_pair_convention_calendars_by_pair(
    const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Listing currency pair convention calendars for pair: "
                               << pair_code;
    return repo_.read_latest_by_pair(pair_code);
}

std::vector<domain::currency_pair_convention_calendar>
currency_pair_convention_calendar_service::list_pair_convention_calendars_by_pair(
    const std::string& pair_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing currency pair convention calendars for pair: "
                               << pair_code << " offset: " << offset << " limit: " << limit;
    return repo_.read_latest_by_pair(pair_code, offset, limit);
}

std::uint32_t
currency_pair_convention_calendar_service::get_total_pair_convention_calendar_count_by_pair(
    const std::string& pair_code) {
    return repo_.get_total_pair_convention_calendar_count_by_pair(pair_code);
}

std::uint32_t
currency_pair_convention_calendar_service::get_total_pair_convention_calendar_count_by_calendar(
    const std::string& calendar_code) {
    return repo_.get_total_pair_convention_calendar_count_by_calendar(calendar_code);
}

void currency_pair_convention_calendar_service::save_pair_convention_calendar(
    const domain::currency_pair_convention_calendar& pair_convention_calendar) {
    if (pair_convention_calendar.pair_code.empty()) {
        throw std::invalid_argument("Pair cannot be empty.");
    }
    if (pair_convention_calendar.calendar_code.empty()) {
        throw std::invalid_argument("Calendar cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving currency pair convention calendar: "
                               << pair_convention_calendar.pair_code << "/"
                               << pair_convention_calendar.calendar_code;
    auto t = pair_convention_calendar;
    stamp(t, ctx_);
    repo_.write(t);
    BOOST_LOG_SEV(lg(), info) << "Saved currency pair convention calendar: "
                              << pair_convention_calendar.pair_code << "/"
                              << pair_convention_calendar.calendar_code;
}

void currency_pair_convention_calendar_service::remove_pair_convention_calendar(
    const std::string& pair_code, const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency pair convention calendar: " << pair_code << "/"
                               << calendar_code;
    repo_.remove(pair_code, calendar_code);
    BOOST_LOG_SEV(lg(), info) << "Removed currency pair convention calendar: " << pair_code << "/"
                              << calendar_code;
}

}
