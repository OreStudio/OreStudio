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
#include "ores.refdata.core/service/calendar_event_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

calendar_event_service::calendar_event_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::calendar_event>
calendar_event_service::list_calendar_events(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all calendar events";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t calendar_event_service::count_calendar_events() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total calendar events count";
    return repo_.get_total_calendar_event_count(ctx_);
}


std::vector<domain::calendar_event> calendar_event_service::list_calendar_events_by_calendar_code(
    const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendar events by calendar_code: " << calendar_code;
    return repo_.read_latest_by_calendar_code(ctx_, calendar_code, offset, limit);
}

std::uint32_t
calendar_event_service::count_calendar_events_by_calendar_code(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total calendar events count by calendar_code: "
                               << calendar_code;
    return repo_.get_total_calendar_event_count_by_calendar_code(ctx_, calendar_code);
}

std::vector<domain::calendar_event>
calendar_event_service::list_calendar_events_by_calendar_code_as_of(
    const std::string& calendar_code,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendar events by calendar_code as of window: "
                               << calendar_code;
    return repo_.read_by_calendar_code_as_of(ctx_, calendar_code, valid_from_bound, valid_to_bound);
}
std::vector<domain::calendar_event>
calendar_event_service::list_calendar_events_by_diary_entry_type(
    const std::string& diary_entry_type, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendar events by diary_entry_type: "
                               << diary_entry_type;
    return repo_.read_latest_by_diary_entry_type(ctx_, diary_entry_type, offset, limit);
}

std::uint32_t calendar_event_service::count_calendar_events_by_diary_entry_type(
    const std::string& diary_entry_type) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total calendar events count by diary_entry_type: "
                               << diary_entry_type;
    return repo_.get_total_calendar_event_count_by_diary_entry_type(ctx_, diary_entry_type);
}

std::vector<domain::calendar_event>
calendar_event_service::list_calendar_events_by_diary_entry_type_as_of(
    const std::string& diary_entry_type,
    std::chrono::system_clock::time_point valid_from_bound,
    std::chrono::system_clock::time_point valid_to_bound) {
    BOOST_LOG_SEV(lg(), debug) << "Listing calendar events by diary_entry_type as of window: "
                               << diary_entry_type;
    return repo_.read_by_diary_entry_type_as_of(
        ctx_, diary_entry_type, valid_from_bound, valid_to_bound);
}
std::optional<domain::calendar_event>
calendar_event_service::get_calendar_event_at_version(const std::string& id,
                                                      std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting calendar event at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::calendar_event>
calendar_event_service::get_calendar_event(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting calendar event. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void calendar_event_service::save_calendar_event(const domain::calendar_event& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Calendar Event id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving calendar event. " << "id: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved calendar event. " << "id: " << v.id;
}

void calendar_event_service::save_calendar_events(
    const std::vector<domain::calendar_event>& calendar_events) {
    for (const auto& e : calendar_events) {
        if (e.id.is_nil())
            throw std::invalid_argument("Calendar Event id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << calendar_events.size() << " calendar events";
    auto ts = calendar_events;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void calendar_event_service::delete_calendar_event(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing calendar event. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed calendar event. " << "id: " << id;
}

void calendar_event_service::delete_calendar_events(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::calendar_event>
calendar_event_service::get_calendar_event_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for calendar event. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
