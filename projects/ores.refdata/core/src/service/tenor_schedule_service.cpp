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
#include "ores.refdata.core/service/tenor_schedule_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::refdata::service {

using namespace ores::logging;

tenor_schedule_service::tenor_schedule_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::tenor_schedule> tenor_schedule_service::list_schedules(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all tenor schedules";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t tenor_schedule_service::count_schedules() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenor schedules count";
    return repo_.get_total_schedule_count(ctx_);
}


std::vector<domain::tenor_schedule> tenor_schedule_service::list_schedules_by_calendar_code(
    const std::string& calendar_code, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing tenor schedules by calendar_code: " << calendar_code;
    return repo_.read_latest_by_calendar_code(ctx_, calendar_code, offset, limit);
}

std::uint32_t
tenor_schedule_service::count_schedules_by_calendar_code(const std::string& calendar_code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenor schedules count by calendar_code: "
                               << calendar_code;
    return repo_.get_total_schedule_count_by_calendar_code(ctx_, calendar_code);
}

std::vector<domain::tenor_schedule> tenor_schedule_service::list_schedules_by_diary_entry_type(
    const std::string& diary_entry_type, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing tenor schedules by diary_entry_type: "
                               << diary_entry_type;
    return repo_.read_latest_by_diary_entry_type(ctx_, diary_entry_type, offset, limit);
}

std::uint32_t
tenor_schedule_service::count_schedules_by_diary_entry_type(const std::string& diary_entry_type) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total tenor schedules count by diary_entry_type: "
                               << diary_entry_type;
    return repo_.get_total_schedule_count_by_diary_entry_type(ctx_, diary_entry_type);
}

std::optional<domain::tenor_schedule>
tenor_schedule_service::get_schedule_at_version(const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor schedule at version. " << "code: " << code
                               << " version: " << version;
    return repo_.read_at_version(ctx_, code, version);
}

std::optional<domain::tenor_schedule>
tenor_schedule_service::get_schedule(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting tenor schedule. " << "code: " << code;
    auto results = repo_.read_latest(ctx_, code);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void tenor_schedule_service::save_schedule(const domain::tenor_schedule& v) {
    if (v.code.empty())
        throw std::invalid_argument("Tenor Schedule code cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving tenor schedule. " << "code: " << v.code;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved tenor schedule. " << "code: " << v.code;
}

void tenor_schedule_service::save_schedules(const std::vector<domain::tenor_schedule>& schedules) {
    for (const auto& e : schedules) {
        if (e.code.empty())
            throw std::invalid_argument("Tenor Schedule code cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << schedules.size() << " tenor schedules";
    auto ts = schedules;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void tenor_schedule_service::delete_schedule(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor schedule. " << "code: " << code;
    repo_.remove(ctx_, code);
    BOOST_LOG_SEV(lg(), info) << "Removed tenor schedule. " << "code: " << code;
}

void tenor_schedule_service::delete_schedules(const std::vector<std::string>& codes) {
    repo_.remove(ctx_, codes);
}

std::vector<domain::tenor_schedule>
tenor_schedule_service::get_schedule_history(const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for tenor schedule. " << "code: " << code;
    return repo_.read_all(ctx_, code);
}

}
