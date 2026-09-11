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
#include "ores.trading.core/service/instrument_schedule_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

instrument_schedule_service::instrument_schedule_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::instrument_schedule>
instrument_schedule_service::list_instrument_schedules(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all instrument schedules";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t instrument_schedule_service::count_instrument_schedules() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total instrument schedules count";
    return repo_.get_total_instrument_schedule_count(ctx_);
}


std::optional<domain::instrument_schedule>
instrument_schedule_service::get_instrument_schedule_at_version(const std::string& instrument_id,
                                                                const std::string& leg_role,
                                                                const std::string& leg_number,
                                                                const std::string& schedule_role,
                                                                std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting instrument schedule at version. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number
                               << " schedule_role: " << schedule_role << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, leg_role, leg_number, schedule_role, version);
}

std::optional<domain::instrument_schedule>
instrument_schedule_service::get_instrument_schedule(const std::string& instrument_id,
                                                     const std::string& leg_role,
                                                     const std::string& leg_number,
                                                     const std::string& schedule_role) {
    BOOST_LOG_SEV(lg(), debug) << "Getting instrument schedule. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number
                               << " schedule_role: " << schedule_role;
    auto results = repo_.read_latest(ctx_, instrument_id, leg_role, leg_number, schedule_role);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void instrument_schedule_service::save_instrument_schedule(const domain::instrument_schedule& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument("Instrument Schedule instrument_id cannot be empty.");
    if (v.leg_role.empty())
        throw std::invalid_argument("Instrument Schedule leg_role cannot be empty.");
    if (v.schedule_role.empty())
        throw std::invalid_argument("Instrument Schedule schedule_role cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving instrument schedule. "
                               << "instrument_id: " << v.instrument_id
                               << " leg_role: " << v.leg_role << " leg_number: " << v.leg_number
                               << " schedule_role: " << v.schedule_role;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved instrument schedule. "
                              << "instrument_id: " << v.instrument_id << " leg_role: " << v.leg_role
                              << " leg_number: " << v.leg_number
                              << " schedule_role: " << v.schedule_role;
}

void instrument_schedule_service::save_instrument_schedules(
    const std::vector<domain::instrument_schedule>& instrument_schedules) {
    for (const auto& e : instrument_schedules) {
        if (e.instrument_id.is_nil())
            throw std::invalid_argument("Instrument Schedule instrument_id cannot be empty.");
        if (e.leg_role.empty())
            throw std::invalid_argument("Instrument Schedule leg_role cannot be empty.");
        if (e.schedule_role.empty())
            throw std::invalid_argument("Instrument Schedule schedule_role cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << instrument_schedules.size()
                               << " instrument schedules";
    auto ts = instrument_schedules;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void instrument_schedule_service::delete_instrument_schedule(const std::string& instrument_id,
                                                             const std::string& leg_role,
                                                             const std::string& leg_number,
                                                             const std::string& schedule_role) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument schedule. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number
                               << " schedule_role: " << schedule_role;
    repo_.remove(ctx_, instrument_id, leg_role, leg_number, schedule_role);
    BOOST_LOG_SEV(lg(), info) << "Removed instrument schedule. "
                              << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                              << " leg_number: " << leg_number
                              << " schedule_role: " << schedule_role;
}

void instrument_schedule_service::delete_instrument_schedules(
    const std::vector<std::string>& instrument_ids,
    const std::vector<std::string>& leg_roles,
    const std::vector<std::string>& leg_numbers,
    const std::vector<std::string>& schedule_roles) {
    repo_.remove(ctx_, instrument_ids, leg_roles, leg_numbers, schedule_roles);
}

std::vector<domain::instrument_schedule>
instrument_schedule_service::get_instrument_schedule_history(const std::string& instrument_id,
                                                             const std::string& leg_role,
                                                             const std::string& leg_number,
                                                             const std::string& schedule_role) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for instrument schedule. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number
                               << " schedule_role: " << schedule_role;
    return repo_.read_all(ctx_, instrument_id, leg_role, leg_number, schedule_role);
}

}
