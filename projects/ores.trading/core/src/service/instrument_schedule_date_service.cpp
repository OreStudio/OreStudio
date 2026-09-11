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
#include "ores.trading.core/service/instrument_schedule_date_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

instrument_schedule_date_service::instrument_schedule_date_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::instrument_schedule_date>
instrument_schedule_date_service::list_instrument_schedule_dates(std::uint32_t offset,
                                                                 std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all instrument schedule dates";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t instrument_schedule_date_service::count_instrument_schedule_dates() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total instrument schedule dates count";
    return repo_.get_total_instrument_schedule_date_count(ctx_);
}


std::optional<domain::instrument_schedule_date>
instrument_schedule_date_service::get_instrument_schedule_date_at_version(
    const std::string& instrument_id,
    const std::string& owner_role,
    const std::string& owner_number,
    const std::string& schedule_role,
    const std::string& schedule_sequence_number,
    const std::string& sequence_number,
    std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting instrument schedule date at version. "
                               << "instrument_id: " << instrument_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " schedule_sequence_number: " << schedule_sequence_number
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_,
                                 instrument_id,
                                 owner_role,
                                 owner_number,
                                 schedule_role,
                                 schedule_sequence_number,
                                 sequence_number,
                                 version);
}

std::optional<domain::instrument_schedule_date>
instrument_schedule_date_service::get_instrument_schedule_date(
    const std::string& instrument_id,
    const std::string& owner_role,
    const std::string& owner_number,
    const std::string& schedule_role,
    const std::string& schedule_sequence_number,
    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting instrument schedule date. "
                               << "instrument_id: " << instrument_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " schedule_sequence_number: " << schedule_sequence_number
                               << " sequence_number: " << sequence_number;
    auto results = repo_.read_latest(ctx_,
                                     instrument_id,
                                     owner_role,
                                     owner_number,
                                     schedule_role,
                                     schedule_sequence_number,
                                     sequence_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void instrument_schedule_date_service::save_instrument_schedule_date(
    const domain::instrument_schedule_date& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument("Instrument Schedule Date instrument_id cannot be empty.");
    if (v.owner_role.empty())
        throw std::invalid_argument("Instrument Schedule Date owner_role cannot be empty.");
    if (v.schedule_role.empty())
        throw std::invalid_argument("Instrument Schedule Date schedule_role cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving instrument schedule date. "
                               << "instrument_id: " << v.instrument_id
                               << " owner_role: " << v.owner_role
                               << " owner_number: " << v.owner_number
                               << " schedule_role: " << v.schedule_role
                               << " schedule_sequence_number: " << v.schedule_sequence_number
                               << " sequence_number: " << v.sequence_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved instrument schedule date. "
                              << "instrument_id: " << v.instrument_id
                              << " owner_role: " << v.owner_role
                              << " owner_number: " << v.owner_number
                              << " schedule_role: " << v.schedule_role
                              << " schedule_sequence_number: " << v.schedule_sequence_number
                              << " sequence_number: " << v.sequence_number;
}

void instrument_schedule_date_service::save_instrument_schedule_dates(
    const std::vector<domain::instrument_schedule_date>& instrument_schedule_dates) {
    for (const auto& e : instrument_schedule_dates) {
        if (e.instrument_id.is_nil())
            throw std::invalid_argument("Instrument Schedule Date instrument_id cannot be empty.");
        if (e.owner_role.empty())
            throw std::invalid_argument("Instrument Schedule Date owner_role cannot be empty.");
        if (e.schedule_role.empty())
            throw std::invalid_argument("Instrument Schedule Date schedule_role cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << instrument_schedule_dates.size()
                               << " instrument schedule dates";
    auto ts = instrument_schedule_dates;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void instrument_schedule_date_service::delete_instrument_schedule_date(
    const std::string& instrument_id,
    const std::string& owner_role,
    const std::string& owner_number,
    const std::string& schedule_role,
    const std::string& schedule_sequence_number,
    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument schedule date. "
                               << "instrument_id: " << instrument_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " schedule_sequence_number: " << schedule_sequence_number
                               << " sequence_number: " << sequence_number;
    repo_.remove(ctx_,
                 instrument_id,
                 owner_role,
                 owner_number,
                 schedule_role,
                 schedule_sequence_number,
                 sequence_number);
    BOOST_LOG_SEV(lg(), info) << "Removed instrument schedule date. "
                              << "instrument_id: " << instrument_id << " owner_role: " << owner_role
                              << " owner_number: " << owner_number
                              << " schedule_role: " << schedule_role
                              << " schedule_sequence_number: " << schedule_sequence_number
                              << " sequence_number: " << sequence_number;
}

void instrument_schedule_date_service::delete_instrument_schedule_dates(
    const std::vector<std::string>& instrument_ids,
    const std::vector<std::string>& owner_roles,
    const std::vector<std::string>& owner_numbers,
    const std::vector<std::string>& schedule_roles,
    const std::vector<std::string>& schedule_sequence_numbers,
    const std::vector<std::string>& sequence_numbers) {
    repo_.remove(ctx_,
                 instrument_ids,
                 owner_roles,
                 owner_numbers,
                 schedule_roles,
                 schedule_sequence_numbers,
                 sequence_numbers);
}

std::vector<domain::instrument_schedule_date>
instrument_schedule_date_service::get_instrument_schedule_date_history(
    const std::string& instrument_id,
    const std::string& owner_role,
    const std::string& owner_number,
    const std::string& schedule_role,
    const std::string& schedule_sequence_number,
    const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for instrument schedule date. "
                               << "instrument_id: " << instrument_id
                               << " owner_role: " << owner_role << " owner_number: " << owner_number
                               << " schedule_role: " << schedule_role
                               << " schedule_sequence_number: " << schedule_sequence_number
                               << " sequence_number: " << sequence_number;
    return repo_.read_all(ctx_,
                          instrument_id,
                          owner_role,
                          owner_number,
                          schedule_role,
                          schedule_sequence_number,
                          sequence_number);
}

}
