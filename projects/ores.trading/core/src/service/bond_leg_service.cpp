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
#include "ores.trading.core/service/bond_leg_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

bond_leg_service::bond_leg_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::bond_leg> bond_leg_service::list_bond_legs(std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all bond legs";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t bond_leg_service::count_bond_legs() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total bond legs count";
    return repo_.get_total_bond_leg_count(ctx_);
}


std::optional<domain::bond_leg>
bond_leg_service::get_bond_leg_at_version(const std::string& instrument_id,
                                          const std::string& leg_role,
                                          const std::string& leg_number,
                                          std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond leg at version. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, leg_role, leg_number, version);
}

std::optional<domain::bond_leg> bond_leg_service::get_bond_leg(const std::string& instrument_id,
                                                               const std::string& leg_role,
                                                               const std::string& leg_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond leg. " << "instrument_id: " << instrument_id
                               << " leg_role: " << leg_role << " leg_number: " << leg_number;
    auto results = repo_.read_latest(ctx_, instrument_id, leg_role, leg_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void bond_leg_service::save_bond_leg(const domain::bond_leg& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument("Bond Leg instrument_id cannot be empty.");
    if (v.leg_role.empty())
        throw std::invalid_argument("Bond Leg leg_role cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving bond leg. " << "instrument_id: " << v.instrument_id
                               << " leg_role: " << v.leg_role << " leg_number: " << v.leg_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved bond leg. " << "instrument_id: " << v.instrument_id
                              << " leg_role: " << v.leg_role << " leg_number: " << v.leg_number;
}

void bond_leg_service::save_bond_legs(const std::vector<domain::bond_leg>& bond_legs) {
    for (const auto& e : bond_legs) {
        if (e.instrument_id.is_nil())
            throw std::invalid_argument("Bond Leg instrument_id cannot be empty.");
        if (e.leg_role.empty())
            throw std::invalid_argument("Bond Leg leg_role cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << bond_legs.size() << " bond legs";
    auto ts = bond_legs;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void bond_leg_service::delete_bond_leg(const std::string& instrument_id,
                                       const std::string& leg_role,
                                       const std::string& leg_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond leg. " << "instrument_id: " << instrument_id
                               << " leg_role: " << leg_role << " leg_number: " << leg_number;
    repo_.remove(ctx_, instrument_id, leg_role, leg_number);
    BOOST_LOG_SEV(lg(), info) << "Removed bond leg. " << "instrument_id: " << instrument_id
                              << " leg_role: " << leg_role << " leg_number: " << leg_number;
}

void bond_leg_service::delete_bond_legs(const std::vector<std::string>& instrument_ids,
                                        const std::vector<std::string>& leg_roles,
                                        const std::vector<std::string>& leg_numbers) {
    repo_.remove(ctx_, instrument_ids, leg_roles, leg_numbers);
}

std::vector<domain::bond_leg> bond_leg_service::get_bond_leg_history(
    const std::string& instrument_id, const std::string& leg_role, const std::string& leg_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for bond leg. "
                               << "instrument_id: " << instrument_id << " leg_role: " << leg_role
                               << " leg_number: " << leg_number;
    return repo_.read_all(ctx_, instrument_id, leg_role, leg_number);
}

}
