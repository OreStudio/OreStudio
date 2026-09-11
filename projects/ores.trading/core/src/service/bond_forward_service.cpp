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
#include "ores.trading.core/service/bond_forward_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

bond_forward_service::bond_forward_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::bond_forward> bond_forward_service::list_bond_forwards(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all bond forwards";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t bond_forward_service::count_bond_forwards() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total bond forwards count";
    return repo_.get_total_bond_forward_count(ctx_);
}


std::optional<domain::bond_forward>
bond_forward_service::get_bond_forward_at_version(const std::string& instrument_id,
                                                  std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond forward at version. "
                               << "instrument_id: " << instrument_id << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, version);
}

std::optional<domain::bond_forward>
bond_forward_service::get_bond_forward(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond forward. " << "instrument_id: " << instrument_id;
    auto results = repo_.read_latest(ctx_, instrument_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void bond_forward_service::save_bond_forward(const domain::bond_forward& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument("Bond Forward instrument_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving bond forward. " << "instrument_id: " << v.instrument_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved bond forward. " << "instrument_id: " << v.instrument_id;
}

void bond_forward_service::save_bond_forwards(
    const std::vector<domain::bond_forward>& bond_forwards) {
    for (const auto& e : bond_forwards) {
        if (e.instrument_id.is_nil())
            throw std::invalid_argument("Bond Forward instrument_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << bond_forwards.size() << " bond forwards";
    auto ts = bond_forwards;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void bond_forward_service::delete_bond_forward(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond forward. " << "instrument_id: " << instrument_id;
    repo_.remove(ctx_, instrument_id);
    BOOST_LOG_SEV(lg(), info) << "Removed bond forward. " << "instrument_id: " << instrument_id;
}

void bond_forward_service::delete_bond_forwards(const std::vector<std::string>& instrument_ids) {
    repo_.remove(ctx_, instrument_ids);
}

std::vector<domain::bond_forward>
bond_forward_service::get_bond_forward_history(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for bond forward. "
                               << "instrument_id: " << instrument_id;
    return repo_.read_all(ctx_, instrument_id);
}

}
