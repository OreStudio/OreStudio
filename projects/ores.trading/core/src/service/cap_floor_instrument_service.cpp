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
#include "ores.trading.core/service/cap_floor_instrument_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

cap_floor_instrument_service::cap_floor_instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::cap_floor_instrument>
cap_floor_instrument_service::list_cap_floor_instruments(std::uint32_t offset,
                                                         std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all cap/floor instruments";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t cap_floor_instrument_service::count_cap_floor_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total cap/floor instruments count";
    return repo_.get_total_cap_floor_instrument_count(ctx_);
}


std::optional<domain::cap_floor_instrument>
cap_floor_instrument_service::get_cap_floor_instrument_at_version(const std::string& instrument_id,
                                                                  std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting cap/floor instrument at version. "
                               << "instrument_id: " << instrument_id << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, version);
}

std::optional<domain::cap_floor_instrument>
cap_floor_instrument_service::get_cap_floor_instrument(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting cap/floor instrument. "
                               << "instrument_id: " << instrument_id;
    auto results = repo_.read_latest(ctx_, instrument_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::cap_floor_instrument> cap_floor_instrument_service::get_cap_floor_instruments(
    const std::vector<std::string>& instrument_ids) {
    return repo_.read_latest(ctx_, instrument_ids);
}

void cap_floor_instrument_service::save_cap_floor_instrument(
    const domain::cap_floor_instrument& v) {
    if (v.identity.instrument_id.is_nil())
        throw std::invalid_argument("Cap/Floor Instrument instrument_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving cap/floor instrument. "
                               << "instrument_id: " << v.identity.instrument_id;
    auto t = v;
    stamp(t.identity, ctx_);
    stamp(t.audit, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved cap/floor instrument. "
                              << "instrument_id: " << v.identity.instrument_id;
}

void cap_floor_instrument_service::save_cap_floor_instruments(
    const std::vector<domain::cap_floor_instrument>& cap_floor_instruments) {
    for (const auto& e : cap_floor_instruments) {
        if (e.identity.instrument_id.is_nil())
            throw std::invalid_argument("Cap/Floor Instrument instrument_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << cap_floor_instruments.size()
                               << " cap/floor instruments";
    auto ts = cap_floor_instruments;
    for (auto& e : ts) {
        stamp(e.identity, ctx_);
        stamp(e.audit, ctx_);
    }
    repo_.write(ctx_, ts);
}

void cap_floor_instrument_service::delete_cap_floor_instrument(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing cap/floor instrument. "
                               << "instrument_id: " << instrument_id;
    repo_.remove(ctx_, instrument_id);
    BOOST_LOG_SEV(lg(), info) << "Removed cap/floor instrument. "
                              << "instrument_id: " << instrument_id;
}

void cap_floor_instrument_service::delete_cap_floor_instruments(
    const std::vector<std::string>& instrument_ids) {
    repo_.remove(ctx_, instrument_ids);
}

std::vector<domain::cap_floor_instrument>
cap_floor_instrument_service::get_cap_floor_instrument_history(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for cap/floor instrument. "
                               << "instrument_id: " << instrument_id;
    return repo_.read_all(ctx_, instrument_id);
}

}
