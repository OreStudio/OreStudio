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
#include "ores.trading.core/service/bond_instrument_service.hpp"

#include <stdexcept>
#include "ores.service/messaging/handler_helpers.hpp"

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

bond_instrument_service::bond_instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::bond_instrument>
bond_instrument_service::list_bond_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all bond_instruments";
    return repo_.read_latest(ctx_);
}

std::vector<domain::bond_instrument>
bond_instrument_service::list_bond_instruments(std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing bond_instruments with offset="
                               << offset << ", limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t bond_instrument_service::count_bond_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Counting bond_instruments";
    return repo_.count_latest(ctx_);
}

std::optional<domain::bond_instrument>
bond_instrument_service::get_bond_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting bond_instrument: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void bond_instrument_service::save_bond_instrument(const domain::bond_instrument& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Bond instrument id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving bond_instrument: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved bond_instrument: " << t.id;
}

void bond_instrument_service::remove_bond_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond_instrument: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed bond_instrument: " << id;
}

std::vector<domain::bond_instrument>
bond_instrument_service::get_bond_instrument_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for bond_instrument: " << id;
    return repo_.read_all(ctx_, id);
}

}
