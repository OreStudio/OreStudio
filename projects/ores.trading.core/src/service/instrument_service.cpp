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
#include "ores.trading.core/service/instrument_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/nil_generator.hpp>
#include "ores.service/messaging/handler_helpers.hpp"

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

instrument_service::instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::instrument> instrument_service::list_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all instruments";
    return repo_.read_latest(ctx_);
}

std::vector<domain::instrument>
instrument_service::list_instruments(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing instruments with offset=" << offset
                               << ", limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t instrument_service::count_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Counting instruments";
    return repo_.count_latest(ctx_);
}

std::optional<domain::instrument>
instrument_service::find_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding instrument: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

std::vector<domain::swap_leg>
instrument_service::get_legs(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting legs for instrument: " << instrument_id;
    return leg_repo_.read_by_instrument(ctx_, instrument_id);
}

void instrument_service::save_instrument(const domain::instrument& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Instrument id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving instrument: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved instrument: " << t.id;
}

void instrument_service::save_instrument(const domain::instrument& v,
    const std::vector<domain::swap_leg>& legs) {
    if (v.id.is_nil())
        throw std::invalid_argument("Instrument id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving instrument: " << v.id
                               << " with " << legs.size() << " legs";
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    for (auto& leg : legs) {
        auto l = leg;
        stamp(l, ctx_);
        leg_repo_.write(ctx_, l);
    }
    BOOST_LOG_SEV(lg(), info) << "Saved instrument: " << t.id;
}

void instrument_service::remove_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing instrument: " << id;
    leg_repo_.remove_by_instrument(ctx_, id);
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed instrument: " << id;
}

std::vector<domain::instrument>
instrument_service::get_instrument_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for instrument: " << id;
    return repo_.read_all(ctx_, id);
}

}
