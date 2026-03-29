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
#include "ores.trading.core/service/composite_instrument_service.hpp"

#include <boost/uuid/random_generator.hpp>
#include "ores.service/messaging/handler_helpers.hpp"

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

composite_instrument_service::composite_instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::composite_instrument>
composite_instrument_service::list_composite_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all composite_instruments";
    return repo_.read_latest(ctx_);
}

std::vector<domain::composite_instrument>
composite_instrument_service::list_composite_instruments(std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing composite_instruments with offset="
                               << offset << ", limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t composite_instrument_service::count_composite_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Counting composite_instruments";
    return repo_.count_latest(ctx_);
}

std::optional<domain::composite_instrument>
composite_instrument_service::find_composite_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding composite_instrument: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

std::vector<domain::composite_leg>
composite_instrument_service::get_legs(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting legs for composite instrument: " << instrument_id;
    return leg_repo_.read_by_instrument(ctx_, instrument_id);
}

void composite_instrument_service::save_composite_instrument(
    const domain::composite_instrument& v,
    const std::vector<domain::composite_leg>& legs) {
    auto t = v;
    if (t.id.is_nil()) {
        static boost::uuids::random_generator gen;
        t.id = gen();
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving composite_instrument: " << t.id
                               << " with " << legs.size() << " legs";
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    for (auto& leg : legs) {
        auto l = leg;
        stamp(l, ctx_);
        leg_repo_.write(ctx_, l);
    }
    BOOST_LOG_SEV(lg(), info) << "Saved composite_instrument: " << t.id;
}

void composite_instrument_service::remove_composite_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing composite_instrument: " << id;
    leg_repo_.remove_by_instrument(ctx_, id);
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed composite_instrument: " << id;
}

std::vector<domain::composite_instrument>
composite_instrument_service::get_composite_instrument_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting history for composite_instrument: " << id;
    return repo_.read_all(ctx_, id);
}

}
