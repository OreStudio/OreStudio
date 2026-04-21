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
#include "ores.trading.core/service/commodity_instrument_service.hpp"

#include <boost/uuid/random_generator.hpp>
#include "ores.service/messaging/handler_helpers.hpp"

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

commodity_instrument_service::commodity_instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::commodity_instrument>
commodity_instrument_service::list_commodity_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all commodity_instruments";
    return repo_.read_latest(ctx_);
}

std::vector<domain::commodity_instrument>
commodity_instrument_service::list_commodity_instruments(std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing commodity_instruments with offset="
                               << offset << ", limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t commodity_instrument_service::count_commodity_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Counting commodity_instruments";
    return repo_.count_latest(ctx_);
}

std::optional<domain::commodity_instrument>
commodity_instrument_service::get_commodity_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting commodity_instrument: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void commodity_instrument_service::save_commodity_instrument(const domain::commodity_instrument& v) {
    auto t = v;
    if (t.id.is_nil()) {
        boost::uuids::random_generator gen;
        t.id = gen();
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving commodity_instrument: " << t.id;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved commodity_instrument: " << t.id;
}

void commodity_instrument_service::remove_commodity_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing commodity_instrument: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed commodity_instrument: " << id;
}

std::vector<domain::commodity_instrument>
commodity_instrument_service::get_commodity_instrument_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for commodity_instrument: " << id;
    return repo_.read_all(ctx_, id);
}

}
