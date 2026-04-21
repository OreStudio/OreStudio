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
#include "ores.trading.core/service/credit_instrument_service.hpp"

#include <stdexcept>
#include "ores.service/messaging/handler_helpers.hpp"

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

credit_instrument_service::credit_instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::credit_instrument>
credit_instrument_service::list_credit_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all credit_instruments";
    return repo_.read_latest(ctx_);
}

std::vector<domain::credit_instrument>
credit_instrument_service::list_credit_instruments(std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing credit_instruments with offset="
                               << offset << ", limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t credit_instrument_service::count_credit_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Counting credit_instruments";
    return repo_.count_latest(ctx_);
}

std::optional<domain::credit_instrument>
credit_instrument_service::get_credit_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting credit_instrument: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void credit_instrument_service::save_credit_instrument(const domain::credit_instrument& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Credit instrument id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving credit_instrument: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved credit_instrument: " << t.id;
}

void credit_instrument_service::remove_credit_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing credit_instrument: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed credit_instrument: " << id;
}

std::vector<domain::credit_instrument>
credit_instrument_service::get_credit_instrument_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for credit_instrument: " << id;
    return repo_.read_all(ctx_, id);
}

}
