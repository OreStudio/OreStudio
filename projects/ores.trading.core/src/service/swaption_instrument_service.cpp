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
#include "ores.trading.core/service/swaption_instrument_service.hpp"

#include <algorithm>
#include <stdexcept>
#include "ores.service/messaging/handler_helpers.hpp"

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

swaption_instrument_service::swaption_instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::swaption_instrument>
swaption_instrument_service::list_swaption_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all swaption_instruments";
    return repo_.read_latest(ctx_);
}

std::vector<domain::swaption_instrument>
swaption_instrument_service::list_swaption_instruments(std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing swaption_instruments with offset="
                               << offset << ", limit=" << limit;
    auto all = repo_.read_latest(ctx_);
    const auto begin = std::min(static_cast<std::size_t>(offset), all.size());
    const auto end = std::min(begin + static_cast<std::size_t>(limit), all.size());
    return {all.begin() + begin, all.begin() + end};
}

std::uint32_t swaption_instrument_service::count_swaption_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Counting swaption_instruments";
    return static_cast<std::uint32_t>(repo_.read_latest(ctx_).size());
}

std::optional<domain::swaption_instrument>
swaption_instrument_service::find_swaption_instrument(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding swaption_instrument: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void swaption_instrument_service::save_swaption_instrument(
    const domain::swaption_instrument& v) {
    if (v.instrument_id.is_nil())
        throw std::invalid_argument("Swaption instrument id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving swaption_instrument: "
                               << v.instrument_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved swaption_instrument: "
                              << t.instrument_id;
}

void swaption_instrument_service::remove_swaption_instrument(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing swaption_instrument: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed swaption_instrument: " << id;
}

std::vector<domain::swaption_instrument>
swaption_instrument_service::get_swaption_instrument_history(
    const std::string& id) {
    BOOST_LOG_SEV(lg(), debug)
        << "Getting history for swaption_instrument: " << id;
    return repo_.read_all(ctx_, id);
}

}
