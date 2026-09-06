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
#include "ores.trading.core/service/vanilla_swap_instrument_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

vanilla_swap_instrument_service::vanilla_swap_instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::vanilla_swap_instrument>
vanilla_swap_instrument_service::list_vanilla_swap_instruments(std::uint32_t offset,
                                                               std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all vanilla swap instruments";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t vanilla_swap_instrument_service::count_vanilla_swap_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total vanilla swap instruments count";
    return repo_.get_total_vanilla_swap_instrument_count(ctx_);
}


std::optional<domain::vanilla_swap_instrument>
vanilla_swap_instrument_service::get_vanilla_swap_instrument_at_version(
    const std::string& instrument_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting vanilla swap instrument at version. "
                               << "instrument_id: " << instrument_id << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, version);
}

std::optional<domain::vanilla_swap_instrument>
vanilla_swap_instrument_service::get_vanilla_swap_instrument(const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting vanilla swap instrument. "
                               << "instrument_id: " << instrument_id;
    auto results = repo_.read_latest(ctx_, instrument_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::vanilla_swap_instrument>
vanilla_swap_instrument_service::get_vanilla_swap_instruments(
    const std::vector<std::string>& instrument_ids) {
    return repo_.read_latest(ctx_, instrument_ids);
}

void vanilla_swap_instrument_service::save_vanilla_swap_instrument(
    const domain::vanilla_swap_instrument& v) {
    if (v.identity.instrument_id.is_nil())
        throw std::invalid_argument("Vanilla Swap Instrument instrument_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving vanilla swap instrument. "
                               << "instrument_id: " << v.identity.instrument_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved vanilla swap instrument. "
                              << "instrument_id: " << v.identity.instrument_id;
}

void vanilla_swap_instrument_service::save_vanilla_swap_instruments(
    const std::vector<domain::vanilla_swap_instrument>& vanilla_swap_instruments) {
    for (const auto& e : vanilla_swap_instruments) {
        if (e.identity.instrument_id.is_nil())
            throw std::invalid_argument("Vanilla Swap Instrument instrument_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << vanilla_swap_instruments.size()
                               << " vanilla swap instruments";
    auto ts = vanilla_swap_instruments;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void vanilla_swap_instrument_service::delete_vanilla_swap_instrument(
    const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing vanilla swap instrument. "
                               << "instrument_id: " << instrument_id;
    repo_.remove(ctx_, instrument_id);
    BOOST_LOG_SEV(lg(), info) << "Removed vanilla swap instrument. "
                              << "instrument_id: " << instrument_id;
}

void vanilla_swap_instrument_service::delete_vanilla_swap_instruments(
    const std::vector<std::string>& instrument_ids) {
    repo_.remove(ctx_, instrument_ids);
}

std::vector<domain::vanilla_swap_instrument>
vanilla_swap_instrument_service::get_vanilla_swap_instrument_history(
    const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for vanilla swap instrument. "
                               << "instrument_id: " << instrument_id;
    return repo_.read_all(ctx_, instrument_id);
}

}
