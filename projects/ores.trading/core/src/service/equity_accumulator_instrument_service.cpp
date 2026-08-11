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
#include "ores.trading.core/service/equity_accumulator_instrument_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

equity_accumulator_instrument_service::equity_accumulator_instrument_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::equity_accumulator_instrument>
equity_accumulator_instrument_service::list_equity_accumulator_instruments(std::uint32_t offset,
                                                                           std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all Equity Accumulator instruments";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t equity_accumulator_instrument_service::count_equity_accumulator_instruments() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total Equity Accumulator instruments count";
    return repo_.get_total_equity_accumulator_instrument_count(ctx_);
}


std::optional<domain::equity_accumulator_instrument>
equity_accumulator_instrument_service::get_equity_accumulator_instrument_at_version(
    const std::string& instrument_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting Equity Accumulator instrument at version. "
                               << "instrument_id: " << instrument_id << " version: " << version;
    return repo_.read_at_version(ctx_, instrument_id, version);
}

std::optional<domain::equity_accumulator_instrument>
equity_accumulator_instrument_service::get_equity_accumulator_instrument(
    const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting Equity Accumulator instrument. "
                               << "instrument_id: " << instrument_id;
    auto results = repo_.read_latest(ctx_, instrument_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

std::vector<domain::equity_accumulator_instrument>
equity_accumulator_instrument_service::get_equity_accumulator_instruments(
    const std::vector<std::string>& instrument_ids) {
    return repo_.read_latest(ctx_, instrument_ids);
}

void equity_accumulator_instrument_service::save_equity_accumulator_instrument(
    const domain::equity_accumulator_instrument& v) {
    if (v.identity.instrument_id.is_nil())
        throw std::invalid_argument("Equity Accumulator Instrument instrument_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving Equity Accumulator instrument. "
                               << "instrument_id: " << v.identity.instrument_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved Equity Accumulator instrument. "
                              << "instrument_id: " << v.identity.instrument_id;
}

void equity_accumulator_instrument_service::save_equity_accumulator_instruments(
    const std::vector<domain::equity_accumulator_instrument>& equity_accumulator_instruments) {
    for (const auto& e : equity_accumulator_instruments)
        if (e.identity.instrument_id.is_nil())
            throw std::invalid_argument(
                "Equity Accumulator Instrument instrument_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << equity_accumulator_instruments.size()
                               << " Equity Accumulator instruments";
    auto ts = equity_accumulator_instruments;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void equity_accumulator_instrument_service::delete_equity_accumulator_instrument(
    const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing Equity Accumulator instrument. "
                               << "instrument_id: " << instrument_id;
    repo_.remove(ctx_, instrument_id);
    BOOST_LOG_SEV(lg(), info) << "Removed Equity Accumulator instrument. "
                              << "instrument_id: " << instrument_id;
}

void equity_accumulator_instrument_service::delete_equity_accumulator_instruments(
    const std::vector<std::string>& instrument_ids) {
    repo_.remove(ctx_, instrument_ids);
}

std::vector<domain::equity_accumulator_instrument>
equity_accumulator_instrument_service::get_equity_accumulator_instrument_history(
    const std::string& instrument_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for Equity Accumulator instrument. "
                               << "instrument_id: " << instrument_id;
    return repo_.read_all(ctx_, instrument_id);
}

}
