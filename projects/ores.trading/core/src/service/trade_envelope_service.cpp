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
#include "ores.trading.core/service/trade_envelope_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

trade_envelope_service::trade_envelope_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::trade_envelope>
trade_envelope_service::list_trade_envelopes(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trade envelopes";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t trade_envelope_service::count_trade_envelopes() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total trade envelopes count";
    return repo_.get_total_trade_envelope_count(ctx_);
}


std::optional<domain::trade_envelope>
trade_envelope_service::get_trade_envelope_at_version(const std::string& trade_id,
                                                      std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade envelope at version. " << "trade_id: " << trade_id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, trade_id, version);
}

std::optional<domain::trade_envelope>
trade_envelope_service::get_trade_envelope(const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade envelope. " << "trade_id: " << trade_id;
    auto results = repo_.read_latest(ctx_, trade_id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void trade_envelope_service::save_trade_envelope(const domain::trade_envelope& v) {
    if (v.trade_id.is_nil())
        throw std::invalid_argument("Trade Envelope trade_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade envelope. " << "trade_id: " << v.trade_id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved trade envelope. " << "trade_id: " << v.trade_id;
}

void trade_envelope_service::save_trade_envelopes(
    const std::vector<domain::trade_envelope>& trade_envelopes) {
    for (const auto& e : trade_envelopes) {
        if (e.trade_id.is_nil())
            throw std::invalid_argument("Trade Envelope trade_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << trade_envelopes.size() << " trade envelopes";
    auto ts = trade_envelopes;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void trade_envelope_service::delete_trade_envelope(const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade envelope. " << "trade_id: " << trade_id;
    repo_.remove(ctx_, trade_id);
    BOOST_LOG_SEV(lg(), info) << "Removed trade envelope. " << "trade_id: " << trade_id;
}

void trade_envelope_service::delete_trade_envelopes(const std::vector<std::string>& trade_ids) {
    repo_.remove(ctx_, trade_ids);
}

std::vector<domain::trade_envelope>
trade_envelope_service::get_trade_envelope_history(const std::string& trade_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for trade envelope. "
                               << "trade_id: " << trade_id;
    return repo_.read_all(ctx_, trade_id);
}

}
