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
#include "ores.trading.core/service/trade_envelope_portfolio_id_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

trade_envelope_portfolio_id_service::trade_envelope_portfolio_id_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::trade_envelope_portfolio_id>
trade_envelope_portfolio_id_service::list_trade_envelope_portfolio_ids(std::uint32_t offset,
                                                                       std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trade envelope portfolio identifiers";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t trade_envelope_portfolio_id_service::count_trade_envelope_portfolio_ids() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total trade envelope portfolio identifiers count";
    return repo_.get_total_trade_envelope_portfolio_id_count(ctx_);
}


std::optional<domain::trade_envelope_portfolio_id>
trade_envelope_portfolio_id_service::get_trade_envelope_portfolio_id_at_version(
    const std::string& trade_id, const std::string& sequence_number, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade envelope portfolio identifier at version. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number
                               << " version: " << version;
    return repo_.read_at_version(ctx_, trade_id, sequence_number, version);
}

std::optional<domain::trade_envelope_portfolio_id>
trade_envelope_portfolio_id_service::get_trade_envelope_portfolio_id(
    const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade envelope portfolio identifier. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    auto results = repo_.read_latest(ctx_, trade_id, sequence_number);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void trade_envelope_portfolio_id_service::save_trade_envelope_portfolio_id(
    const domain::trade_envelope_portfolio_id& v) {
    if (v.trade_id.is_nil())
        throw std::invalid_argument(
            "Trade Envelope Portfolio Identifier trade_id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade envelope portfolio identifier. "
                               << "trade_id: " << v.trade_id
                               << " sequence_number: " << v.sequence_number;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved trade envelope portfolio identifier. "
                              << "trade_id: " << v.trade_id
                              << " sequence_number: " << v.sequence_number;
}

void trade_envelope_portfolio_id_service::save_trade_envelope_portfolio_ids(
    const std::vector<domain::trade_envelope_portfolio_id>& trade_envelope_portfolio_ids) {
    for (const auto& e : trade_envelope_portfolio_ids) {
        if (e.trade_id.is_nil())
            throw std::invalid_argument(
                "Trade Envelope Portfolio Identifier trade_id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << trade_envelope_portfolio_ids.size()
                               << " trade envelope portfolio identifiers";
    auto ts = trade_envelope_portfolio_ids;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void trade_envelope_portfolio_id_service::delete_trade_envelope_portfolio_id(
    const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade envelope portfolio identifier. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    repo_.remove(ctx_, trade_id, sequence_number);
    BOOST_LOG_SEV(lg(), info) << "Removed trade envelope portfolio identifier. "
                              << "trade_id: " << trade_id
                              << " sequence_number: " << sequence_number;
}

void trade_envelope_portfolio_id_service::delete_trade_envelope_portfolio_ids(
    const std::vector<std::string>& trade_ids, const std::vector<std::string>& sequence_numbers) {
    repo_.remove(ctx_, trade_ids, sequence_numbers);
}

std::vector<domain::trade_envelope_portfolio_id>
trade_envelope_portfolio_id_service::get_trade_envelope_portfolio_id_history(
    const std::string& trade_id, const std::string& sequence_number) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for trade envelope portfolio identifier. "
                               << "trade_id: " << trade_id
                               << " sequence_number: " << sequence_number;
    return repo_.read_all(ctx_, trade_id, sequence_number);
}

}
