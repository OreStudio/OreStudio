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
#include "ores.trading/service/trade_service.hpp"

#include <stdexcept>
#include <boost/uuid/uuid.hpp>

namespace ores::trading::service {

using namespace ores::logging;

trade_service::trade_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::trade> trade_service::list_trades() {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trades";
    return repo_.read_latest(ctx_);
}

std::vector<domain::trade>
trade_service::list_trades(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing trades with offset=" << offset
                               << ", limit=" << limit;
    return repo_.read_latest(ctx_, offset, limit);
}

std::vector<domain::trade>
trade_service::list_trades_filtered(std::uint32_t offset, std::uint32_t limit,
    std::optional<boost::uuids::uuid> book_id,
    std::optional<boost::uuids::uuid> portfolio_id,
    std::optional<boost::uuids::uuid> business_unit_id) {
    BOOST_LOG_SEV(lg(), debug) << "Listing trades (filtered) offset=" << offset
                               << ", limit=" << limit;
    return repo_.read_latest_filtered(
        ctx_, offset, limit, book_id, portfolio_id, business_unit_id);
}

std::uint32_t trade_service::count_trades() {
    BOOST_LOG_SEV(lg(), debug) << "Counting trades";
    return repo_.count_latest(ctx_);
}

std::uint32_t trade_service::count_trades_filtered(
    std::optional<boost::uuids::uuid> book_id,
    std::optional<boost::uuids::uuid> portfolio_id,
    std::optional<boost::uuids::uuid> business_unit_id) {
    BOOST_LOG_SEV(lg(), debug) << "Counting trades (filtered)";
    return repo_.count_latest_filtered(ctx_, book_id, portfolio_id, business_unit_id);
}

std::optional<domain::trade>
trade_service::find_trade(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding trade: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty()) return std::nullopt;
    return results.front();
}

void trade_service::save_trade(const domain::trade& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Trade id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade: " << v.id;
    repo_.write(ctx_, v);
    BOOST_LOG_SEV(lg(), info) << "Saved trade: " << v.id;
}

void trade_service::remove_trade(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed trade: " << id;
}

std::vector<domain::trade>
trade_service::get_trade_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for trade: " << id;
    return repo_.read_all(ctx_, id);
}

}
