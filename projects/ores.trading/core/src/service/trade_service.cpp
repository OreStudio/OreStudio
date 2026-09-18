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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_service.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.trading.core/service/trade_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::trading::service {

using namespace ores::logging;

trade_service::trade_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::trade> trade_service::list_trades(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all trades";
    return repo_.read_latest(ctx_, offset, limit);
}

std::vector<domain::trade>
trade_service::list_trades(std::uint32_t offset, std::uint32_t limit, const std::string& node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Listing trades by node_id"
                               << " offset=" << offset << ", limit=" << limit;
    return repo_.read_latest_for_node_id(ctx_, offset, limit, node_id);
}

std::uint32_t trade_service::count_trades(const std::string& node_id) {
    BOOST_LOG_SEV(lg(), debug) << "Counting trades by node_id";
    return repo_.count_latest_for_node_id(ctx_, node_id);
}

std::uint32_t trade_service::count_trades() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total trades count";
    return repo_.get_total_trade_count(ctx_);
}


std::optional<domain::trade> trade_service::get_trade_at_version(const std::string& id,
                                                                 std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade at version. " << "id: " << id
                               << " version: " << version;
    return repo_.read_at_version(ctx_, id, version);
}

std::optional<domain::trade> trade_service::get_trade(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting trade. " << "id: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void trade_service::save_trade(const domain::trade& v) {
    if (v.identity.id.is_nil())
        throw std::invalid_argument("Trade id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving trade. " << "id: " << v.identity.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved trade. " << "id: " << v.identity.id;
}

void trade_service::save_trades(const std::vector<domain::trade>& trades) {
    for (const auto& e : trades) {
        if (e.identity.id.is_nil())
            throw std::invalid_argument("Trade id cannot be empty.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Saving " << trades.size() << " trades";
    auto ts = trades;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void trade_service::delete_trade(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade. " << "id: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed trade. " << "id: " << id;
}

void trade_service::delete_trades(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::trade> trade_service::get_trade_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for trade. " << "id: " << id;
    return repo_.read_all(ctx_, id);
}

}
