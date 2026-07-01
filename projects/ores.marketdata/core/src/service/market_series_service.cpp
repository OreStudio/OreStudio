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
#include "ores.marketdata.core/service/market_series_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::marketdata::service {

using namespace ores::logging;

market_series_service::market_series_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::market_series> market_series_service::list_market_series(std::uint32_t offset,
                                                                             std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all market series";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t market_series_service::count_market_series() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total market series count";
    return repo_.get_total_market_series_count(ctx_);
}

std::optional<domain::market_series>
market_series_service::get_market_series(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting market series: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void market_series_service::save_market_series(const domain::market_series& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Market Series id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving market series: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved market series: " << v.id;
}

void market_series_service::save_market_series(
    const std::vector<domain::market_series>& market_series) {
    for (const auto& e : market_series)
        if (e.id.is_nil())
            throw std::invalid_argument("Market Series id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << market_series.size() << " market series";
    auto ts = market_series;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void market_series_service::delete_market_series(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing market series: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed market series: " << id;
}

void market_series_service::delete_market_series(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::market_series>
market_series_service::get_market_series_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for market series: " << id;
    return repo_.read_all(ctx_, id);
}

}
