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
#include "ores.marketdata.core/service/market_observation_service.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <cstdint>
#include <stdexcept>

using ores::service::messaging::stamp;

namespace ores::marketdata::service {

using namespace ores::logging;

market_observation_service::market_observation_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::market_observation>
market_observation_service::list_market_observations(std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing all market observations";
    return repo_.read_latest(ctx_, offset, limit);
}

std::uint32_t market_observation_service::count_market_observations() {
    BOOST_LOG_SEV(lg(), debug) << "Getting total market observations count";
    return repo_.get_total_market_observation_count(ctx_);
}

std::vector<domain::market_observation>
market_observation_service::list_market_observations_by_series_id(const std::string& series_id,
                                                                  std::uint32_t offset,
                                                                  std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Listing market observations by series_id: " << series_id;
    return repo_.read_latest_by_series_id(ctx_, series_id, offset, limit);
}

std::uint32_t
market_observation_service::count_market_observations_by_series_id(const std::string& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting total market observations count by series_id: "
                               << series_id;
    return repo_.get_total_market_observation_count_by_series_id(ctx_, series_id);
}


std::optional<domain::market_observation>
market_observation_service::get_market_observation(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting market observation: " << id;
    auto results = repo_.read_latest(ctx_, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void market_observation_service::save_market_observation(const domain::market_observation& v) {
    if (v.id.is_nil())
        throw std::invalid_argument("Market Observation id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving market observation: " << v.id;
    auto t = v;
    stamp(t, ctx_);
    repo_.write(ctx_, t);
    BOOST_LOG_SEV(lg(), info) << "Saved market observation: " << v.id;
}

void market_observation_service::save_market_observations(
    const std::vector<domain::market_observation>& market_observations) {
    for (const auto& e : market_observations)
        if (e.id.is_nil())
            throw std::invalid_argument("Market Observation id cannot be empty.");
    BOOST_LOG_SEV(lg(), debug) << "Saving " << market_observations.size() << " market observations";
    auto ts = market_observations;
    for (auto& e : ts)
        stamp(e, ctx_);
    repo_.write(ctx_, ts);
}

void market_observation_service::delete_market_observation(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing market observation: " << id;
    repo_.remove(ctx_, id);
    BOOST_LOG_SEV(lg(), info) << "Removed market observation: " << id;
}

void market_observation_service::delete_market_observations(const std::vector<std::string>& ids) {
    repo_.remove(ctx_, ids);
}

std::vector<domain::market_observation>
market_observation_service::get_market_observation_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for market observation: " << id;
    return repo_.read_all(ctx_, id);
}

}
