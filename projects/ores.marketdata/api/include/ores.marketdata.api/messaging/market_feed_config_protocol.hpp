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
#ifndef ORES_MARKETDATA_API_MESSAGING_MARKET_FEED_CONFIG_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_MARKET_FEED_CONFIG_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief Request to start a synthetic market data feed.
 *
 * Fields carry the full GMM process configuration so the caller controls
 * the feed parameters. All fields carry PoC defaults matching the
 * hard-coded EUR/USD values used in the initial implementation.
 *
 * PoC scope: only a single EUR/USD GMM feed is supported. ore_key is
 * informational; the handler ignores it and always uses the pre-resolved
 * series_id from the service context.
 */
struct start_market_feed_config_request {
    using response_type = struct start_market_feed_config_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_feed_configs.start";

    std::string ore_key = "FX/RATE/EUR/USD";
    std::vector<double> gmm_means = {-0.0001, 0.0, 0.0001};
    std::vector<double> gmm_stdevs = {0.0010, 0.0005, 0.0010};
    std::vector<double> gmm_weights = {0.2, 0.6, 0.2};
    double gmm_initial_price = 1.0800;
    double ticks_per_hour = 12.0;
    std::string process_type = "geometric"; // "geometric" (GBM) or "arithmetic"
};

struct start_market_feed_config_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Request to stop a running synthetic market data feed.
 *
 * PoC scope: ore_key is informational; the handler stops whatever feed
 * is currently running regardless of the supplied key.
 */
struct stop_market_feed_config_request {
    using response_type = struct stop_market_feed_config_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_feed_configs.stop";

    std::string ore_key; // empty = stop all running feeds
};

struct stop_market_feed_config_response {
    bool success = false;
    std::string message;
};

}

#endif
