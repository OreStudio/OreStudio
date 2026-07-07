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
 * Fields carry the full GMM process configuration so the caller controls the
 * feed parameters; the defaults are a convenience for ad-hoc EUR/USD starts.
 * The feed is keyed by source_name and its market series is resolved per feed
 * from ore_key, so many feeds (including two for the same pair) run at once.
 */
struct start_market_feed_config_request {
    using response_type = struct start_market_feed_config_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_feed_configs.start";

    std::string ore_key = "FX/RATE/EUR/USD";
    // Unique producer identity; the feed is keyed by this and publishes on
    // "synthetic.v1.tick.<source_name>". Lets two producers for the same pair
    // coexist. Defaults to ore_key when empty.
    std::string source_name;
    std::vector<double> gmm_means = {-0.0001, 0.0, 0.0001};
    std::vector<double> gmm_stdevs = {0.0010, 0.0005, 0.0010};
    std::vector<double> gmm_weights = {0.2, 0.6, 0.2};
    double gmm_initial_price = 1.0800;
    double ticks_per_hour = 12.0;
    std::string process_type = "geometric"; // "geometric" (GBM) or "arithmetic"

    // Vintage-availability guard: the (source, date) of the reference market
    // data this feed's initial spot depends on. Checked at start() against
    // market_observation before the feed is actually started; empty
    // vintage_source skips the check (e.g. ad-hoc/default requests).
    std::string vintage_source;
    std::string vintage_date; // ISO format, e.g. "2016-02-05"
};

struct start_market_feed_config_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Request to check whether a feed's required vintage data exists,
 * without starting it. Powers the Market Simulator "validate all" action.
 */
struct validate_market_feed_config_request {
    using response_type = struct validate_market_feed_config_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_feed_configs.validate";

    std::string ore_key;
    std::string source_name;
    std::string vintage_source;
    std::string vintage_date; // ISO format, e.g. "2016-02-05"
};

struct validate_market_feed_config_response {
    bool success = false; // request-level success (malformed body, etc.)
    bool available = false; // whether the vintage data was found
    std::string message;
};

/**
 * @brief Request to stop running synthetic market data feed(s).
 *
 * Stops the feed identified by source_name; if source_name is empty, stops
 * all running feeds.
 */
struct stop_market_feed_config_request {
    using response_type = struct stop_market_feed_config_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_feed_configs.stop";

    std::string source_name; // empty = stop all running feeds
};

struct stop_market_feed_config_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Request the set of currently running feed source_names.
 *
 * No parameters — returns whatever the service has in memory. The response
 * is a snapshot; callers should re-query after start/stop operations.
 */
struct list_market_feed_configs_request {
    using response_type = struct list_market_feed_configs_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.market_feed_configs.list";
};

struct list_market_feed_configs_response {
    bool success = false;
    std::vector<std::string> running_source_names; // source_names currently running
};

}

#endif
