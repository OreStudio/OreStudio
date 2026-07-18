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
#ifndef ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_FEED_CONFIG_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_FEED_CONFIG_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::synthetic::messaging {

/**
 * @brief Request to start an ir_curve_generation_config's feed on demand.
 *
 * Mirrors ores.marketdata's start_market_feed_config_request for FX, but keyed by the config's
 * own id rather than a caller-supplied ore_key/source_name -- an IR curve config already carries
 * its own stable currency/index identity, so there is nothing else for the caller to name. The
 * handler resolves the config + its Curve Template entries + the refdata catalog server-side and
 * builds the feed the same way auto-start does (see make_ir_curve_feed()), so a config published
 * to any tenant (not just system) can be started from the UI without restarting the service.
 */
struct start_ir_curve_feed_request {
    using response_type = struct start_ir_curve_feed_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve_feed_configs.start";

    std::string config_id;
};

struct start_ir_curve_feed_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Request to stop running IR curve feed(s).
 *
 * Stops the feed identified by config_id (resolved server-side to its source_name, the same way
 * start does -- the client never needs to know the "ir_curve.<ccy>.<idx>" naming convention). If
 * both config_id and source_name are empty, stops all running IR curve feeds.
 */
struct stop_ir_curve_feed_request {
    using response_type = struct stop_ir_curve_feed_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve_feed_configs.stop";

    std::string config_id;   // preferred: resolved server-side to source_name
    std::string source_name; // used only if config_id is empty; empty too = stop all
};

struct stop_ir_curve_feed_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Request the set of currently running IR curve feed source_names.
 */
struct list_ir_curve_feed_configs_request {
    using response_type = struct list_ir_curve_feed_configs_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve_feed_configs.list";
};

struct list_ir_curve_feed_configs_response {
    bool success = false;
    std::vector<std::string> running_source_names;
};

}

#endif
