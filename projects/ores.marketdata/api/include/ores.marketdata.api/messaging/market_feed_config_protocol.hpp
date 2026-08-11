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

#include <map>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief Per-kind outcome counts of one folder start/stop request, keyed
 * by the producer kind string (e.g. "fx_spot", "ir_curve") — see
 * ores.synthetic.api's feed_factory kind constants. The protocol names no
 * asset class: a new producer kind appears as a new map key.
 */
struct feed_kind_counts final {
    int started = 0;
    int already_running = 0;
    int skipped = 0;
};

/**
 * @brief Request to start every feed under a synthetic folder subtree.
 *
 * folder_id may name a Root, Collection, asset-class, or instrument-type
 * folder — the service resolves the whole subtree server-side (via
 * ores.synthetic.folder's hierarchy_fn) and starts every feed row whose
 * folder_id falls anywhere in it, of every asset class, dispatching through
 * the producer factory. One request expresses what used to require
 * client-side enumeration of every pair; works identically from Qt,
 * ores.shell, or a wt workflow step.
 */
struct start_feeds_under_folder_request {
    using response_type = struct start_feeds_under_folder_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_feed_configs.start_folder";

    std::string folder_id;
};

struct start_feeds_under_folder_response {
    bool success = false;
    std::string message;
    // Aggregates across every kind — the summary clients print. The
    // per-kind breakdown follows; both are always populated.
    int started = 0;
    int already_running = 0;
    // e.g. no children rows, or disabled
    int skipped = 0;
    std::map<std::string, feed_kind_counts> by_kind;
};

/**
 * @brief Request to stop every running feed under a synthetic folder subtree.
 * Same folder_id semantics as start_feeds_under_folder_request.
 */
struct stop_feeds_under_folder_request {
    using response_type = struct stop_feeds_under_folder_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_feed_configs.stop_folder";

    std::string folder_id;
};

struct stop_feeds_under_folder_response {
    bool success = false;
    std::string message;
    int stopped = 0;
    std::map<std::string, int> stopped_by_kind;
};

/**
 * @brief Vintage-availability status for one fx_spot_generation_config,
 * computed live (not stored) against market_observation.
 */
struct vintage_validity_entry {
    std::string fx_spot_generation_config_id;
    // False for price_source = "fixed" feeds -- the vintage check doesn't
    // apply to them, so `valid` is meaningless and should not be shown.
    bool applicable = false;
    bool valid = false;
};

/**
 * @brief Request the vintage-availability status of every feed visible to
 * the caller, computed at read time (no persisted/cached column) so it is
 * always accurate -- including immediately after a data import that added
 * no new feed rows, only new market_observation ones.
 */
struct get_vintage_validity_request {
    using response_type = struct get_vintage_validity_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_feed_configs.vintage_validity";
};

struct get_vintage_validity_response {
    bool success = false;
    std::string message;
    std::vector<vintage_validity_entry> entries;
};

}

#endif
