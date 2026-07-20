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
#ifndef ORES_MARKETDATA_API_MESSAGING_CURVE_SNAPSHOT_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_CURVE_SNAPSHOT_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_observation.hpp"
#include <cstdint>
#include <string>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief Requests the latest-as-of snapshot of a curve/grid series: one observation per
 * point_id, reconstructed from independently-ticking market_observation rows.
 *
 * series_type/metric/qualifier identify the series the same way market_series is looked up
 * elsewhere (e.g. curve_feed_ingest_loop) -- the caller does not need to know the internal
 * series_id. Always "latest" (now) for the as-of time; no as-of-in-the-past parameter yet.
 */
struct get_curve_snapshot_request {
    using response_type = struct get_curve_snapshot_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve-snapshot.get";
    std::string series_type;
    std::string metric;
    std::string qualifier;
};

struct get_curve_snapshot_response {
    std::vector<ores::marketdata::domain::market_observation> observations;
    bool success = false;
    std::string message;
};

/**
 * @brief Requests a curve-evolution view: bucket_count as-of snapshots, one every bucket_seconds,
 * ending now.
 */
struct get_curve_snapshot_buckets_request {
    using response_type = struct get_curve_snapshot_buckets_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve-snapshot.buckets";
    std::string series_type;
    std::string metric;
    std::string qualifier;
    std::int64_t bucket_seconds = 1800;
    std::uint32_t bucket_count = 5;
};

struct get_curve_snapshot_buckets_response {
    // Oldest to newest, one entry per bucket; a bucket with no observations at/before its
    // boundary is an empty vector.
    std::vector<std::vector<ores::marketdata::domain::market_observation>> buckets;
    bool success = false;
    std::string message;
};

}

#endif
