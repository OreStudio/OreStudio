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
#include "ores.marketdata.core/service/series_snapshot_reader.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.core/datum/oresmd_uri_codec.hpp"
#include "ores.marketdata.core/repository/curve_snapshot_staleness.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.marketdata.core/service/series_shape.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

namespace ores::marketdata::service {

using namespace ores::logging;

namespace {

inline std::string_view logger_name = "ores.marketdata.core.series_snapshot_reader";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/// A refusal the caller reads, rather than an outcome it must infer.
messaging::get_curve_snapshot_response refuse(std::string why) {
    messaging::get_curve_snapshot_response response;
    response.success = false;
    response.message = std::move(why);
    return response;
}

}

messaging::get_curve_snapshot_response
series_snapshot_reader::read(ores::database::context ctx,
                             const messaging::get_curve_snapshot_request& request,
                             std::chrono::system_clock::time_point as_of) {
    using ores::marketdata::datum::name_of;

    // Set before the series is resolved, so an empty snapshot still carries the
    // instant it is relative to rather than leaving it at the epoch.
    messaging::get_curve_snapshot_response response;
    response.as_of = as_of;

    repository::market_series_identity_reader resolver;
    const auto series = resolver.read(ctx, request.identity);
    if (series.empty()) {
        // No series carries the identity, which is not an error: a feed may not
        // have published yet, and the answer is an empty snapshot.
        response.success = true;
        return response;
    }
    if (series.size() > 1)
        return refuse("the identity names " + std::to_string(series.size()) +
                      " series; state the party so that it names one");

    const auto series_id = boost::uuids::to_string(series.front().id);
    const auto declared = read_series_shape(ctx, series_id);
    if (declared.axes.empty())
        return refuse("the object declares no axis, so it is not a composite object");

    const series_grid grid(declared);
    response.coordinate_fields.reserve(declared.axes.size());
    for (const auto axis : declared.axes)
        response.coordinate_fields.emplace_back(name_of(axis));
    response.nodes.reserve(grid.size());
    for (std::size_t n = 0; n < grid.size(); ++n)
        response.nodes.push_back({grid.coordinates(n), {}});
    response.values.assign(grid.size(), std::string());
    response.recorded_at.assign(grid.size(), std::chrono::system_clock::time_point{});

    const auto records = repository::market_observation_repository{}.read_as_of_records(
        ctx, series.front().id, as_of);
    for (const auto& record : records) {
        const auto parsed = datum::oresmd_uri_codec::read(record.observation.oresmd_uri);
        if (!parsed)
            throw std::runtime_error("a stored oresmd URI does not read: " +
                                     record.observation.oresmd_uri);
        const auto node = grid.node_of(*parsed);
        if (!node)
            continue;
        response.values[*node] = record.observation.value;
        response.recorded_at[*node] = record.recorded_at;
    }

    const auto summary = repository::summarise_staleness(records, as_of);
    response.oldest_age_seconds = summary.oldest_age_seconds;
    response.spread_seconds = summary.spread_seconds;
    response.warning = summary.warning;

    BOOST_LOG_SEV(lg(), debug) << "Read the snapshot of series " << series_id << ": "
                               << records.size() << " points over " << grid.size() << " nodes";
    response.success = true;
    return response;
}

}
