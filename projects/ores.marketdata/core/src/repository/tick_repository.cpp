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
#include "ores.marketdata.core/repository/tick_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.marketdata.core/repository/pg_array.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <format>
#include <stdexcept>
#include <string_view>

namespace ores::marketdata::repository {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

std::vector<tick> tick_repository::read_range(context ctx,
                                              const boost::uuids::uuid& series_id,
                                              std::chrono::system_clock::time_point from,
                                              std::chrono::system_clock::time_point to,
                                              const std::vector<std::string>& coordinates,
                                              std::size_t limit) {
    // The page is cut before the join, and by time alone. The time order lets the planner walk
    // the index on the series and the time and stop after the limit, where an order that names
    // the coordinate too makes it read and sort the whole span. A span over the ceiling is
    // refused, so the cut matters only when the reply is refused. The outer query puts the page
    // in the order by time and then by coordinate. The dictionary has the tenant policy only, so
    // its join states the party of the tick. The time comes back as whole microseconds since the
    // epoch, which parse back without a rounding step, and the value as the shortest text that
    // reads back as the same double.
    const auto statement = [](std::string_view coordinate_filter, int limit_parameter) {
        return std::format(R"(
            WITH page AS (
                SELECT t.ts, t.coord, t.party_id, t.value
                FROM ores_marketdata_ticks_tbl t
                WHERE t.series_id = $1::uuid
                  AND t.ts >= $2::timestamptz AND t.ts < $3::timestamptz{}
                ORDER BY t.ts
                LIMIT ${}::int
            )
            SELECT (extract(epoch FROM p.ts) * 1000000)::bigint::text, c.oresmd_uri, p.value::text
            FROM page p
            JOIN ores_marketdata_tick_coords_tbl c
              ON c.series_id = $1::uuid AND c.coord = p.coord AND c.party_id = p.party_id
            ORDER BY p.ts, p.coord
        )",
                           coordinate_filter,
                           limit_parameter);
    };

    const auto series = boost::uuids::to_string(series_id);
    const auto from_text = ores::platform::time::datetime::to_iso8601_utc(from);
    const auto to_text = ores::platform::time::datetime::to_iso8601_utc(to);
    const auto limit_text = std::to_string(limit);

    // A coordinate filter names URIs, and the ticks store numbers, so the numbers come first. A
    // filter that names no datum of the series reads nothing.
    std::vector<std::string> numbers;
    if (!coordinates.empty()) {
        for (const auto& row : execute_parameterized_multi_column_query(
                 ctx,
                 "SELECT coord::text FROM ores_marketdata_tick_coords_tbl "
                 "WHERE series_id = $1::uuid AND oresmd_uri = ANY($2::text[])",
                 {series, pg_array(coordinates)},
                 lg(),
                 "reading the numbers of the coordinates of a series")) {
            if (!row.empty() && row[0])
                numbers.push_back(*row[0]);
        }
        if (numbers.empty())
            return {};
    }

    const auto rows =
        numbers.empty() ?
            execute_parameterized_multi_column_query(ctx,
                                                     statement("", 4),
                                                     {series, from_text, to_text, limit_text},
                                                     lg(),
                                                     "reading the ticks of a series") :
            execute_parameterized_multi_column_query(
                ctx,
                statement(" AND t.coord = ANY($4::smallint[])", 5),
                {series, from_text, to_text, pg_array(numbers), limit_text},
                lg(),
                "reading the ticks of a series by coordinate");

    std::vector<tick> ticks;
    ticks.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 3 || !row[0] || !row[1] || !row[2])
            throw std::runtime_error("A tick row of a series is short");
        ticks.push_back(
            {std::chrono::system_clock::time_point(std::chrono::microseconds(std::stoll(*row[0]))),
             *row[1],
             std::stod(*row[2])});
    }
    return ticks;
}

}
