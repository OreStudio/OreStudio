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
#include <stdexcept>

namespace ores::marketdata::repository {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

std::vector<tick> tick_repository::read_range(context ctx,
                                              const boost::uuids::uuid& series_id,
                                              std::chrono::system_clock::time_point from,
                                              std::chrono::system_clock::time_point to,
                                              const std::vector<std::string>& coordinates,
                                              std::size_t limit) {
    // The dictionary has the tenant policy only, so the join states the party of the tick.
    // The time comes back as whole microseconds since the epoch, which parse back without a
    // rounding step, and the value as the shortest text that reads back as the same double.
    static const std::string sql = R"(
        SELECT (extract(epoch FROM t.ts) * 1000000)::bigint::text, c.oresmd_uri, t.value::text
        FROM ores_marketdata_ticks_tbl t
        JOIN ores_marketdata_tick_coords_tbl c
          ON c.series_id = t.series_id AND c.coord = t.coord AND c.party_id = t.party_id
        WHERE t.series_id = $1::uuid
          AND t.ts >= $2::timestamptz AND t.ts < $3::timestamptz
          AND (cardinality($4::text[]) = 0 OR c.oresmd_uri = ANY($4::text[]))
        ORDER BY t.ts, t.coord
        LIMIT $5::int
    )";

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {boost::uuids::to_string(series_id),
         ores::platform::time::datetime::to_iso8601_utc(from),
         ores::platform::time::datetime::to_iso8601_utc(to),
         pg_array(coordinates),
         std::to_string(limit)},
        lg(),
        "reading the ticks of a series");

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
