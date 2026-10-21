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
#include "ores.marketdata.core/repository/tick_coordinate_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <stdexcept>

namespace ores::marketdata::repository {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

std::int16_t tick_coordinate_repository::resolve(context ctx,
                                                 const boost::uuids::uuid& party_id,
                                                 const boost::uuids::uuid& series_id,
                                                 const std::string& oresmd_uri) {
    // The insert takes the next number of the series and does nothing when the datum or the
    // number is taken. The second select then reads the number that stands.
    static const std::string sql = R"(
        WITH inserted AS (
            INSERT INTO ores_marketdata_tick_coords_tbl
                (tenant_id, party_id, series_id, coord, oresmd_uri)
            SELECT $1::uuid, $2::uuid, $3::uuid, coalesce(max(coord), 0) + 1, $4
            FROM ores_marketdata_tick_coords_tbl
            WHERE series_id = $3::uuid
            ON CONFLICT DO NOTHING
            RETURNING coord
        )
        SELECT coord::text FROM inserted
        UNION ALL
        SELECT coord::text FROM ores_marketdata_tick_coords_tbl
        WHERE series_id = $3::uuid AND oresmd_uri = $4
        LIMIT 1
    )";

    const auto tenant = ctx.tenant_id().to_string();
    const auto party = boost::uuids::to_string(party_id);
    const auto series = boost::uuids::to_string(series_id);

    constexpr int attempts = 3;
    for (int attempt = 0; attempt < attempts; ++attempt) {
        const auto rows = execute_parameterized_multi_column_query(
            ctx, sql, {tenant, party, series, oresmd_uri}, lg(), "resolving a tick coordinate");
        if (!rows.empty() && !rows.front().empty() && rows.front().front())
            return static_cast<std::int16_t>(std::stoi(*rows.front().front()));
    }
    throw std::runtime_error("The tick coordinate of '" + oresmd_uri + "' in series " + series +
                             " cannot be assigned");
}

}
