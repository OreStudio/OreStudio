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
#include "ores.marketdata.core/repository/pg_array.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <map>
#include <set>
#include <stdexcept>
#include <utility>

namespace ores::marketdata::repository {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

namespace {

using datum_key = std::pair<std::string, std::string>;

}

std::vector<std::int16_t>
tick_coordinate_repository::resolve_all(context ctx, const std::vector<tick_datum>& wanted) {
    if (wanted.empty())
        return {};

    static const std::string read_sql = R"(
        SELECT series_id::text, oresmd_uri, coord::text
        FROM ores_marketdata_tick_coords_tbl
        WHERE series_id = ANY($1::uuid[])
    )";

    // The insert takes the next numbers of each series, one for each URI, and does nothing for
    // a datum or a number that is taken. The read after it finds what stands.
    static const std::string insert_sql = R"(
        WITH wanted AS (
            SELECT DISTINCT w.party_id, w.series_id, w.uri
            FROM unnest($2::uuid[], $3::uuid[], $4::text[]) AS w(party_id, series_id, uri)
        ),
        base AS (
            SELECT series_id, max(coord) AS top
            FROM ores_marketdata_tick_coords_tbl
            WHERE series_id = ANY($3::uuid[])
            GROUP BY series_id
        )
        INSERT INTO ores_marketdata_tick_coords_tbl
            (tenant_id, party_id, series_id, coord, oresmd_uri)
        SELECT $1::uuid, w.party_id, w.series_id,
               (coalesce(b.top, 0) + row_number() OVER (PARTITION BY w.series_id ORDER BY w.uri))
                   ::smallint,
               w.uri
        FROM wanted w
        LEFT JOIN base b ON b.series_id = w.series_id
        ON CONFLICT DO NOTHING
        RETURNING 1
    )";

    const auto tenant = ctx.tenant_id().to_string();
    std::set<std::string> series_set;
    for (const auto& d : wanted)
        series_set.insert(boost::uuids::to_string(d.series_id));
    const auto series_array = pg_array({series_set.begin(), series_set.end()});

    constexpr int attempts = 3;
    for (int attempt = 0; attempt < attempts; ++attempt) {
        std::map<datum_key, std::int16_t> known;
        for (const auto& row : execute_parameterized_multi_column_query(
                 ctx, read_sql, {series_array}, lg(), "reading tick coordinates")) {
            if (row.size() >= 3 && row[0] && row[1] && row[2])
                known[{*row[0], *row[1]}] = static_cast<std::int16_t>(std::stoi(*row[2]));
        }

        std::vector<std::string> parties;
        std::vector<std::string> series;
        std::vector<std::string> uris;
        std::vector<std::int16_t> result;
        result.reserve(wanted.size());
        bool complete = true;
        for (const auto& d : wanted) {
            const auto series_text = boost::uuids::to_string(d.series_id);
            const auto found = known.find({series_text, d.oresmd_uri});
            if (found != known.end()) {
                result.push_back(found->second);
                continue;
            }
            complete = false;
            parties.push_back(boost::uuids::to_string(d.party_id));
            series.push_back(series_text);
            uris.push_back(d.oresmd_uri);
        }
        if (complete)
            return result;

        execute_parameterized_multi_column_query(
            ctx,
            insert_sql,
            {tenant, pg_array(parties), pg_array(series), pg_array(uris)},
            lg(),
            "assigning tick coordinates");
    }
    throw std::runtime_error("The tick coordinates of " + std::to_string(wanted.size()) +
                             " datum(s) cannot be assigned");
}

std::int16_t tick_coordinate_repository::resolve(context ctx,
                                                 const boost::uuids::uuid& party_id,
                                                 const boost::uuids::uuid& series_id,
                                                 const std::string& oresmd_uri) {
    return resolve_all(std::move(ctx), {{party_id, series_id, oresmd_uri}}).front();
}

}
