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
#include "ores.marketdata.core/repository/feed_binding_activity_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::marketdata::repository {

using namespace ores::logging;
using ores::database::repository::execute_parameterized_multi_column_query;

void feed_binding_activity_repository::record(context ctx,
                                              const boost::uuids::uuid& party_id,
                                              const std::string& source_name,
                                              const boost::uuids::uuid& series_id,
                                              std::chrono::system_clock::time_point last_tick_at) {
    // RETURNING makes the statement a query, which the helper requires. A row that already
    // holds a newer tick returns nothing.
    static const std::string sql = R"(
        INSERT INTO ores_marketdata_feed_binding_activity_tbl AS a
            (tenant_id, party_id, source_name, series_id, last_tick_at)
        VALUES ($1::uuid, $2::uuid, $3, $4::uuid, $5::timestamptz)
        ON CONFLICT (tenant_id, party_id, source_name) DO UPDATE
            SET series_id = EXCLUDED.series_id, last_tick_at = EXCLUDED.last_tick_at
            WHERE a.last_tick_at < EXCLUDED.last_tick_at
        RETURNING 1
    )";

    execute_parameterized_multi_column_query(
        ctx,
        sql,
        {ctx.tenant_id().to_string(),
         boost::uuids::to_string(party_id),
         source_name,
         boost::uuids::to_string(series_id),
         ores::platform::time::datetime::to_iso8601_utc(last_tick_at)},
        lg(),
        "recording the activity of a feed binding");
}

std::optional<feed_binding_activity_record> feed_binding_activity_repository::read(
    context ctx, const boost::uuids::uuid& party_id, const std::string& source_name) {
    static const std::string sql = R"(
        SELECT series_id::text, last_tick_at
        FROM ores_marketdata_feed_binding_activity_tbl
        WHERE tenant_id = $1::uuid AND party_id = $2::uuid AND source_name = $3
    )";

    const auto rows = execute_parameterized_multi_column_query(
        ctx,
        sql,
        {ctx.tenant_id().to_string(), boost::uuids::to_string(party_id), source_name},
        lg(),
        "reading the activity of a feed binding");
    if (rows.empty() || rows.front().size() < 2 || !rows.front()[0] || !rows.front()[1])
        return std::nullopt;
    return feed_binding_activity_record{
        boost::uuids::string_generator{}(*rows.front()[0]),
        ores::database::repository::timestamp_to_timepoint(std::string_view{*rows.front()[1]})};
}

}
