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
#include "ores.marketdata.core/repository/market_observations_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.marketdata.api/domain/market_observation_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_observation_entity.hpp"
#include "ores.marketdata.core/repository/market_observation_mapper.hpp"
#include "ores.platform/time/datetime.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string market_observations_repository::sql() {
    return generate_create_table_sql<market_observation_entity>(lg());
}

void market_observations_repository::write(context ctx, const domain::market_observation& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market observation for series: " << v.series_id;
    execute_write_query(
        ctx, market_observation_mapper::map(v), lg(), "Writing market observation to database.");
}

void market_observations_repository::write(context ctx,
                                           const std::vector<domain::market_observation>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market observations. Count: " << v.size();
    execute_write_query(
        ctx, market_observation_mapper::map(v), lg(), "Writing market observations to database.");
}

std::vector<domain::market_observation>
market_observations_repository::read_latest(context ctx, const boost::uuids::uuid& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest observations for series: " << series_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto query =
        sqlgen::read<std::vector<market_observation_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == sid && "valid_to"_c == max.value()) |
        order_by("observation_datetime"_c);

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations.");
}

std::vector<domain::market_observation> market_observations_repository::read_latest(
    context ctx,
    const boost::uuids::uuid& series_id,
    const std::chrono::system_clock::time_point& from_datetime,
    const std::chrono::system_clock::time_point& to_datetime) {
    using ores::platform::time::datetime;
    BOOST_LOG_SEV(lg(), debug) << "Reading observations for series: " << series_id
                               << " from: " << datetime::to_iso8601_utc(from_datetime)
                               << " to: " << datetime::to_iso8601_utc(to_datetime);
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto from_str = datetime::to_iso8601_utc(from_datetime);
    const auto to_str = datetime::to_iso8601_utc(to_datetime);
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "series_id"_c == sid &&
                             "observation_datetime"_c >= from_str &&
                             "observation_datetime"_c <= to_str && "valid_to"_c == max.value()) |
                       order_by("observation_datetime"_c);

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading market observations by date range.");
}

std::vector<domain::market_observation> market_observations_repository::read_as_of(
    context ctx,
    const boost::uuids::uuid& series_id,
    const std::chrono::system_clock::time_point& as_of_datetime) {
    using ores::platform::time::datetime;
    BOOST_LOG_SEV(lg(), debug) << "Reading as-of snapshot for series: " << series_id
                               << " as-of: " << datetime::to_iso8601_utc(as_of_datetime);
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto as_of_str = datetime::to_iso8601_utc(as_of_datetime);

    // DISTINCT ON (point_id) returns exactly one row per point -- the latest at or before
    // as_of_datetime -- reconstructing a curve/grid snapshot from independently-ticking rows.
    // Correct whether every point shares one observation_datetime (today's synchronous
    // publish) or points have staggered timestamps (the general case).
    static const std::string sql = R"(
        SELECT DISTINCT ON (point_id)
            id, tenant_id, party_id, series_id, observation_datetime, point_id, value,
            source, valid_from, valid_to
        FROM ores_marketdata_market_observations_tbl
        WHERE tenant_id = $1 AND series_id = $2 AND observation_datetime <= $3
            AND valid_to = $4
        ORDER BY point_id, observation_datetime DESC
    )";

    const auto rows = execute_parameterized_multi_column_query(
        ctx, sql, {tid, sid, as_of_str, MAX_TIMESTAMP}, lg(), "reading as-of curve snapshot");

    std::vector<domain::market_observation> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 10)
            continue;

        market_observation_entity e;
        e.id = row[0].value_or("");
        e.tenant_id = row[1].value_or("");
        e.party_id = row[2].value_or("");
        e.series_id = row[3].value_or("");
        e.observation_datetime = row[4].value_or("");
        e.point_id = row[5].value_or("");
        e.value = row[6].value_or("");
        e.source = row[7];
        e.valid_from = row[8].value_or("");
        e.valid_to = row[9].value_or("");
        result.push_back(market_observation_mapper::map(e));
    }
    return result;
}

std::vector<std::vector<domain::market_observation>>
market_observations_repository::read_as_of_buckets(
    context ctx,
    const boost::uuids::uuid& series_id,
    const std::vector<std::chrono::system_clock::time_point>& bucket_boundaries) {
    BOOST_LOG_SEV(lg(), debug) << "Reading " << bucket_boundaries.size()
                               << " as-of bucket snapshots for series: " << series_id;
    std::vector<std::vector<domain::market_observation>> result;
    result.reserve(bucket_boundaries.size());
    for (const auto& boundary : bucket_boundaries)
        result.push_back(read_as_of(ctx, series_id, boundary));
    return result;
}

void market_observations_repository::remove(context ctx, const boost::uuids::uuid& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing observations for series: " << series_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto query =
        sqlgen::delete_from<market_observation_entity> |
        where("tenant_id"_c == tid && "series_id"_c == sid && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing market observations from database.");
}

}
