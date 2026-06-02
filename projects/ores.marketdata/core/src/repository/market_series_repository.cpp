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
#include "ores.marketdata.core/repository/market_series_repository.hpp"

#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.marketdata.api/domain/market_series_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_series_entity.hpp"
#include "ores.marketdata.core/repository/market_series_mapper.hpp"

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string market_series_repository::sql() {
    return generate_create_table_sql<market_series_entity>(lg());
}

void market_series_repository::write(context ctx, const domain::market_series& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market series: " << v.series_type
        << "/" << v.metric << "/" << v.qualifier;
    execute_write_query(ctx, market_series_mapper::map(v),
        lg(), "Writing market series to database.");
}

void market_series_repository::write(
    context ctx, const std::vector<domain::market_series>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market series. Count: " << v.size();
    execute_write_query(ctx, market_series_mapper::map(v),
        lg(), "Writing market series to database.");
}

std::vector<domain::market_series>
market_series_repository::read_latest(context ctx) {
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("series_type"_c, "metric"_c, "qualifier"_c);

    return execute_read_query<market_series_entity, domain::market_series>(
        ctx, query,
        [](const auto& entities) { return market_series_mapper::map(entities); },
        lg(), "Reading latest market series.");
}

std::vector<domain::market_series>
market_series_repository::read_latest(context ctx, const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market series by id: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<market_series_entity>> |
        where("tenant_id"_c == tid && "id"_c == id_str && "valid_to"_c == max.value());

    return execute_read_query<market_series_entity, domain::market_series>(
        ctx, query,
        [](const auto& entities) { return market_series_mapper::map(entities); },
        lg(), "Reading latest market series by id.");
}

std::vector<domain::market_series>
market_series_repository::read_latest_by_type(context ctx,
    const std::string& series_type,
    const std::string& metric,
    const std::string& qualifier) {
    BOOST_LOG_SEV(lg(), debug) << "Reading market series by type: "
        << series_type << "/" << metric << "/" << qualifier;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_series_entity>> |
        where("tenant_id"_c == tid &&
              "series_type"_c == series_type &&
              "metric"_c == metric &&
              "qualifier"_c == qualifier &&
              "valid_to"_c == max.value());

    return execute_read_query<market_series_entity, domain::market_series>(
        ctx, query,
        [](const auto& entities) { return market_series_mapper::map(entities); },
        lg(), "Reading market series by type.");
}

std::vector<domain::market_series>
market_series_repository::read_all(context ctx, const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all versions of market series: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::read<std::vector<market_series_entity>> |
        where("tenant_id"_c == tid && "id"_c == id_str) |
        order_by("version"_c.desc());

    return execute_read_query<market_series_entity, domain::market_series>(
        ctx, query,
        [](const auto& entities) { return market_series_mapper::map(entities); },
        lg(), "Reading all versions of market series.");
}

void market_series_repository::remove(context ctx, const boost::uuids::uuid& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing market series: " << id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto id_str = boost::uuids::to_string(id);
    const auto query = sqlgen::delete_from<market_series_entity> |
        where("tenant_id"_c == tid && "id"_c == id_str && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing market series from database.");
}

}
