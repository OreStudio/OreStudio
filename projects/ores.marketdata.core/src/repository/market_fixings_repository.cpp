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
#include "ores.marketdata.core/repository/market_fixings_repository.hpp"

#include <format>
#include <boost/uuid/uuid_io.hpp>
#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.marketdata.api/domain/market_fixing_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_fixing_entity.hpp"
#include "ores.marketdata.core/repository/market_fixing_mapper.hpp"

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string market_fixings_repository::sql() {
    return generate_create_table_sql<market_fixing_entity>(lg());
}

void market_fixings_repository::write(
    context ctx, const domain::market_fixing& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market fixing for series: " << v.series_id;
    execute_write_query(ctx, market_fixing_mapper::map(v),
        lg(), "Writing market fixing to database.");
}

void market_fixings_repository::write(
    context ctx, const std::vector<domain::market_fixing>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market fixings. Count: " << v.size();
    execute_write_query(ctx, market_fixing_mapper::map(v),
        lg(), "Writing market fixings to database.");
}

std::vector<domain::market_fixing>
market_fixings_repository::read_latest(
    context ctx, const boost::uuids::uuid& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest fixings for series: " << series_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto query = sqlgen::read<std::vector<market_fixing_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == sid &&
              "valid_to"_c == max.value()) |
        order_by("fixing_date"_c);

    return execute_read_query<market_fixing_entity, domain::market_fixing>(
        ctx, query,
        [](const auto& entities) { return market_fixing_mapper::map(entities); },
        lg(), "Reading latest market fixings.");
}

std::vector<domain::market_fixing>
market_fixings_repository::read_latest(
    context ctx, const boost::uuids::uuid& series_id,
    const std::chrono::year_month_day& from_date,
    const std::chrono::year_month_day& to_date) {
    BOOST_LOG_SEV(lg(), debug) << "Reading fixings for series: " << series_id
        << " from: " << std::format("{:%Y-%m-%d}", from_date)
        << " to: " << std::format("{:%Y-%m-%d}", to_date);
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto from_str = std::format("{:%Y-%m-%d}", from_date);
    const auto to_str   = std::format("{:%Y-%m-%d}", to_date);
    const auto query = sqlgen::read<std::vector<market_fixing_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == sid &&
              "fixing_date"_c >= from_str && "fixing_date"_c <= to_str &&
              "valid_to"_c == max.value()) |
        order_by("fixing_date"_c);

    return execute_read_query<market_fixing_entity, domain::market_fixing>(
        ctx, query,
        [](const auto& entities) { return market_fixing_mapper::map(entities); },
        lg(), "Reading market fixings by date range.");
}

void market_fixings_repository::remove(
    context ctx, const boost::uuids::uuid& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing fixings for series: " << series_id;
    const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto sid = boost::uuids::to_string(series_id);
    const auto query = sqlgen::delete_from<market_fixing_entity> |
        where("tenant_id"_c == tid && "series_id"_c == sid &&
              "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(),
        "Removing market fixings from database.");
}

}
