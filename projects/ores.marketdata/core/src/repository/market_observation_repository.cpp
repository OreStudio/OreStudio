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
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.marketdata.api/domain/market_observation_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_observation_entity.hpp"
#include "ores.marketdata.core/repository/market_observation_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string market_observation_repository::sql() {
    return generate_create_table_sql<market_observation_entity>(lg());
}

void market_observation_repository::write(context ctx, const domain::market_observation& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market observation: " << v.id;
    execute_write_query(
        ctx, market_observation_mapper::map(v), lg(), "Writing market observation to database.");
}

void market_observation_repository::write(context ctx,
                                          const std::vector<domain::market_observation>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market observations. Count: " << v.size();
    execute_write_query(
        ctx, market_observation_mapper::map(v), lg(), "Writing market observations to database.");
}

std::vector<domain::market_observation> market_observation_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations");
}

std::vector<domain::market_observation>
market_observation_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market observation. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observation by id.");
}

std::vector<domain::market_observation>
market_observation_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all market observation versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading all market observation versions by id.");
}


std::vector<domain::market_observation> market_observation_repository::read_latest_by_series_id(
    context ctx, const std::string& series_id, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market observations. series_id: " << series_id
                               << " offset: " << offset << " limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<market_observation_entity>> |
        where("tenant_id"_c == tid && "series_id"_c == series_id && "valid_to"_c == max.value()) |
        order_by("observation_datetime"_c.desc()) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations by series_id.");
}

std::uint32_t market_observation_repository::get_total_market_observation_count_by_series_id(
    context ctx, const std::string& series_id) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active market observations count. series_id: "
                               << series_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<market_observation_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "series_id"_c == series_id && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active market observations count by series_id: " << count;
    return count;
}

void market_observation_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing market observation: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_observation_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing market observation from database.");
}

std::vector<domain::market_observation>
market_observation_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market observations with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_observation_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<market_observation_entity, domain::market_observation>(
        ctx,
        query,
        [](const auto& entities) { return market_observation_mapper::map(entities); },
        lg(),
        "Reading latest market observations with pagination.");
}

std::uint32_t market_observation_repository::get_total_market_observation_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active market observation count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<market_observation_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active market observation count: " << count;
    return count;
}

void market_observation_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_observation_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing market observations.");
}


}
