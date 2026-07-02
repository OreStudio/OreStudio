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
#include "ores.marketdata.core/repository/market_fixing_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.marketdata.api/domain/market_fixing_json_io.hpp" // IWYU pragma: keep.
#include "ores.marketdata.core/repository/market_fixing_entity.hpp"
#include "ores.marketdata.core/repository/market_fixing_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::marketdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string market_fixing_repository::sql() {
    return generate_create_table_sql<market_fixing_entity>(lg());
}

void market_fixing_repository::write(context ctx, const domain::market_fixing& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market fixing: " << v.id;
    execute_write_query(
        ctx, market_fixing_mapper::map(v), lg(), "Writing market fixing to database.");
}

void market_fixing_repository::write(context ctx, const std::vector<domain::market_fixing>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing market fixings. Count: " << v.size();
    execute_write_query(
        ctx, market_fixing_mapper::map(v), lg(), "Writing market fixings to database.");
}

std::vector<domain::market_fixing> market_fixing_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_fixing_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<market_fixing_entity, domain::market_fixing>(
        ctx,
        query,
        [](const auto& entities) { return market_fixing_mapper::map(entities); },
        lg(),
        "Reading latest market fixings");
}

std::vector<domain::market_fixing> market_fixing_repository::read_latest(context ctx,
                                                                         const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market fixing. id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_fixing_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<market_fixing_entity, domain::market_fixing>(
        ctx,
        query,
        [](const auto& entities) { return market_fixing_mapper::map(entities); },
        lg(),
        "Reading latest market fixing by id.");
}

std::vector<domain::market_fixing> market_fixing_repository::read_all(context ctx,
                                                                      const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all market fixing versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_fixing_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<market_fixing_entity, domain::market_fixing>(
        ctx,
        query,
        [](const auto& entities) { return market_fixing_mapper::map(entities); },
        lg(),
        "Reading all market fixing versions by id.");
}

void market_fixing_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing market fixing: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_fixing_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing market fixing from database.");
}

std::vector<domain::market_fixing>
market_fixing_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest market fixings with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<market_fixing_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<market_fixing_entity, domain::market_fixing>(
        ctx,
        query,
        [](const auto& entities) { return market_fixing_mapper::map(entities); },
        lg(),
        "Reading latest market fixings with pagination.");
}

std::uint32_t market_fixing_repository::get_total_market_fixing_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active market fixing count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<market_fixing_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active market fixing count: " << count;
    return count;
}

void market_fixing_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<market_fixing_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing market fixings.");
}


}
