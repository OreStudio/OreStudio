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
#include "ores.trading/repository/trade_repository.hpp"

#include <sqlgen/postgres.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.trading/domain/trade_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading/repository/trade_entity.hpp"
#include "ores.trading/repository/trade_mapper.hpp"

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string trade_repository::sql() {
    return generate_create_table_sql<trade_entity>(lg());
}

void trade_repository::write(context ctx, const domain::trade& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trade: " << v.id;
    execute_write_query(ctx, trade_mapper::map(v),
        lg(), "Writing trade to database.");
}

void trade_repository::write(
    context ctx, const std::vector<domain::trade>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing trades. Count: " << v.size();
    execute_write_query(ctx, trade_mapper::map(v),
        lg(), "Writing trades to database.");
}

std::vector<domain::trade>
trade_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<trade_entity, domain::trade>(
        ctx, query,
        [](const auto& entities) { return trade_mapper::map(entities); },
        lg(), "Reading latest trades");
}

std::vector<domain::trade>
trade_repository::read_latest(context ctx, std::uint32_t offset,
    std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trades with offset: "
                               << offset << " and limit: " << limit;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        order_by("id"_c) |
        sqlgen::offset(offset) |
        sqlgen::limit(limit);

    return execute_read_query<trade_entity, domain::trade>(
        ctx, query,
        [](const auto& entities) { return trade_mapper::map(entities); },
        lg(), "Reading latest trades with pagination");
}

std::uint32_t trade_repository::count_latest(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Counting latest trades";
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();

    struct count_result {
        long long count;
    };

    const auto query = sqlgen::select_from<trade_entity>(
        sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active trade count: " << count;
    return count;
}

std::vector<domain::trade>
trade_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest trade. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<trade_entity, domain::trade>(
        ctx, query,
        [](const auto& entities) { return trade_mapper::map(entities); },
        lg(), "Reading latest trade by id.");
}

std::vector<domain::trade>
trade_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all trade versions. id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<trade_entity>> |
        where("tenant_id"_c == tid && "id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<trade_entity, domain::trade>(
        ctx, query,
        [](const auto& entities) { return trade_mapper::map(entities); },
        lg(), "Reading all trade versions by id.");
}

void trade_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing trade: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<trade_entity> |
        where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing trade from database.");
}

}
