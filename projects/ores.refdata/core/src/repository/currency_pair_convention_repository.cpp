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
#include "ores.refdata.core/repository/currency_pair_convention_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/currency_pair_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/currency_pair_convention_entity.hpp"
#include "ores.refdata.core/repository/currency_pair_convention_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string currency_pair_convention_repository::sql() {
    return generate_create_table_sql<currency_pair_convention_entity>(lg());
}

void currency_pair_convention_repository::write(context ctx,
                                                const domain::currency_pair_convention& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency pair convention: " << v.pair_code;
    execute_write_query(ctx,
                        currency_pair_convention_mapper::map(v),
                        lg(),
                        "Writing currency pair convention to database.");
}

void currency_pair_convention_repository::write(
    context ctx, const std::vector<domain::currency_pair_convention>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing currency pair conventions. Count: " << v.size();
    execute_write_query(ctx,
                        currency_pair_convention_mapper::map(v),
                        lg(),
                        "Writing currency pair conventions to database.");
}

std::vector<domain::currency_pair_convention>
currency_pair_convention_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_pair_convention_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("pair_code"_c);

    return execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
        ctx,
        query,
        [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
        lg(),
        "Reading latest currency pair conventions");
}

std::vector<domain::currency_pair_convention>
currency_pair_convention_repository::read_latest(context ctx, const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency pair convention. pair_code: "
                               << pair_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<currency_pair_convention_entity>> |
        where("tenant_id"_c == tid && "pair_code"_c == pair_code && "valid_to"_c == max.value());

    return execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
        ctx,
        query,
        [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
        lg(),
        "Reading latest currency pair convention by pair_code.");
}

std::vector<domain::currency_pair_convention>
currency_pair_convention_repository::read_all(context ctx, const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all currency pair convention versions. pair_code: "
                               << pair_code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_pair_convention_entity>> |
                       where("tenant_id"_c == tid && "pair_code"_c == pair_code) |
                       order_by("version"_c.desc());

    return execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
        ctx,
        query,
        [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
        lg(),
        "Reading all currency pair convention versions by pair_code.");
}


void currency_pair_convention_repository::remove(context ctx, const std::string& pair_code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing currency pair convention: " << pair_code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<currency_pair_convention_entity> |
        where("tenant_id"_c == tid && "pair_code"_c == pair_code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing currency pair convention from database.");
}

std::vector<domain::currency_pair_convention> currency_pair_convention_repository::read_latest(
    context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest currency pair conventions with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<currency_pair_convention_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("pair_code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<currency_pair_convention_entity, domain::currency_pair_convention>(
        ctx,
        query,
        [](const auto& entities) { return currency_pair_convention_mapper::map(entities); },
        lg(),
        "Reading latest currency pair conventions with pagination.");
}

std::uint32_t currency_pair_convention_repository::get_total_convention_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active currency pair convention count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<currency_pair_convention_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "valid_to"_c == max.value()) | sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active currency pair convention count: " << count;
    return count;
}

void currency_pair_convention_repository::remove(context ctx,
                                                 const std::vector<std::string>& pair_codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<currency_pair_convention_entity> |
        where("tenant_id"_c == tid && "pair_code"_c.in(pair_codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing currency pair conventions.");
}


}
