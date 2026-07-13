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
#include "ores.refdata.core/repository/tenor_unit_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/tenor_unit_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/tenor_unit_entity.hpp"
#include "ores.refdata.core/repository/tenor_unit_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenor_unit_repository::sql() {
    return generate_create_table_sql<tenor_unit_entity>(lg());
}

void tenor_unit_repository::write(context ctx, const domain::tenor_unit& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor unit: " << v.code;
    execute_write_query(ctx, tenor_unit_mapper::map(v), lg(), "Writing tenor unit to database.");
}

void tenor_unit_repository::write(context ctx, const std::vector<domain::tenor_unit>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenor units. Count: " << v.size();
    execute_write_query(ctx, tenor_unit_mapper::map(v), lg(), "Writing tenor units to database.");
}

std::vector<domain::tenor_unit> tenor_unit_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_unit_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading latest tenor units");
}

std::vector<domain::tenor_unit> tenor_unit_repository::read_latest(context ctx,
                                                                   const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor unit. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<tenor_unit_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading latest tenor unit by code.");
}

std::vector<domain::tenor_unit> tenor_unit_repository::read_all(context ctx,
                                                                const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenor unit versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_unit_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc());

    return execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading all tenor unit versions by code.");
}

std::optional<domain::tenor_unit> tenor_unit_repository::read_at_version(context ctx,
                                                                         const std::string& code,
                                                                         std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading tenor unit at version. code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_unit_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading tenor unit at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void tenor_unit_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenor unit: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenor_unit_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing tenor unit from database.");
}

std::vector<domain::tenor_unit>
tenor_unit_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenor units with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenor_unit_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<tenor_unit_entity, domain::tenor_unit>(
        ctx,
        query,
        [](const auto& entities) { return tenor_unit_mapper::map(entities); },
        lg(),
        "Reading latest tenor units with pagination.");
}

std::uint32_t tenor_unit_repository::get_total_unit_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenor unit count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<tenor_unit_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenor unit count: " << count;
    return count;
}

void tenor_unit_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenor_unit_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing tenor units.");
}


}
