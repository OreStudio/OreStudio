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
#include "ores.refdata.core/repository/rounding_type_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/rounding_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/rounding_type_entity.hpp"
#include "ores.refdata.core/repository/rounding_type_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string rounding_type_repository::sql() {
    return generate_create_table_sql<rounding_type_entity>(lg());
}

void rounding_type_repository::write(context ctx, const domain::rounding_type& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing rounding type: " << v.code;
    execute_write_query(
        ctx, rounding_type_mapper::map(v), lg(), "Writing rounding type to database.");
}

void rounding_type_repository::write(context ctx, const std::vector<domain::rounding_type>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing rounding types. Count: " << v.size();
    execute_write_query(
        ctx, rounding_type_mapper::map(v), lg(), "Writing rounding types to database.");
}

std::vector<domain::rounding_type> rounding_type_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<rounding_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<rounding_type_entity, domain::rounding_type>(
        ctx,
        query,
        [](const auto& entities) { return rounding_type_mapper::map(entities); },
        lg(),
        "Reading latest rounding types");
}

std::vector<domain::rounding_type> rounding_type_repository::read_latest(context ctx,
                                                                         const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest rounding type. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<rounding_type_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<rounding_type_entity, domain::rounding_type>(
        ctx,
        query,
        [](const auto& entities) { return rounding_type_mapper::map(entities); },
        lg(),
        "Reading latest rounding type by code.");
}

std::vector<domain::rounding_type> rounding_type_repository::read_all(context ctx,
                                                                      const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all rounding type versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<rounding_type_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<rounding_type_entity, domain::rounding_type>(
        ctx,
        query,
        [](const auto& entities) { return rounding_type_mapper::map(entities); },
        lg(),
        "Reading all rounding type versions by code.");
}

std::optional<domain::rounding_type> rounding_type_repository::read_at_version(
    context ctx, const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading rounding type at version. code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<rounding_type_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<rounding_type_entity, domain::rounding_type>(
        ctx,
        query,
        [](const auto& entities) { return rounding_type_mapper::map(entities); },
        lg(),
        "Reading rounding type at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void rounding_type_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing rounding type: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<rounding_type_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing rounding type from database.");
}

std::vector<domain::rounding_type>
rounding_type_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest rounding types with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<rounding_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<rounding_type_entity, domain::rounding_type>(
        ctx,
        query,
        [](const auto& entities) { return rounding_type_mapper::map(entities); },
        lg(),
        "Reading latest rounding types with pagination.");
}

std::uint32_t rounding_type_repository::get_total_type_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active rounding type count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<rounding_type_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active rounding type count: " << count;
    return count;
}

void rounding_type_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<rounding_type_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing rounding types.");
}


}
