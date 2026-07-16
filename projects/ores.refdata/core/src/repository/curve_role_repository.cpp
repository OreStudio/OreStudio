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
#include "ores.refdata.core/repository/curve_role_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.refdata.api/domain/curve_role_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/curve_role_entity.hpp"
#include "ores.refdata.core/repository/curve_role_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::refdata::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string curve_role_repository::sql() {
    return generate_create_table_sql<curve_role_entity>(lg());
}

void curve_role_repository::write(context ctx, const domain::curve_role& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing curve role: " << v.code;
    execute_write_query(ctx, curve_role_mapper::map(v), lg(), "Writing curve role to database.");
}

void curve_role_repository::write(context ctx, const std::vector<domain::curve_role>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing curve roles. Count: " << v.size();
    execute_write_query(ctx, curve_role_mapper::map(v), lg(), "Writing curve roles to database.");
}

std::vector<domain::curve_role> curve_role_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_role_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<curve_role_entity, domain::curve_role>(
        ctx,
        query,
        [](const auto& entities) { return curve_role_mapper::map(entities); },
        lg(),
        "Reading latest curve roles");
}

std::vector<domain::curve_role> curve_role_repository::read_latest(context ctx,
                                                                   const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest curve role. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<curve_role_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<curve_role_entity, domain::curve_role>(
        ctx,
        query,
        [](const auto& entities) { return curve_role_mapper::map(entities); },
        lg(),
        "Reading latest curve role by code.");
}

std::vector<domain::curve_role> curve_role_repository::read_all(context ctx,
                                                                const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all curve role versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_role_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<curve_role_entity, domain::curve_role>(
        ctx,
        query,
        [](const auto& entities) { return curve_role_mapper::map(entities); },
        lg(),
        "Reading all curve role versions by code.");
}

std::optional<domain::curve_role> curve_role_repository::read_at_version(context ctx,
                                                                         const std::string& code,
                                                                         std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading curve role at version. code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_role_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<curve_role_entity, domain::curve_role>(
        ctx,
        query,
        [](const auto& entities) { return curve_role_mapper::map(entities); },
        lg(),
        "Reading curve role at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void curve_role_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing curve role: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<curve_role_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing curve role from database.");
}

std::vector<domain::curve_role>
curve_role_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest curve roles with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<curve_role_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<curve_role_entity, domain::curve_role>(
        ctx,
        query,
        [](const auto& entities) { return curve_role_mapper::map(entities); },
        lg(),
        "Reading latest curve roles with pagination.");
}

std::uint32_t curve_role_repository::get_total_role_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active curve role count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<curve_role_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active curve role count: " << count;
    return count;
}

void curve_role_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<curve_role_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing curve roles.");
}


}
