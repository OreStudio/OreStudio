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
#include "ores.iam.core/repository/tenant_type_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/tenant_type_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/tenant_type_entity.hpp"
#include "ores.iam.core/repository/tenant_type_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenant_type_repository::sql() {
    return generate_create_table_sql<tenant_type_entity>(lg());
}

void tenant_type_repository::write(context ctx, const domain::tenant_type& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant type. " << "type: " << v.type;
    execute_write_query(ctx, tenant_type_mapper::map(v), lg(), "Writing tenant type to database.");
}

void tenant_type_repository::write(context ctx, const std::vector<domain::tenant_type>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant types. Count: " << v.size();
    execute_write_query(ctx, tenant_type_mapper::map(v), lg(), "Writing tenant types to database.");
}

std::vector<domain::tenant_type> tenant_type_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenant_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("type"_c);

    return execute_read_query<tenant_type_entity, domain::tenant_type>(
        ctx,
        query,
        [](const auto& entities) { return tenant_type_mapper::map(entities); },
        lg(),
        "Reading latest tenant types");
}

std::vector<domain::tenant_type> tenant_type_repository::read_latest(context ctx,
                                                                     const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant type. " << "type: " << type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<tenant_type_entity>> |
        where("tenant_id"_c == tid && "type"_c == type && "valid_to"_c == max.value());

    return execute_read_query<tenant_type_entity, domain::tenant_type>(
        ctx,
        query,
        [](const auto& entities) { return tenant_type_mapper::map(entities); },
        lg(),
        "Reading latest tenant type by type.");
}


std::vector<domain::tenant_type> tenant_type_repository::read_all(context ctx,
                                                                  const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenant type versions. " << "type: " << type;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenant_type_entity>> |
                       where("tenant_id"_c == tid && "type"_c == type) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<tenant_type_entity, domain::tenant_type>(
        ctx,
        query,
        [](const auto& entities) { return tenant_type_mapper::map(entities); },
        lg(),
        "Reading all tenant type versions by type.");
}

std::optional<domain::tenant_type> tenant_type_repository::read_at_version(context ctx,
                                                                           const std::string& type,
                                                                           std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading tenant type at version. " << "type: " << type
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenant_type_entity>> |
                       where("tenant_id"_c == tid && "type"_c == type && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<tenant_type_entity, domain::tenant_type>(
        ctx,
        query,
        [](const auto& entities) { return tenant_type_mapper::map(entities); },
        lg(),
        "Reading tenant type at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void tenant_type_repository::remove(context ctx, const std::string& type) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant type. " << "type: " << type;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenant_type_entity> |
        where("tenant_id"_c == tid && "type"_c == type && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing tenant type from database.");
}

std::vector<domain::tenant_type>
tenant_type_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant types with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenant_type_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("type"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<tenant_type_entity, domain::tenant_type>(
        ctx,
        query,
        [](const auto& entities) { return tenant_type_mapper::map(entities); },
        lg(),
        "Reading latest tenant types with pagination.");
}

std::uint32_t tenant_type_repository::get_total_type_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenant type count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<tenant_type_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenant type count: " << count;
    return count;
}

void tenant_type_repository::remove(context ctx, const std::vector<std::string>& types) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenant_type_entity> |
        where("tenant_id"_c == tid && "type"_c.in(types) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing tenant types.");
}


}
