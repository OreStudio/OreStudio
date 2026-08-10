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
#include "ores.iam.core/repository/tenant_status_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/tenant_status_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/tenant_status_entity.hpp"
#include "ores.iam.core/repository/tenant_status_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string tenant_status_repository::sql() {
    return generate_create_table_sql<tenant_status_entity>(lg());
}

void tenant_status_repository::write(context ctx, const domain::tenant_status& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant status. " << "status: " << v.status;
    execute_write_query(
        ctx, tenant_status_mapper::map(v), lg(), "Writing tenant status to database.");
}

void tenant_status_repository::write(context ctx, const std::vector<domain::tenant_status>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing tenant statuses. Count: " << v.size();
    execute_write_query(
        ctx, tenant_status_mapper::map(v), lg(), "Writing tenant statuses to database.");
}

std::vector<domain::tenant_status> tenant_status_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenant_status_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("status"_c);

    return execute_read_query<tenant_status_entity, domain::tenant_status>(
        ctx,
        query,
        [](const auto& entities) { return tenant_status_mapper::map(entities); },
        lg(),
        "Reading latest tenant statuses");
}

std::vector<domain::tenant_status>
tenant_status_repository::read_latest(context ctx, const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant status. " << "status: " << status;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<tenant_status_entity>> |
        where("tenant_id"_c == tid && "status"_c == status && "valid_to"_c == max.value());

    return execute_read_query<tenant_status_entity, domain::tenant_status>(
        ctx,
        query,
        [](const auto& entities) { return tenant_status_mapper::map(entities); },
        lg(),
        "Reading latest tenant status by status.");
}


std::vector<domain::tenant_status> tenant_status_repository::read_all(context ctx,
                                                                      const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all tenant status versions. " << "status: " << status;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenant_status_entity>> |
                       where("tenant_id"_c == tid && "status"_c == status) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<tenant_status_entity, domain::tenant_status>(
        ctx,
        query,
        [](const auto& entities) { return tenant_status_mapper::map(entities); },
        lg(),
        "Reading all tenant status versions by status.");
}

std::optional<domain::tenant_status> tenant_status_repository::read_at_version(
    context ctx, const std::string& status, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading tenant status at version. " << "status: " << status
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<tenant_status_entity>> |
        where("tenant_id"_c == tid && "status"_c == status && "version"_c == version) |
        sqlgen::limit(1);

    const auto entities = execute_read_query<tenant_status_entity, domain::tenant_status>(
        ctx,
        query,
        [](const auto& entities) { return tenant_status_mapper::map(entities); },
        lg(),
        "Reading tenant status at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void tenant_status_repository::remove(context ctx, const std::string& status) {
    BOOST_LOG_SEV(lg(), debug) << "Removing tenant status. " << "status: " << status;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenant_status_entity> |
        where("tenant_id"_c == tid && "status"_c == status && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing tenant status from database.");
}

std::vector<domain::tenant_status>
tenant_status_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest tenant statuses with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<tenant_status_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("status"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<tenant_status_entity, domain::tenant_status>(
        ctx,
        query,
        [](const auto& entities) { return tenant_status_mapper::map(entities); },
        lg(),
        "Reading latest tenant statuses with pagination.");
}

std::uint32_t tenant_status_repository::get_total_status_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active tenant status count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<tenant_status_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active tenant status count: " << count;
    return count;
}

void tenant_status_repository::remove(context ctx, const std::vector<std::string>& statuss) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<tenant_status_entity> |
        where("tenant_id"_c == tid && "status"_c.in(statuss) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing tenant statuses.");
}


}
