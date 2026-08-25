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
#include "ores.compute.core/repository/app_version_repository.hpp"
#include "ores.compute.api/domain/app_version_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/app_version_entity.hpp"
#include "ores.compute.core/repository/app_version_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.database/service/tenant_context.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string app_version_repository::sql() {
    return generate_create_table_sql<app_version_entity>(lg());
}

void app_version_repository::write(context ctx, const domain::app_version& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing app version. " << "id: " << v.id;
    execute_write_query(ctx, app_version_mapper::map(v), lg(), "Writing app version to database.");
}

void app_version_repository::write(context ctx, const std::vector<domain::app_version>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing app versions. Count: " << v.size();
    execute_write_query(ctx, app_version_mapper::map(v), lg(), "Writing app versions to database.");
}

std::vector<domain::app_version> app_version_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        sqlgen::read<std::vector<app_version_entity>> |
        where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<app_version_entity, domain::app_version>(
        ctx,
        query,
        [](const auto& entities) { return app_version_mapper::map(entities); },
        lg(),
        "Reading latest app versions");
}

std::vector<domain::app_version> app_version_repository::read_latest(context ctx,
                                                                     const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app version. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = sqlgen::read<std::vector<app_version_entity>> |
                       where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "id"_c == id &&
                             "valid_to"_c == max.value());

    return execute_read_query<app_version_entity, domain::app_version>(
        ctx,
        query,
        [](const auto& entities) { return app_version_mapper::map(entities); },
        lg(),
        "Reading latest app version by id.");
}


std::vector<domain::app_version> app_version_repository::read_all(context ctx,
                                                                  const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all app version versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = sqlgen::read<std::vector<app_version_entity>> |
                       where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<app_version_entity, domain::app_version>(
        ctx,
        query,
        [](const auto& entities) { return app_version_mapper::map(entities); },
        lg(),
        "Reading all app version versions by id.");
}

std::optional<domain::app_version>
app_version_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading app version at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query = sqlgen::read<std::vector<app_version_entity>> |
                       where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "id"_c == id &&
                             "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<app_version_entity, domain::app_version>(
        ctx,
        query,
        [](const auto& entities) { return app_version_mapper::map(entities); },
        lg(),
        "Reading app version at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


void app_version_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing app version. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<app_version_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing app version from database.");
}

std::vector<domain::app_version>
app_version_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest app versions with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        sqlgen::read<std::vector<app_version_entity>> |
        where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "valid_to"_c == max.value()) |
        order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<app_version_entity, domain::app_version>(
        ctx,
        query,
        [](const auto& entities) { return app_version_mapper::map(entities); },
        lg(),
        "Reading latest app versions with pagination.");
}

std::uint32_t app_version_repository::get_total_app_version_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active app version count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    static const std::string sys(ores::database::service::tenant_context::system_tenant_id);
    const auto query =
        sqlgen::select_from<app_version_entity>(sqlgen::count().as<"count">()) |
        where(("tenant_id"_c == tid || "tenant_id"_c == sys) && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active app version count: " << count;
    return count;
}

void app_version_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<app_version_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing app versions.");
}


}
