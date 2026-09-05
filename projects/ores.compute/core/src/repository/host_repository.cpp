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
#include "ores.compute.core/repository/host_repository.hpp"
#include "ores.compute.api/domain/host_json_io.hpp" // IWYU pragma: keep.
#include "ores.compute.core/repository/host_entity.hpp"
#include "ores.compute.core/repository/host_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::compute::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string host_repository::sql() {
    return generate_create_table_sql<host_entity>(lg());
}

void host_repository::write(context ctx, const domain::host& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing compute host. " << "id: " << v.id;
    execute_write_query(ctx, host_mapper::map(v), lg(), "Writing compute host to database.");
}

void host_repository::write(context ctx, const std::vector<domain::host>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing compute hosts. Count: " << v.size();
    execute_write_query(ctx, host_mapper::map(v), lg(), "Writing compute hosts to database.");
}

std::vector<domain::host> host_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<host_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<host_entity, domain::host>(
        ctx,
        query,
        [](const auto& entities) { return host_mapper::map(entities); },
        lg(),
        "Reading latest compute hosts");
}

std::vector<domain::host> host_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute host. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<host_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<host_entity, domain::host>(
        ctx,
        query,
        [](const auto& entities) { return host_mapper::map(entities); },
        lg(),
        "Reading latest compute host by id.");
}


std::vector<domain::host> host_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all compute host versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<host_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<host_entity, domain::host>(
        ctx,
        query,
        [](const auto& entities) { return host_mapper::map(entities); },
        lg(),
        "Reading all compute host versions by id.");
}

std::optional<domain::host>
host_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading compute host at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<host_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<host_entity, domain::host>(
        ctx,
        query,
        [](const auto& entities) { return host_mapper::map(entities); },
        lg(),
        "Reading compute host at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void host_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing compute host. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<host_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing compute host from database.");
}

std::vector<domain::host>
host_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest compute hosts with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<host_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<host_entity, domain::host>(
        ctx,
        query,
        [](const auto& entities) { return host_mapper::map(entities); },
        lg(),
        "Reading latest compute hosts with pagination.");
}

std::uint32_t host_repository::get_total_host_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active compute host count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<host_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active compute host count: " << count;
    return count;
}

void host_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<host_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing compute hosts.");
}


}
