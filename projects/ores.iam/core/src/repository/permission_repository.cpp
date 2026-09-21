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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/repository/permission_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/permission_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/permission_entity.hpp"
#include "ores.iam.core/repository/permission_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string permission_repository::sql() {
    return generate_create_table_sql<permission_entity>(lg());
}

void permission_repository::write(context ctx, const domain::permission& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing permission. " << "id: " << v.id;
    execute_write_query(ctx, permission_mapper::map(v), lg(), "Writing permission to database.");
}

void permission_repository::write(context ctx, const std::vector<domain::permission>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing permissions. Count: " << v.size();
    execute_write_query(ctx, permission_mapper::map(v), lg(), "Writing permissions to database.");
}

std::vector<domain::permission> permission_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permissions");
}

std::vector<domain::permission> permission_repository::read_latest(context ctx,
                                                                   const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permission. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permission by id.");
}

std::vector<domain::permission>
permission_repository::read_latest_by_code(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permission by code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<permission_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permission by code.");
}

std::vector<domain::permission> permission_repository::read_all(context ctx,
                                                                const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all permission versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading all permission versions by id.");
}


permission_repository::remove_status permission_repository::remove(
    context ctx, const std::string& id, std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing permission. " << "id: " << id;
    // The store keeps no version column, so a caller that stated a version
    // asked a question this table cannot answer.
    if (version)
        return remove_status::unsupported;
    const auto current = read_latest(ctx, id);
    if (current.empty())
        return remove_status::missing;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<permission_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing permission from database.");
    return remove_status::removed;
}

void permission_repository::remove(context ctx, const std::string& id) {
    static_cast<void>(remove(ctx, id, std::nullopt));
}

std::vector<domain::permission>
permission_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest permissions with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permissions with pagination.");
}

std::uint32_t permission_repository::get_total_permission_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active permission count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<permission_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active permission count: " << count;
    return count;
}

std::vector<domain::permission>
permission_repository::read_latest(context ctx, const std::vector<std::string>& ids) {
    if (ids.empty())
        return {};
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<permission_entity>> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    auto result = execute_read_query<permission_entity, domain::permission>(
        ctx,
        query,
        [](const auto& entities) { return permission_mapper::map(entities); },
        lg(),
        "Reading latest permissions by ids.");
    return result;
}

void permission_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<permission_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing permissions.");
}


}
