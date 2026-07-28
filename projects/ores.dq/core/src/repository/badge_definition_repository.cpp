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
#include "ores.dq.core/repository/badge_definition_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.dq.api/domain/badge_definition_json_io.hpp" // IWYU pragma: keep.
#include "ores.dq.core/repository/badge_definition_entity.hpp"
#include "ores.dq.core/repository/badge_definition_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::dq::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string badge_definition_repository::sql() {
    return generate_create_table_sql<badge_definition_entity>(lg());
}

void badge_definition_repository::write(context ctx, const domain::badge_definition& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing badge definition: " << v.code;
    execute_write_query(
        ctx, badge_definition_mapper::map(v), lg(), "Writing badge definition to database.");
}

void badge_definition_repository::write(context ctx,
                                        const std::vector<domain::badge_definition>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing badge definitions. Count: " << v.size();
    execute_write_query(
        ctx, badge_definition_mapper::map(v), lg(), "Writing badge definitions to database.");
}

std::vector<domain::badge_definition> badge_definition_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_definition_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c);

    return execute_read_query<badge_definition_entity, domain::badge_definition>(
        ctx,
        query,
        [](const auto& entities) { return badge_definition_mapper::map(entities); },
        lg(),
        "Reading latest badge definitions");
}

std::vector<domain::badge_definition>
badge_definition_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest badge definition. code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<badge_definition_entity>> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    return execute_read_query<badge_definition_entity, domain::badge_definition>(
        ctx,
        query,
        [](const auto& entities) { return badge_definition_mapper::map(entities); },
        lg(),
        "Reading latest badge definition by code.");
}

std::vector<domain::badge_definition>
badge_definition_repository::read_all(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all badge definition versions. code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_definition_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<badge_definition_entity, domain::badge_definition>(
        ctx,
        query,
        [](const auto& entities) { return badge_definition_mapper::map(entities); },
        lg(),
        "Reading all badge definition versions by code.");
}

std::optional<domain::badge_definition> badge_definition_repository::read_at_version(
    context ctx, const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading badge definition at version. code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_definition_entity>> |
                       where("tenant_id"_c == tid && "code"_c == code && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<badge_definition_entity, domain::badge_definition>(
        ctx,
        query,
        [](const auto& entities) { return badge_definition_mapper::map(entities); },
        lg(),
        "Reading badge definition at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void badge_definition_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing badge definition: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<badge_definition_entity> |
        where("tenant_id"_c == tid && "code"_c == code && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing badge definition from database.");
}

std::vector<domain::badge_definition>
badge_definition_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest badge definitions with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<badge_definition_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<badge_definition_entity, domain::badge_definition>(
        ctx,
        query,
        [](const auto& entities) { return badge_definition_mapper::map(entities); },
        lg(),
        "Reading latest badge definitions with pagination.");
}

std::uint32_t badge_definition_repository::get_total_definition_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active badge definition count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<badge_definition_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active badge definition count: " << count;
    return count;
}

void badge_definition_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<badge_definition_entity> |
        where("tenant_id"_c == tid && "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing badge definitions.");
}


}
