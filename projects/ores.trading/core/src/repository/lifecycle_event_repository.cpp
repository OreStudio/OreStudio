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
#include "ores.trading.core/repository/lifecycle_event_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/lifecycle_event_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/lifecycle_event_entity.hpp"
#include "ores.trading.core/repository/lifecycle_event_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string lifecycle_event_repository::sql() {
    return generate_create_table_sql<lifecycle_event_entity>(lg());
}

void lifecycle_event_repository::write(context ctx, const domain::lifecycle_event& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing lifecycle event. " << "code: " << v.code;
    execute_write_query(
        ctx, lifecycle_event_mapper::map(v), lg(), "Writing lifecycle event to database.");
}

void lifecycle_event_repository::write(context ctx, const std::vector<domain::lifecycle_event>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing lifecycle events. Count: " << v.size();
    execute_write_query(
        ctx, lifecycle_event_mapper::map(v), lg(), "Writing lifecycle events to database.");
}

std::vector<domain::lifecycle_event> lifecycle_event_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto& chain = ctx.workspace_resolution();
    if (!chain.empty()) {
        const auto query = sqlgen::read<std::vector<lifecycle_event_entity>> |
                           where("tenant_id"_c == tid && "workspace_id"_c.in(chain) &&
                                 "valid_to"_c == max.value()) |
                           order_by("code"_c);
        return execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
            ctx,
            query,
            [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
            lg(),
            "Reading latest lifecycle events (workspace resolution chain).");
    }
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<lifecycle_event_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("code"_c);

    return execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading latest lifecycle events");
}

std::vector<domain::lifecycle_event>
lifecycle_event_repository::read_latest(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest lifecycle event. " << "code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<lifecycle_event_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "code"_c == code &&
                             "valid_to"_c == max.value());

    return execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading latest lifecycle event by code.");
}


std::vector<domain::lifecycle_event> lifecycle_event_repository::read_all(context ctx,
                                                                          const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all lifecycle event versions. " << "code: " << code;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<lifecycle_event_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "code"_c == code) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading all lifecycle event versions by code.");
}

std::optional<domain::lifecycle_event> lifecycle_event_repository::read_at_version(
    context ctx, const std::string& code, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading lifecycle event at version. " << "code: " << code
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<lifecycle_event_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "code"_c == code &&
                             "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading lifecycle event at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void lifecycle_event_repository::remove(context ctx, const std::string& code) {
    BOOST_LOG_SEV(lg(), debug) << "Removing lifecycle event. " << "code: " << code;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<lifecycle_event_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid && "code"_c == code &&
                             "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing lifecycle event from database.");
}

std::vector<domain::lifecycle_event>
lifecycle_event_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest lifecycle events with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<lifecycle_event_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("code"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<lifecycle_event_entity, domain::lifecycle_event>(
        ctx,
        query,
        [](const auto& entities) { return lifecycle_event_mapper::map(entities); },
        lg(),
        "Reading latest lifecycle events with pagination.");
}

std::uint32_t lifecycle_event_repository::get_total_event_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active lifecycle event count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<lifecycle_event_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active lifecycle event count: " << count;
    return count;
}

void lifecycle_event_repository::remove(context ctx, const std::vector<std::string>& codes) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<lifecycle_event_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "code"_c.in(codes) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing lifecycle events.");
}


}
