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
#include "ores.trading.core/repository/bond_issue_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.trading.api/domain/bond_issue_json_io.hpp" // IWYU pragma: keep.
#include "ores.trading.core/repository/bond_issue_entity.hpp"
#include "ores.trading.core/repository/bond_issue_mapper.hpp"
#include <sqlgen/postgres.hpp>

namespace ores::trading::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string bond_issue_repository::sql() {
    return generate_create_table_sql<bond_issue_entity>(lg());
}

void bond_issue_repository::write(context ctx, const domain::bond_issue& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing bond issue. " << "issue_id: " << v.issue_id;
    execute_write_query(ctx, bond_issue_mapper::map(v), lg(), "Writing bond issue to database.");
}

void bond_issue_repository::write(context ctx, const std::vector<domain::bond_issue>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing bond issues. Count: " << v.size();
    execute_write_query(ctx, bond_issue_mapper::map(v), lg(), "Writing bond issues to database.");
}

std::vector<domain::bond_issue> bond_issue_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto& chain = ctx.workspace_resolution();
    if (!chain.empty()) {
        const auto query = sqlgen::read<std::vector<bond_issue_entity>> |
                           where("tenant_id"_c == tid && "workspace_id"_c.in(chain) &&
                                 "valid_to"_c == max.value()) |
                           order_by("issue_id"_c);
        return execute_read_query<bond_issue_entity, domain::bond_issue>(
            ctx,
            query,
            [](const auto& entities) { return bond_issue_mapper::map(entities); },
            lg(),
            "Reading latest bond issues (workspace resolution chain).");
    }
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<bond_issue_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("issue_id"_c);

    return execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading latest bond issues");
}

std::vector<domain::bond_issue> bond_issue_repository::read_latest(context ctx,
                                                                   const std::string& issue_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest bond issue. " << "issue_id: " << issue_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<bond_issue_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "issue_id"_c == issue_id && "valid_to"_c == max.value());

    return execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading latest bond issue by issue_id.");
}


std::vector<domain::bond_issue> bond_issue_repository::read_all(context ctx,
                                                                const std::string& issue_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all bond issue versions. " << "issue_id: " << issue_id;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<bond_issue_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "issue_id"_c == issue_id) |
        order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading all bond issue versions by issue_id.");
}

std::optional<domain::bond_issue> bond_issue_repository::read_at_version(
    context ctx, const std::string& issue_id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading bond issue at version. " << "issue_id: " << issue_id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::read<std::vector<bond_issue_entity>> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "issue_id"_c == issue_id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading bond issue at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}

void bond_issue_repository::remove(context ctx, const std::string& issue_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing bond issue. " << "issue_id: " << issue_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<bond_issue_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "issue_id"_c == issue_id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing bond issue from database.");
}

std::vector<domain::bond_issue>
bond_issue_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest bond issues with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::read<std::vector<bond_issue_entity>> |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        order_by("issue_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<bond_issue_entity, domain::bond_issue>(
        ctx,
        query,
        [](const auto& entities) { return bond_issue_mapper::map(entities); },
        lg(),
        "Reading latest bond issues with pagination.");
}

std::uint32_t bond_issue_repository::get_total_issue_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active bond issue count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query =
        sqlgen::select_from<bond_issue_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "workspace_id"_c == wid && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active bond issue count: " << count;
    return count;
}

void bond_issue_repository::remove(context ctx, const std::vector<std::string>& issue_ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto wid = ctx.workspace_id();
    const auto query = sqlgen::delete_from<bond_issue_entity> |
                       where("tenant_id"_c == tid && "workspace_id"_c == wid &&
                             "issue_id"_c.in(issue_ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing bond issues.");
}


}
