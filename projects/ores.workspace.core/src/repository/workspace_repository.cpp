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
#include "ores.workspace.core/repository/workspace_repository.hpp"

#include <stdexcept>
#include <sqlgen/postgres.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/helpers.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.workspace.api/domain/workspace_json_io.hpp" // IWYU pragma: keep.
#include "ores.workspace.core/repository/workspace_entity.hpp"
#include "ores.workspace.core/repository/workspace_mapper.hpp"

namespace ores::workspace::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string workspace_repository::sql() {
    return generate_create_table_sql<workspace_entity>(lg());
}

void workspace_repository::write(context ctx, const domain::workspace& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing workspace: " << v.id;
    execute_write_query(ctx, workspace_mapper::map(v),
        lg(), "Writing workspace to database.");
}

void workspace_repository::write(
    context ctx, const std::vector<domain::workspace>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing workspaces. Count: " << v.size();
    execute_write_query(ctx, workspace_mapper::map(v),
        lg(), "Writing workspaces to database.");
}

std::vector<domain::workspace>
workspace_repository::read_latest(context ctx) {
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
        where("valid_to"_c == max.value()) |
        order_by("id"_c);

    return execute_read_query<workspace_entity, domain::workspace>(
        ctx, query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(), "Reading latest workspaces");
}

std::vector<domain::workspace>
workspace_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest workspace. id: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
        where("id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<workspace_entity, domain::workspace>(
        ctx, query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(), "Reading latest workspace by id.");
}

std::vector<domain::workspace>
workspace_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all workspace versions. id: " << id;
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
        where("id"_c == id) |
        order_by("version"_c.desc());

    return execute_read_query<workspace_entity, domain::workspace>(
        ctx, query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(), "Reading all workspace versions by id.");
}

void workspace_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing workspace: " << id;
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::delete_from<workspace_entity> |
        where("id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing workspace from database.");
}

std::vector<domain::workspace>
workspace_repository::read_history(context ctx, const std::string& id) {
    return read_all(ctx, id);
}

std::vector<domain::workspace>
workspace_repository::list_active(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Listing active workspaces.";
    static auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const std::string active("active");
    const auto query = sqlgen::read<std::vector<workspace_entity>> |
        where("valid_to"_c == max.value() && "status_code"_c == active) |
        order_by("name"_c);

    return execute_read_query<workspace_entity, domain::workspace>(
        ctx, query,
        [](const auto& entities) { return workspace_mapper::map(entities); },
        lg(), "Listing active workspaces");
}

std::optional<domain::workspace>
workspace_repository::find_by_id(context ctx, const std::string& id) {
    const auto results = read_latest(ctx, id);
    if (results.empty())
        return std::nullopt;
    return results.front();
}

void workspace_repository::archive(context ctx, const std::string& id,
    const std::string& modified_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {

    BOOST_LOG_SEV(lg(), debug) << "Archiving workspace: " << id;
    const auto current = find_by_id(ctx, id);
    if (!current)
        throw std::runtime_error("Workspace not found: " + id);

    domain::workspace archived = *current;
    archived.status_code = "archived";
    archived.modified_by = modified_by;
    archived.performed_by = modified_by;
    archived.change_reason_code = change_reason_code;
    archived.change_commentary = change_commentary;
    write(ctx, archived);
}

std::vector<std::string>
workspace_repository::resolution_order(context ctx,
    const std::string& workspace_id) {

    BOOST_LOG_SEV(lg(), debug) << "Resolving workspace chain for: " << workspace_id;
    const std::string sql =
        "SELECT unnest(ores_workspace_resolution_order_fn($1::uuid))::text";
    return execute_parameterized_string_query(
        ctx, sql, {workspace_id},
        lg(), "Resolving workspace resolution order");
}

void workspace_repository::set_trade_scope(context ctx,
    const std::string& workspace_id,
    const std::vector<boost::uuids::uuid>& trade_ids) {

    BOOST_LOG_SEV(lg(), debug) << "Setting trade scope for workspace: " << workspace_id
                               << " count: " << trade_ids.size();
    execute_parameterized_command(
        ctx,
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = $1::uuid",
        {workspace_id}, lg(), "Clearing existing trade scope");

    for (const auto& tid : trade_ids) {
        execute_parameterized_command(
            ctx,
            "INSERT INTO ores_workspace_trade_scope_tbl (workspace_id, trade_id)"
            " VALUES ($1::uuid, $2::uuid)",
            {workspace_id, boost::uuids::to_string(tid)},
            lg(), "Inserting trade scope entry");
    }
}

void workspace_repository::clear_trade_scope(context ctx,
    const std::string& workspace_id) {

    BOOST_LOG_SEV(lg(), debug) << "Clearing trade scope for workspace: " << workspace_id;
    execute_parameterized_command(
        ctx,
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = $1::uuid",
        {workspace_id}, lg(), "Clearing trade scope");
}

}
