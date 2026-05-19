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
#include <boost/uuid/uuid_io.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.workspace.core/repository/workspace_entity.hpp"
#include "ores.workspace.core/repository/workspace_mapper.hpp"

namespace ores::workspace::repository {

using namespace ores::logging;
using namespace ores::database::repository;

workspace_repository::workspace_repository(context ctx)
    : ctx_(std::move(ctx)) {}

namespace {

// Columns must match the field order in parse_rows below.
constexpr std::string_view SELECT_COLS =
    "SELECT id::text, version, name, description, source_path,"
    "       parent_workspace_id::text, scope_portfolio_id::text,"
    "       owner_id::text, status_code,"
    "       modified_by, performed_by, change_reason_code, change_commentary,"
    "       valid_from::text"
    "  FROM ores_workspaces_tbl"
    " WHERE valid_to = ores_utility_infinity_timestamp_fn()";

std::vector<domain::workspace> parse_rows(
    const std::vector<std::vector<std::optional<std::string>>>& rows) {

    std::vector<domain::workspace> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        if (row.size() < 14) continue;
        workspace_entity e;
        if (row[0])  e.id                   = *row[0];
        if (row[1])  e.version              = std::stoi(*row[1]);
        if (row[2])  e.name                 = *row[2];
        if (row[3])  e.description          = *row[3];
        if (row[4])  e.source_path          = *row[4];
        if (row[5])  e.parent_workspace_id  = *row[5];
        if (row[6])  e.scope_portfolio_id   = *row[6];
        if (row[7])  e.owner_id             = *row[7];
        if (row[8])  e.status_code          = *row[8];
        if (row[9])  e.modified_by          = *row[9];
        if (row[10]) e.performed_by         = *row[10];
        if (row[11]) e.change_reason_code   = *row[11];
        if (row[12]) e.change_commentary    = *row[12];
        if (row[13]) e.valid_from           = db_timestamp(*row[13]);
        result.push_back(workspace_mapper::map(e));
    }
    return result;
}

} // anonymous namespace

std::vector<domain::workspace> workspace_repository::list_active() {
    BOOST_LOG_SEV(lg(), debug) << "Listing active workspaces.";

    const std::string sql = std::string(SELECT_COLS) +
        "   AND status_code = 'active'"
        " ORDER BY name";

    const auto rows = execute_raw_multi_column_query(
        ctx_, sql, lg(), "Listing active workspaces");
    return parse_rows(rows);
}

std::optional<domain::workspace>
workspace_repository::find_by_id(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding workspace by id: " << id;

    const std::string sql = std::string(SELECT_COLS) +
        "   AND id = $1::uuid";

    const auto rows = execute_parameterized_multi_column_query(
        ctx_, sql, {id}, lg(), "Finding workspace by id");

    if (rows.empty())
        return std::nullopt;
    auto parsed = parse_rows(rows);
    if (parsed.empty())
        return std::nullopt;
    return parsed.front();
}

std::string workspace_repository::create(const domain::workspace& ws) {
    BOOST_LOG_SEV(lg(), debug) << "Creating workspace: " << ws.name;

    // $5/$6 are optional UUIDs — pass empty string and use NULLIF to coerce to NULL.
    static constexpr std::string_view sql =
        "INSERT INTO ores_workspaces_tbl"
        "  (id, version, name, description, source_path,"
        "   parent_workspace_id, scope_portfolio_id,"
        "   owner_id, status_code,"
        "   modified_by, performed_by, change_reason_code, change_commentary,"
        "   valid_from, valid_to)"
        " VALUES ("
        "  $1::uuid, 0, $2, $3, $4,"
        "  NULLIF($5, '')::uuid, NULLIF($6, '')::uuid,"
        "  $7::uuid, $8,"
        "  $9, $10, $11, $12,"
        "  now(), 'infinity')"
        " RETURNING id::text";

    const std::string parent_val = ws.parent_workspace_id.has_value()
        ? boost::uuids::to_string(*ws.parent_workspace_id) : "";
    const std::string portfolio_val = ws.scope_portfolio_id.has_value()
        ? boost::uuids::to_string(*ws.scope_portfolio_id) : "";
    const std::string status = ws.status_code.empty() ? "active" : ws.status_code;

    const auto rows = execute_parameterized_multi_column_query(
        ctx_, std::string(sql),
        {boost::uuids::to_string(ws.id),
         ws.name, ws.description, ws.source_path,
         parent_val, portfolio_val,
         boost::uuids::to_string(ws.owner_id), status,
         ws.modified_by, ws.performed_by,
         ws.change_reason_code, ws.change_commentary},
        lg(), "Creating workspace");

    if (rows.empty() || !rows.front()[0].has_value())
        throw std::runtime_error("Failed to retrieve generated id after workspace insert");
    return *rows.front()[0];
}

void workspace_repository::archive(const std::string& id,
    const std::string& modified_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {

    BOOST_LOG_SEV(lg(), debug) << "Archiving workspace: " << id;

    const auto current = find_by_id(id);
    if (!current.has_value())
        throw std::runtime_error("Workspace not found: " + id);

    // $4/$5 are optional UUIDs — pass empty string and use NULLIF to coerce to NULL.
    // version is passed as $2 (text cast to int) since parameterized API is string-only.
    static constexpr std::string_view sql =
        "INSERT INTO ores_workspaces_tbl"
        "  (id, version, name, description, source_path,"
        "   parent_workspace_id, scope_portfolio_id,"
        "   owner_id, status_code,"
        "   modified_by, performed_by, change_reason_code, change_commentary,"
        "   valid_from, valid_to)"
        " VALUES ("
        "  $1::uuid, $2::int, $3, $4, $5,"
        "  NULLIF($6, '')::uuid, NULLIF($7, '')::uuid,"
        "  $8::uuid, 'archived',"
        "  $9, $9, $10, $11,"
        "  now(), 'infinity')";

    const std::string parent_val = current->parent_workspace_id.has_value()
        ? boost::uuids::to_string(*current->parent_workspace_id) : "";
    const std::string portfolio_val = current->scope_portfolio_id.has_value()
        ? boost::uuids::to_string(*current->scope_portfolio_id) : "";

    execute_parameterized_command(
        ctx_, std::string(sql),
        {id,
         std::to_string(current->version),
         current->name, current->description, current->source_path,
         parent_val, portfolio_val,
         boost::uuids::to_string(current->owner_id),
         modified_by, change_reason_code, change_commentary},
        lg(), "Archiving workspace");
}

void workspace_repository::remove(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing workspace: " << id;

    execute_parameterized_command(
        ctx_,
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = $1::uuid",
        {id}, lg(), "Clearing trade scope before remove");

    execute_parameterized_command(
        ctx_,
        "DELETE FROM ores_workspaces_tbl"
        " WHERE id = $1::uuid"
        "   AND valid_to = ores_utility_infinity_timestamp_fn()",
        {id}, lg(), "Soft-closing workspace");
}

std::vector<std::string>
workspace_repository::resolution_order(const std::string& workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Resolving workspace chain for: " << workspace_id;

    const std::string sql =
        "SELECT unnest(ores_workspace_resolution_order_fn($1::uuid))::text";

    return execute_parameterized_string_query(
        ctx_, sql, {workspace_id},
        lg(), "Resolving workspace resolution order");
}

void workspace_repository::set_trade_scope(const std::string& workspace_id,
    const std::vector<boost::uuids::uuid>& trade_ids) {

    BOOST_LOG_SEV(lg(), debug) << "Setting trade scope for workspace: " << workspace_id
                               << " trade count: " << trade_ids.size();

    execute_parameterized_command(
        ctx_,
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = $1::uuid",
        {workspace_id}, lg(), "Clearing existing trade scope");

    for (const auto& tid : trade_ids) {
        execute_parameterized_command(
            ctx_,
            "INSERT INTO ores_workspace_trade_scope_tbl (workspace_id, trade_id)"
            " VALUES ($1::uuid, $2::uuid)",
            {workspace_id, boost::uuids::to_string(tid)},
            lg(), "Inserting trade scope entry");
    }
}

void workspace_repository::clear_trade_scope(const std::string& workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Clearing trade scope for workspace: " << workspace_id;

    execute_parameterized_command(
        ctx_,
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = $1::uuid",
        {workspace_id}, lg(), "Clearing trade scope");
}

}
