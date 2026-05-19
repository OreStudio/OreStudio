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

    auto escape_str = [](const std::string& s) -> std::string {
        std::string r;
        r.reserve(s.size() + 2);
        r += '\'';
        for (char c : s) {
            if (c == '\'') r += "''";
            else r += c;
        }
        r += '\'';
        return r;
    };

    const std::string id_str = boost::uuids::to_string(ws.id);
    const std::string owner_str = boost::uuids::to_string(ws.owner_id);

    std::string parent_val = "NULL";
    if (ws.parent_workspace_id.has_value())
        parent_val = "'" + boost::uuids::to_string(*ws.parent_workspace_id) + "'::uuid";

    std::string portfolio_val = "NULL";
    if (ws.scope_portfolio_id.has_value())
        portfolio_val = "'" + boost::uuids::to_string(*ws.scope_portfolio_id) + "'::uuid";

    const std::string sql =
        "INSERT INTO ores_workspaces_tbl"
        "  (id, version, name, description, source_path,"
        "   parent_workspace_id, scope_portfolio_id,"
        "   owner_id, status_code,"
        "   modified_by, performed_by, change_reason_code, change_commentary,"
        "   valid_from, valid_to)"
        " VALUES ("
        "'" + id_str + "'::uuid, 0, "
        + escape_str(ws.name) + ", "
        + escape_str(ws.description) + ", "
        + escape_str(ws.source_path) + ", "
        + parent_val + ", "
        + portfolio_val + ", "
        "'" + owner_str + "'::uuid, "
        + escape_str(ws.status_code.empty() ? std::string("active") : ws.status_code) + ", "
        + escape_str(ws.modified_by) + ", "
        + escape_str(ws.performed_by) + ", "
        + escape_str(ws.change_reason_code) + ", "
        + escape_str(ws.change_commentary)
        + ", now(), 'infinity')"
        " RETURNING id::text";

    const auto rows = execute_raw_multi_column_query(
        ctx_, sql, lg(), "Creating workspace");

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

    auto escape_str = [](const std::string& s) -> std::string {
        std::string r;
        r.reserve(s.size() + 2);
        r += '\'';
        for (char c : s) {
            if (c == '\'') r += "''";
            else r += c;
        }
        r += '\'';
        return r;
    };

    const std::string owner_str = boost::uuids::to_string(current->owner_id);

    std::string parent_val = "NULL";
    if (current->parent_workspace_id.has_value())
        parent_val = "'" + boost::uuids::to_string(*current->parent_workspace_id) + "'::uuid";

    std::string portfolio_val = "NULL";
    if (current->scope_portfolio_id.has_value())
        portfolio_val = "'" + boost::uuids::to_string(*current->scope_portfolio_id) + "'::uuid";

    const std::string sql =
        "INSERT INTO ores_workspaces_tbl"
        "  (id, version, name, description, source_path,"
        "   parent_workspace_id, scope_portfolio_id,"
        "   owner_id, status_code,"
        "   modified_by, performed_by, change_reason_code, change_commentary,"
        "   valid_from, valid_to)"
        " VALUES ("
        "'" + id + "'::uuid, " + std::to_string(current->version) + ", "
        + escape_str(current->name) + ", "
        + escape_str(current->description) + ", "
        + escape_str(current->source_path) + ", "
        + parent_val + ", "
        + portfolio_val + ", "
        "'" + owner_str + "'::uuid, "
        "'archived', "
        + escape_str(modified_by) + ", "
        + escape_str(modified_by) + ", "
        + escape_str(change_reason_code) + ", "
        + escape_str(change_commentary)
        + ", now(), 'infinity')";

    execute_raw_command(ctx_, sql, lg(), "Archiving workspace");
}

void workspace_repository::remove(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing workspace: " << id;

    const std::string del_scope =
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = '"
        + id + "'::uuid";
    execute_raw_command(ctx_, del_scope, lg(), "Clearing trade scope before remove");

    const std::string del_ws =
        "DELETE FROM ores_workspaces_tbl"
        " WHERE id = '" + id + "'::uuid"
        "   AND valid_to = ores_utility_infinity_timestamp_fn()";
    execute_raw_command(ctx_, del_ws, lg(), "Soft-closing workspace");
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

    const std::string del_sql =
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = '"
        + workspace_id + "'::uuid";
    execute_raw_command(ctx_, del_sql, lg(), "Clearing existing trade scope");

    if (trade_ids.empty())
        return;

    std::string ins_sql =
        "INSERT INTO ores_workspace_trade_scope_tbl (workspace_id, trade_id) VALUES ";
    bool first = true;
    for (const auto& tid : trade_ids) {
        if (!first) ins_sql += ", ";
        ins_sql += "('" + workspace_id + "'::uuid, '"
                 + boost::uuids::to_string(tid) + "'::uuid)";
        first = false;
    }

    execute_raw_command(ctx_, ins_sql, lg(), "Inserting trade scope entries");
}

void workspace_repository::clear_trade_scope(const std::string& workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Clearing trade scope for workspace: " << workspace_id;

    const std::string sql =
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = '"
        + workspace_id + "'::uuid";

    execute_raw_command(ctx_, sql, lg(), "Clearing trade scope");
}

}
