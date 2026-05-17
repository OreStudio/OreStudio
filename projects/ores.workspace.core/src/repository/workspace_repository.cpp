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
#include <boost/uuid/string_generator.hpp>
#include "ores.database/repository/bitemporal_operations.hpp"

namespace ores::workspace::repository {

using namespace ores::logging;
using namespace ores::database::repository;

workspace_repository::workspace_repository(context ctx)
    : ctx_(std::move(ctx)) {}

domain::workspace workspace_repository::map_row(
    const std::vector<std::optional<std::string>>& row) {

    domain::workspace ws;
    ws.id = std::stoi(row[0].value());
    ws.name = row[1].value_or("");
    ws.description = row[2].value_or("");
    ws.source_path = row[3].value_or("");

    if (row[4].has_value()) {
        ws.parent_workspace_id = std::stoi(*row[4]);
    }

    if (row[5].has_value()) {
        boost::uuids::string_generator gen;
        ws.scope_portfolio_id = gen(*row[5]);
    }

    ws.created_by = row[6].value_or("");
    ws.status = row[7].value_or("active");
    return ws;
}

std::vector<domain::workspace> workspace_repository::list_active() {
    BOOST_LOG_SEV(lg(), debug) << "Listing active workspaces.";

    const std::string sql =
        "SELECT id, name, description, source_path, parent_workspace_id,"
        "       scope_portfolio_id::text, created_by, status"
        "  FROM ores_workspaces_tbl"
        " WHERE status = 'active'"
        " ORDER BY id";

    const auto rows = execute_raw_multi_column_query(
        ctx_, sql, lg(), "Listing active workspaces");

    std::vector<domain::workspace> result;
    result.reserve(rows.size());
    for (const auto& row : rows) {
        result.push_back(map_row(row));
    }
    return result;
}

std::optional<domain::workspace> workspace_repository::find_by_id(int id) {
    BOOST_LOG_SEV(lg(), debug) << "Finding workspace by id: " << id;

    const std::string sql =
        "SELECT id, name, description, source_path, parent_workspace_id,"
        "       scope_portfolio_id::text, created_by, status"
        "  FROM ores_workspaces_tbl"
        " WHERE id = " + std::to_string(id);

    const auto rows = execute_raw_multi_column_query(
        ctx_, sql, lg(), "Finding workspace by id");

    if (rows.empty()) {
        return std::nullopt;
    }
    return map_row(rows.front());
}

int workspace_repository::create(const domain::workspace& ws) {
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

    std::string parent_val = "NULL";
    if (ws.parent_workspace_id.has_value()) {
        parent_val = std::to_string(*ws.parent_workspace_id);
    }

    std::string portfolio_val = "NULL";
    if (ws.scope_portfolio_id.has_value()) {
        portfolio_val = "'" + boost::uuids::to_string(*ws.scope_portfolio_id) + "'::uuid";
    }

    const std::string sql =
        "INSERT INTO ores_workspaces_tbl"
        "  (name, description, source_path, parent_workspace_id, scope_portfolio_id, created_by)"
        " VALUES ("
        + escape_str(ws.name) + ", "
        + escape_str(ws.description) + ", "
        + escape_str(ws.source_path) + ", "
        + parent_val + ", "
        + portfolio_val + ", "
        + escape_str(ws.created_by)
        + ") RETURNING id";

    const auto rows = execute_raw_multi_column_query(
        ctx_, sql, lg(), "Creating workspace");

    if (rows.empty() || !rows.front()[0].has_value()) {
        throw std::runtime_error("Failed to retrieve generated id after workspace insert");
    }
    return std::stoi(*rows.front()[0]);
}

void workspace_repository::archive(int id) {
    BOOST_LOG_SEV(lg(), debug) << "Archiving workspace: " << id;

    const std::string sql =
        "UPDATE ores_workspaces_tbl SET status = 'archived' WHERE id = "
        + std::to_string(id);

    execute_raw_command(ctx_, sql, lg(), "Archiving workspace");
}

std::vector<int> workspace_repository::resolution_order(int workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Resolving workspace chain for: " << workspace_id;

    const std::string sql =
        "SELECT unnest(ores_workspace_resolution_order_fn($1::integer))";

    const auto strings = execute_parameterized_string_query(
        ctx_, sql, {std::to_string(workspace_id)},
        lg(), "Resolving workspace resolution order");

    std::vector<int> result;
    result.reserve(strings.size());
    for (const auto& s : strings) {
        result.push_back(std::stoi(s));
    }
    return result;
}

void workspace_repository::set_trade_scope(int workspace_id,
    const std::vector<boost::uuids::uuid>& trade_ids) {

    BOOST_LOG_SEV(lg(), debug) << "Setting trade scope for workspace: " << workspace_id
                               << " trade count: " << trade_ids.size();

    const std::string del_sql =
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = "
        + std::to_string(workspace_id);
    execute_raw_command(ctx_, del_sql, lg(), "Clearing existing trade scope");

    if (trade_ids.empty()) {
        return;
    }

    std::string ins_sql =
        "INSERT INTO ores_workspace_trade_scope_tbl (workspace_id, trade_id) VALUES ";
    bool first = true;
    for (const auto& tid : trade_ids) {
        if (!first) ins_sql += ", ";
        ins_sql += "(" + std::to_string(workspace_id) + ", '"
                 + boost::uuids::to_string(tid) + "'::uuid)";
        first = false;
    }

    execute_raw_command(ctx_, ins_sql, lg(), "Inserting trade scope entries");
}

void workspace_repository::clear_trade_scope(int workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Clearing trade scope for workspace: " << workspace_id;

    const std::string sql =
        "DELETE FROM ores_workspace_trade_scope_tbl WHERE workspace_id = "
        + std::to_string(workspace_id);

    execute_raw_command(ctx_, sql, lg(), "Clearing trade scope");
}

}
