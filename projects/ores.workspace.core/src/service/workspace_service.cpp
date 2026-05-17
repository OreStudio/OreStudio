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
#include "ores.workspace.core/service/workspace_service.hpp"

#include <stdexcept>

namespace ores::workspace::service {

using namespace ores::logging;

workspace_service::workspace_service(context ctx)
    : ctx_(std::move(ctx))
    , repo_(ctx_) {}

std::vector<domain::workspace> workspace_service::list_workspaces() {
    BOOST_LOG_SEV(lg(), debug) << "Listing workspaces.";
    return repo_.list_active();
}

std::optional<domain::workspace> workspace_service::get_workspace(int id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting workspace: " << id;
    return repo_.find_by_id(id);
}

int workspace_service::create_workspace(const domain::workspace& ws) {
    if (ws.name.empty()) {
        throw std::invalid_argument("Workspace name cannot be empty.");
    }
    if (!ws.status.empty() && ws.status != "active" && ws.status != "archived") {
        throw std::invalid_argument(
            "Invalid workspace status: '" + ws.status
            + "'. Must be 'active' or 'archived'.");
    }
    BOOST_LOG_SEV(lg(), debug) << "Creating workspace: " << ws.name;
    return repo_.create(ws);
}

void workspace_service::archive_workspace(int id) {
    BOOST_LOG_SEV(lg(), debug) << "Archiving workspace: " << id;
    repo_.archive(id);
}

std::vector<int> workspace_service::resolve(int workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Resolving workspace: " << workspace_id;
    return repo_.resolution_order(workspace_id);
}

void workspace_service::set_trade_scope(int workspace_id,
    const std::vector<boost::uuids::uuid>& trade_ids) {
    BOOST_LOG_SEV(lg(), debug) << "Setting trade scope for workspace: " << workspace_id;
    repo_.set_trade_scope(workspace_id, trade_ids);
}

void workspace_service::clear_trade_scope(int workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Clearing trade scope for workspace: " << workspace_id;
    repo_.clear_trade_scope(workspace_id);
}

}
