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
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::workspace::service {

using namespace ores::logging;

workspace_service::workspace_service(context ctx)
    : ctx_(std::move(ctx)) {}

std::vector<domain::workspace> workspace_service::list_workspaces() {
    BOOST_LOG_SEV(lg(), debug) << "Listing workspaces.";
    return repo_.list_active(ctx_);
}

std::optional<domain::workspace>
workspace_service::get_workspace(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting workspace: " << id;
    return repo_.find_by_id(ctx_, id);
}

std::string workspace_service::create_workspace(const domain::workspace& ws) {
    if (ws.name.empty())
        throw std::invalid_argument("Workspace name cannot be empty.");

    if (!ws.status_code.empty()
        && ws.status_code != "active"
        && ws.status_code != "archived") {
        throw std::invalid_argument(
            "Invalid workspace status_code: '" + ws.status_code
            + "'. Must be 'active' or 'archived'.");
    }

    domain::workspace to_create = ws;
    if (to_create.id.is_nil()) {
        boost::uuids::random_generator gen;
        to_create.id = gen();
    }
    if (to_create.status_code.empty())
        to_create.status_code = "active";
    to_create.modified_by = ctx_.actor();

    BOOST_LOG_SEV(lg(), debug) << "Creating workspace: " << to_create.name;
    repo_.write(ctx_, to_create);
    return boost::uuids::to_string(to_create.id);
}

void workspace_service::archive_workspace(const std::string& id,
    const std::string& modified_by,
    const std::string& change_reason_code,
    const std::string& change_commentary) {

    BOOST_LOG_SEV(lg(), debug) << "Archiving workspace: " << id;
    repo_.archive(ctx_, id, ctx_.actor(), change_reason_code, change_commentary);
}

void workspace_service::remove_workspace(const std::string& id) {
    if (id == ores::utility::uuid::live_workspace_uuid_str)
        throw std::invalid_argument("Cannot delete the Live workspace.");
    BOOST_LOG_SEV(lg(), debug) << "Removing workspace: " << id;
    repo_.clear_trade_scope(ctx_, id);
    repo_.remove(ctx_, id);
}

std::vector<std::string>
workspace_service::resolve(const std::string& workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Resolving workspace: " << workspace_id;
    return repo_.resolution_order(ctx_, workspace_id);
}

void workspace_service::set_trade_scope(const std::string& workspace_id,
    const std::vector<boost::uuids::uuid>& trade_ids) {
    BOOST_LOG_SEV(lg(), debug) << "Setting trade scope for workspace: " << workspace_id;
    repo_.set_trade_scope(ctx_, workspace_id, trade_ids);
}

void workspace_service::clear_trade_scope(const std::string& workspace_id) {
    BOOST_LOG_SEV(lg(), debug) << "Clearing trade scope for workspace: " << workspace_id;
    repo_.clear_trade_scope(ctx_, workspace_id);
}

std::vector<domain::workspace>
workspace_service::get_workspace_history(const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Getting history for workspace: " << id;
    return repo_.read_history(ctx_, id);
}

}
