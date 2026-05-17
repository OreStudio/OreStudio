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
#ifndef ORES_WORKSPACE_API_MESSAGING_WORKSPACE_PROTOCOL_HPP
#define ORES_WORKSPACE_API_MESSAGING_WORKSPACE_PROTOCOL_HPP

#include <string>
#include <vector>
#include "ores.workspace.api/domain/workspace.hpp"

namespace ores::workspace::messaging {

// ---------------------------------------------------------------------------
// List workspaces
// ---------------------------------------------------------------------------

struct list_workspaces_request {
    using response_type = struct list_workspaces_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.list";
    int offset = 0;
    int limit = 500;
};

struct list_workspaces_response {
    std::vector<ores::workspace::domain::workspace> workspaces;
};

// ---------------------------------------------------------------------------
// Create workspace
// ---------------------------------------------------------------------------

struct create_workspace_request {
    using response_type = struct create_workspace_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.create";
    ores::workspace::domain::workspace data;
};

struct create_workspace_response {
    bool success = false;
    std::string message;
    int id = 0;
};

// ---------------------------------------------------------------------------
// Archive workspace
// ---------------------------------------------------------------------------

struct archive_workspace_request {
    using response_type = struct archive_workspace_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.archive";
    int id = 0;
};

struct archive_workspace_response {
    bool success = false;
    std::string message;
};

// ---------------------------------------------------------------------------
// Resolve workspace (ancestor chain)
// ---------------------------------------------------------------------------

struct resolve_workspace_request {
    using response_type = struct resolve_workspace_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.resolve";
    int workspace_id = 0;
};

struct resolve_workspace_response {
    std::vector<int> resolution_order;
};

// ---------------------------------------------------------------------------
// Set trade scope
// ---------------------------------------------------------------------------

struct set_trade_scope_request {
    using response_type = struct set_trade_scope_response;
    static constexpr std::string_view nats_subject = "workspace.v1.trade-scope.set";
    int workspace_id = 0;
    std::vector<std::string> trade_ids;
};

struct set_trade_scope_response {
    bool success = false;
    std::string message;
};

// ---------------------------------------------------------------------------
// Clear trade scope
// ---------------------------------------------------------------------------

struct clear_trade_scope_request {
    using response_type = struct clear_trade_scope_response;
    static constexpr std::string_view nats_subject = "workspace.v1.trade-scope.clear";
    int workspace_id = 0;
};

struct clear_trade_scope_response {
    bool success = false;
    std::string message;
};

}

#endif
