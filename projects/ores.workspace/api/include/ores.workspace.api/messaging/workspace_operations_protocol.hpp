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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_WORKSPACE_API_MESSAGING_WORKSPACE_OPERATIONS_PROTOCOL_HPP
#define ORES_WORKSPACE_API_MESSAGING_WORKSPACE_OPERATIONS_PROTOCOL_HPP

#include <string>
#include <string_view>
#include <vector>

namespace ores::workspace::messaging {

/**
 * @brief Asks for the ancestor chain of one workspace.
 *
 * The chain starts at the named workspace and ends at the Live workspace, so
 * a caller resolves inherited data by asking which workspaces to read, in
 * order.
 */
struct resolve_workspace_request {
    using response_type = struct resolve_workspace_response;
    static constexpr std::string_view nats_subject = "workspace.v1.workspaces.resolve";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The workspace whose chain is asked for, as a UUID string.
     */
    std::string workspace_id;
};

/**
 * @brief The ancestor chain, closest workspace first.
 */
struct resolve_workspace_response {
    /**
     * @brief The workspace UUID strings, starting at the named workspace.
     */
    std::vector<std::string> resolution_order;
};

/**
 * @brief Replaces the trades a workspace is scoped to.
 *
 * The whitelist replaces whatever the workspace held before, so an empty list
 * clears it.
 */
struct set_trade_scope_request {
    using response_type = struct set_trade_scope_response;
    static constexpr std::string_view nats_subject = "workspace.v1.trade-scope.set";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The workspace whose trade scope is replaced, as a UUID string.
     */
    std::string workspace_id;
    /**
     * @brief The trade UUID strings the workspace is scoped to.
     */
    std::vector<std::string> trade_ids;
};

/**
 * @brief Whether the whitelist was replaced.
 */
struct set_trade_scope_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Asks for the trade scope of a workspace to be emptied.
 */
struct clear_trade_scope_request {
    using response_type = struct clear_trade_scope_response;
    static constexpr std::string_view nats_subject = "workspace.v1.trade-scope.clear";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The workspace whose trade scope is cleared, as a UUID string.
     */
    std::string workspace_id;
};

/**
 * @brief Whether the whitelist was emptied.
 */
struct clear_trade_scope_response {
    bool success = false;
    std::string message;
};

}

#endif
