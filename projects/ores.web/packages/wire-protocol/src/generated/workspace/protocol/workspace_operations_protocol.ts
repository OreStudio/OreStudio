/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief Asks for the ancestor chain of one workspace.
 *
 * The chain starts at the named workspace and ends at the Live workspace, so
 * a caller resolves inherited data by asking which workspaces to read, in
 * order.
 */
export interface ResolveWorkspaceRequest {
    /**
     * @brief The workspace whose chain is asked for, as a UUID string.
     */
    workspace_id: string;
}

/**
 * @brief The ancestor chain, closest workspace first.
 */
export interface ResolveWorkspaceResponse {
    /**
     * @brief The workspace UUID strings, starting at the named workspace.
     */
    resolution_order: string[];
}

/**
 * @brief Replaces the trades a workspace is scoped to.
 *
 * The whitelist replaces whatever the workspace held before, so an empty list
 * clears it.
 */
export interface SetTradeScopeRequest {
    /**
     * @brief The workspace whose trade scope is replaced, as a UUID string.
     */
    workspace_id: string;
    /**
     * @brief The trade UUID strings the workspace is scoped to.
     */
    trade_ids: string[];
}

/**
 * @brief Whether the whitelist was replaced.
 */
export interface SetTradeScopeResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Asks for the trade scope of a workspace to be emptied.
 */
export interface ClearTradeScopeRequest {
    /**
     * @brief The workspace whose trade scope is cleared, as a UUID string.
     */
    workspace_id: string;
}

/**
 * @brief Whether the whitelist was emptied.
 */
export interface ClearTradeScopeResponse {
    success: boolean;
    message: string;
}

export const subjects = {
    resolve_workspace_request: 'workspace.v1.ops.resolve_workspace',
    set_trade_scope_request: 'workspace.v1.ops.set_trade_scope',
    clear_trade_scope_request: 'workspace.v1.ops.clear_trade_scope',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    resolve_workspace_request: true,
    set_trade_scope_request: true,
    clear_trade_scope_request: true,
} as const;
