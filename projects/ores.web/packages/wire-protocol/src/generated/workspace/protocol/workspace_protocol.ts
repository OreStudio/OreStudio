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
import type { Workspace } from '../domain/workspace.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface WorkspaceKey {
    id: string;
}

export interface WorkspaceWrite {
    id: string;
    name: string;
    party_id: string;
    owner_id: string;
    description: string;
    source_path: string;
    parent_workspace_id: string | null;
    scope_portfolio_id: string | null;
    status_code: string;
}

export interface WorkspaceChange {
    write: WorkspaceWrite;
    precondition: Precondition;
}

export interface WorkspaceRemoval {
    key: WorkspaceKey;
    precondition: Precondition;
}

export interface WorkspaceLookup {
    key: WorkspaceKey;
    workspace: Workspace | null;
}

export interface WorkspacesFilter {
    id_one_of: string[] | null;
}

export interface WorkspaceEvent {
    event_id: string;
    key: WorkspaceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface WorkspaceVersionKey {
    workspace: WorkspaceKey;
    version: number;
}

export interface WorkspaceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListWorkspacesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: WorkspacesFilter | null;
    as_of: string | null;
}

export interface ListWorkspacesResponse {
    result: Result;
    workspaces: Workspace[];
    total: number;
}

export interface GetWorkspaceRequest {
    key: WorkspaceKey;
}

export interface GetWorkspaceResponse {
    result: Result;
    workspace: Workspace | null;
}

export interface GetManyWorkspacesRequest {
    keys: WorkspaceKey[];
}

export interface GetManyWorkspacesResponse {
    result: Result;
    entries: WorkspaceLookup[];
}

export interface PutWorkspaceRequest {
    change: WorkspaceChange;
    intent: ChangeIntent;
}

export interface PutWorkspaceResponse {
    result: Result;
    workspace: Workspace | null;
}

export interface PutManyWorkspacesRequest {
    changes: WorkspaceChange[];
    intent: ChangeIntent;
}

export interface PutManyWorkspacesResponse {
    result: Result;
    workspaces: Workspace[];
}

export interface DeleteWorkspaceRequest {
    removal: WorkspaceRemoval;
    intent: ChangeIntent;
}

export interface DeleteWorkspaceResponse {
    result: Result;
}

export interface DeleteManyWorkspacesRequest {
    removals: WorkspaceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyWorkspacesResponse {
    result: Result;
}

export interface ListWorkspaceVersionsRequest {
    key: WorkspaceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: WorkspaceVersionsFilter | null;
}

export interface ListWorkspaceVersionsResponse {
    result: Result;
    versions: Workspace[];
    total: number;
}

export interface GetWorkspaceVersionRequest {
    key: WorkspaceVersionKey;
}

export interface GetWorkspaceVersionResponse {
    result: Result;
    version: Workspace | null;
}

export const subjects = {
    list_workspaces_request: 'workspace.v1.workspaces.list',
    get_workspace_request: 'workspace.v1.workspaces.get',
    get_many_workspaces_request: 'workspace.v1.workspaces.get_many',
    put_workspace_request: 'workspace.v1.workspaces.put',
    put_many_workspaces_request: 'workspace.v1.workspaces.put_many',
    delete_workspace_request: 'workspace.v1.workspaces.delete',
    delete_many_workspaces_request: 'workspace.v1.workspaces.delete_many',
    list_workspace_versions_request: 'workspace.v1.workspaces_versions.list',
    get_workspace_version_request: 'workspace.v1.workspaces_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_workspaces_request: true,
    get_workspace_request: true,
    get_many_workspaces_request: true,
    put_workspace_request: true,
    put_many_workspaces_request: true,
    delete_workspace_request: true,
    delete_many_workspaces_request: true,
    list_workspace_versions_request: true,
    get_workspace_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'workspace.v1.workspaces_events.created',
    updated: 'workspace.v1.workspaces_events.updated',
    deleted: 'workspace.v1.workspaces_events.deleted',
} as const;
