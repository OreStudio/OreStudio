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
import type { Sandbox } from '../domain/sandbox.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface SandboxKey {
    name: string;
}

export interface SandboxWrite {
    id: string;
    name: string;
    purpose: string;
    anchor_portfolio_id: string;
    owner_account_id: string;
    visibility: string;
    status: string;
    review_date: string;
    description: string | null;
}

export interface SandboxChange {
    write: SandboxWrite;
    precondition: Precondition;
}

export interface SandboxRemoval {
    key: SandboxKey;
    precondition: Precondition;
}

export interface SandboxLookup {
    key: SandboxKey;
    sandbox: Sandbox | null;
}

export interface SandboxesFilter {
    anchor_portfolio_id: string | null;
}

export interface SandboxEvent {
    event_id: string;
    key: SandboxKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SandboxVersionKey {
    sandbox: SandboxKey;
    version: number;
}

export interface SandboxVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSandboxesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SandboxesFilter | null;
}

export interface ListSandboxesResponse {
    result: Result;
    sandboxes: Sandbox[];
    total: number;
}

export interface GetSandboxRequest {
    key: SandboxKey;
}

export interface GetSandboxResponse {
    result: Result;
    sandbox: Sandbox | null;
}

export interface GetManySandboxesRequest {
    keys: SandboxKey[];
}

export interface GetManySandboxesResponse {
    result: Result;
    entries: SandboxLookup[];
}

export interface PutSandboxRequest {
    change: SandboxChange;
    intent: ChangeIntent;
}

export interface PutSandboxResponse {
    result: Result;
    sandbox: Sandbox | null;
}

export interface PutManySandboxesRequest {
    changes: SandboxChange[];
    intent: ChangeIntent;
}

export interface PutManySandboxesResponse {
    result: Result;
    sandboxes: Sandbox[];
}

export interface DeleteSandboxRequest {
    removal: SandboxRemoval;
    intent: ChangeIntent;
}

export interface DeleteSandboxResponse {
    result: Result;
}

export interface DeleteManySandboxesRequest {
    removals: SandboxRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySandboxesResponse {
    result: Result;
}

export interface ListByAnchorPortfolioIdSandboxesRequest {
    anchor_portfolio_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: SandboxesFilter | null;
}

export interface ListByAnchorPortfolioIdSandboxesResponse {
    result: Result;
    sandboxes: Sandbox[];
    total: number;
}

export interface ListSandboxVersionsRequest {
    key: SandboxKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SandboxVersionsFilter | null;
}

export interface ListSandboxVersionsResponse {
    result: Result;
    versions: Sandbox[];
    total: number;
}

export interface GetSandboxVersionRequest {
    key: SandboxVersionKey;
}

export interface GetSandboxVersionResponse {
    result: Result;
    version: Sandbox | null;
}

export const subjects = {
    list_sandboxes_request: 'refdata.v1.sandboxes.list',
    get_sandbox_request: 'refdata.v1.sandboxes.get',
    get_many_sandboxes_request: 'refdata.v1.sandboxes.get_many',
    put_sandbox_request: 'refdata.v1.sandboxes.put',
    put_many_sandboxes_request: 'refdata.v1.sandboxes.put_many',
    delete_sandbox_request: 'refdata.v1.sandboxes.delete',
    delete_many_sandboxes_request: 'refdata.v1.sandboxes.delete_many',
    list_by_anchor_portfolio_id_sandboxes_request:
        'refdata.v1.sandboxes.list_by_anchor_portfolio_id',
    list_sandbox_versions_request: 'refdata.v1.sandboxes_versions.list',
    get_sandbox_version_request: 'refdata.v1.sandboxes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_sandboxes_request: true,
    get_sandbox_request: true,
    get_many_sandboxes_request: true,
    put_sandbox_request: true,
    put_many_sandboxes_request: true,
    delete_sandbox_request: true,
    delete_many_sandboxes_request: true,
    list_by_anchor_portfolio_id_sandboxes_request: true,
    list_sandbox_versions_request: true,
    get_sandbox_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.sandboxes_events.created',
    updated: 'refdata.v1.sandboxes_events.updated',
    deleted: 'refdata.v1.sandboxes_events.deleted',
} as const;
