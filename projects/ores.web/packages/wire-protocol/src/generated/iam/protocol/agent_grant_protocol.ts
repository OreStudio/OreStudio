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
import type { AgentGrant } from '../domain/agent_grant.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface AgentGrantKey {
    agent_name: string;
}

export interface AgentGrantLookup {
    key: AgentGrantKey;
    agent_grant: AgentGrant | null;
}

export interface AgentGrantsFilter {
    grantor_account_id: string | null;
    id_one_of: string[] | null;
    grantor_account_id_one_of: string[] | null;
}

export interface AgentGrantEvent {
    event_id: string;
    key: AgentGrantKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AgentGrantVersionKey {
    agent_grant: AgentGrantKey;
    version: number;
}

export interface AgentGrantVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAgentGrantsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AgentGrantsFilter | null;
    as_of: string | null;
}

export interface ListAgentGrantsResponse {
    result: Result;
    grants: AgentGrant[];
    total: number;
}

export interface GetAgentGrantRequest {
    key: AgentGrantKey;
}

export interface GetAgentGrantResponse {
    result: Result;
    agent_grant: AgentGrant | null;
}

export interface GetManyAgentGrantsRequest {
    keys: AgentGrantKey[];
}

export interface GetManyAgentGrantsResponse {
    result: Result;
    entries: AgentGrantLookup[];
}

export interface ListByGrantorAccountIdAgentGrantsRequest {
    grantor_account_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: AgentGrantsFilter | null;
}

export interface ListByGrantorAccountIdAgentGrantsResponse {
    result: Result;
    grants: AgentGrant[];
    total: number;
}

export interface ListAgentGrantVersionsRequest {
    key: AgentGrantKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AgentGrantVersionsFilter | null;
}

export interface ListAgentGrantVersionsResponse {
    result: Result;
    versions: AgentGrant[];
    total: number;
}

export interface GetAgentGrantVersionRequest {
    key: AgentGrantVersionKey;
}

export interface GetAgentGrantVersionResponse {
    result: Result;
    version: AgentGrant | null;
}

export const subjects = {
    list_agent_grants_request: 'iam.v1.agent_grants.list',
    get_agent_grant_request: 'iam.v1.agent_grants.get',
    get_many_agent_grants_request: 'iam.v1.agent_grants.get_many',
    list_by_grantor_account_id_agent_grants_request:
        'iam.v1.agent_grants.list_by_grantor_account_id',
    list_agent_grant_versions_request: 'iam.v1.agent_grants_versions.list',
    get_agent_grant_version_request: 'iam.v1.agent_grants_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_agent_grants_request: true,
    get_agent_grant_request: true,
    get_many_agent_grants_request: true,
    list_by_grantor_account_id_agent_grants_request: true,
    list_agent_grant_versions_request: true,
    get_agent_grant_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.events.agent_grants.created',
    updated: 'iam.v1.events.agent_grants.updated',
    deleted: 'iam.v1.events.agent_grants.deleted',
} as const;
