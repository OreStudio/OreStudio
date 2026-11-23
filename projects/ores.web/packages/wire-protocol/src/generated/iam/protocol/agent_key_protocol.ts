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
import type { AgentKey } from '../domain/agent_key.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface AgentKeyKey {
    id: string;
}

export interface AgentKeyLookup {
    key: AgentKeyKey;
    agent_key: AgentKey | null;
}

export interface AgentKeysFilter {
    grant_id: string | null;
    id_one_of: string[] | null;
    grant_id_one_of: string[] | null;
}

export interface AgentKeyEvent {
    event_id: string;
    key: AgentKeyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AgentKeyVersionKey {
    agent_key: AgentKeyKey;
    version: number;
}

export interface AgentKeyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAgentKeysRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AgentKeysFilter | null;
    as_of: string | null;
}

export interface ListAgentKeysResponse {
    result: Result;
    keys: AgentKey[];
    total: number;
}

export interface GetAgentKeyRequest {
    key: AgentKeyKey;
}

export interface GetAgentKeyResponse {
    result: Result;
    agent_key: AgentKey | null;
}

export interface GetManyAgentKeysRequest {
    keys: AgentKeyKey[];
}

export interface GetManyAgentKeysResponse {
    result: Result;
    entries: AgentKeyLookup[];
}

export interface ListByGrantIdAgentKeysRequest {
    grant_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: AgentKeysFilter | null;
}

export interface ListByGrantIdAgentKeysResponse {
    result: Result;
    keys: AgentKey[];
    total: number;
}

export interface ListAgentKeyVersionsRequest {
    key: AgentKeyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AgentKeyVersionsFilter | null;
}

export interface ListAgentKeyVersionsResponse {
    result: Result;
    versions: AgentKey[];
    total: number;
}

export interface GetAgentKeyVersionRequest {
    key: AgentKeyVersionKey;
}

export interface GetAgentKeyVersionResponse {
    result: Result;
    version: AgentKey | null;
}

export const subjects = {
    list_agent_keys_request: 'iam.v1.agent_keys.list',
    get_agent_key_request: 'iam.v1.agent_keys.get',
    get_many_agent_keys_request: 'iam.v1.agent_keys.get_many',
    list_by_grant_id_agent_keys_request: 'iam.v1.agent_keys.list_by_grant_id',
    list_agent_key_versions_request: 'iam.v1.agent_keys_versions.list',
    get_agent_key_version_request: 'iam.v1.agent_keys_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_agent_keys_request: true,
    get_agent_key_request: true,
    get_many_agent_keys_request: true,
    list_by_grant_id_agent_keys_request: true,
    list_agent_key_versions_request: true,
    get_agent_key_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'iam.v1.events.agent_keys.created',
    updated: 'iam.v1.events.agent_keys.updated',
    deleted: 'iam.v1.events.agent_keys.deleted',
} as const;
