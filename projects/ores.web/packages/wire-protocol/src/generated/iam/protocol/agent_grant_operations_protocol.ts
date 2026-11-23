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
import type { AgentCall } from '../domain/agent_call.js';
import type { AgentGrant } from '../domain/agent_grant.js';
import type { AgentHeldWrite } from '../domain/agent_held_write.js';
import type { AgentKey } from '../domain/agent_key.js';

/**
 * @brief Records a person's consent that one named agent acts for them.
 *
 * Sent by the person, with their own session, who must hold
 * iam::agent_grants:create and every permission of the role. The grant's
 * tenant and party are the session's, and its grantor is the session's
 * account. Refused when the session names an agent, so an agent never makes a
 * grant, and when the role names a permission withheld from agents. The
 * response carries the grant's first key, the only time it is shown.
 */
export interface CreateAgentGrantRequest {
    /**
     * @brief The name the person gives the agent.
     */
    agent_name: string;
    /**
     * @brief The name of the role the agent acts under.
     */
    role: string;
    /**
     * @brief How many days the grant serves, from one to ninety.
     */
    valid_days: number;
}

/**
 * @brief The grant and its first key, or why there is none.
 */
export interface CreateAgentGrantResponse {
    success: boolean;
    message: string;
    grant_id: string;
    /**
     * @brief The key, shown this once and never stored.
     */
    key: string;
}

/**
 * @brief Makes another key for a grant, so a key can be replaced.
 *
 * Sent by the grant's person, with their own session.
 */
export interface CreateAgentKeyRequest {
    grant_id: string;
}

/**
 * @brief The new key, or why there is none.
 */
export interface CreateAgentKeyResponse {
    success: boolean;
    message: string;
    key_id: string;
    /**
     * @brief The key, shown this once and never stored.
     */
    key: string;
}

/**
 * @brief Ends a grant, and with it every key that redeems it.
 *
 * Sent by the grant's person or by an administrator.
 */
export interface RevokeAgentGrantRequest {
    grant_id: string;
    reason: string;
}

/**
 * @brief Whether the grant was revoked.
 */
export interface RevokeAgentGrantResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Ends one key, leaving the grant and its other keys.
 *
 * Sent by the grant's person or by an administrator.
 */
export interface RevokeAgentKeyRequest {
    key_id: string;
    reason: string;
}

/**
 * @brief Whether the key was revoked.
 */
export interface RevokeAgentKeyResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Asks IAM for a token, presenting an agent's key.
 *
 * Sent by ores.http, with its own token, when a request carries an agent key
 * as its bearer token. The key names the tenant to read it in.
 */
export interface ExchangeAgentKeyRequest {
    key: string;
    /**
     * @brief The agent's address as ores.http saw it, for its session and the log.
     */
    client_address: string;
}

/**
 * @brief The token naming the person and the agent, or why there is none.
 */
export interface ExchangeAgentKeyResponse {
    success: boolean;
    message: string;
    token: string;
    /**
     * @brief When the token stops working, in seconds since the epoch.
     */
    expires_at: number;
}

/**
 * @brief Lists the caller's own agent grants and their keys.
 *
 * A self read: sent by a person who holds iam::agent_grants:create, with
 * their own session, and answering only grants whose grantor is the caller.
 * Reading another person's grants is the privileged read of the generated
 * entity protocol, which needs iam::agent_grants:read.
 */
export interface ListOwnAgentGrantsRequest {}

/**
 * @brief The caller's grants and their keys, never a key's secret.
 */
export interface ListOwnAgentGrantsResponse {
    success: boolean;
    message: string;
    grants: AgentGrant[];
    keys: AgentKey[];
}

/**
 * @brief Lists every agent grant and its keys in the caller's parties.
 *
 * The privileged read: it needs iam::agent_grants:read, which administrators
 * hold and no agent does. Row-level security keeps it to the parties the
 * caller sees.
 */
export interface ListEveryAgentGrantRequest {}

/**
 * @brief Every grant the caller sees and its keys, never a key's secret or hash.
 */
export interface ListEveryAgentGrantResponse {
    success: boolean;
    message: string;
    grants: AgentGrant[];
    keys: AgentKey[];
}

/**
 * @brief Lists the calls the caller's own LLM agents made, newest first.
 *
 * A self read, like the caller's own grants: it needs
 * iam::agent_grants:create and is refused to an agent.
 */
export interface ListOwnAgentActivityRequest {
    grant_id: string;
    offset: number;
    limit: number;
}

/**
 * @brief The caller's agents' calls: route, method, answer and duration, never
 * a request or its answer.
 */
export interface ListOwnAgentActivityResponse {
    success: boolean;
    message: string;
    calls: AgentCall[];
}

/**
 * @brief Asks for the token that replays an LLM agent's approved write.
 *
 * Only the gateway asks, under its own identity and iam::agent_keys:exchange,
 * once it has read the request's approval and claimed the held write. IAM
 * issues it only for a held write that is being applied, and checks the grant
 * and the person again, as an exchange does. Never a route.
 */
export interface IssueAgentReplayTokenRequest {
    tenant_id: string;
    request_id: string;
}

/**
 * @brief The replay token, naming the person and the agent, or a refusal.
 */
export interface IssueAgentReplayTokenResponse {
    success: boolean;
    message: string;
    token: string;
}

/**
 * @brief Reads the write an LLM agent made that an approval request holds.
 *
 * The review panel's read: the person the agent acts for reads their own, and
 * a holder of iam::agent_grants:read reads any in their parties. An agent never
 * reads one.
 */
export interface GetAgentHeldWriteRequest {
    request_id: string;
}

/**
 * @brief The held call, the agent that made it and what its replay answered.
 */
export interface GetAgentHeldWriteResponse {
    success: boolean;
    message: string;
    write: AgentHeldWrite;
    agent_name: string;
}

export const subjects = {
    create_agent_grant_request: 'iam.v1.agent_grants.put',
    create_agent_key_request: 'iam.v1.ops.create_agent_key',
    revoke_agent_grant_request: 'iam.v1.ops.revoke_agent_grant',
    revoke_agent_key_request: 'iam.v1.ops.revoke_agent_key',
    exchange_agent_key_request: 'iam.v1.ops.exchange_agent_key',
    list_own_agent_grants_request: 'iam.v1.ops.list_own_agent_grants',
    list_every_agent_grant_request: 'iam.v1.ops.list_every_agent_grant',
    list_own_agent_activity_request: 'iam.v1.ops.list_own_agent_activity',
    issue_agent_replay_token_request: 'iam.v1.ops.issue_agent_replay_token',
    get_agent_held_write_request: 'iam.v1.ops.get_agent_held_write',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    create_agent_grant_request: true,
    create_agent_key_request: true,
    revoke_agent_grant_request: true,
    revoke_agent_key_request: true,
    exchange_agent_key_request: true,
    list_own_agent_grants_request: true,
    list_every_agent_grant_request: true,
    list_own_agent_activity_request: true,
    issue_agent_replay_token_request: true,
    get_agent_held_write_request: true,
} as const;
