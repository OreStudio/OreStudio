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
import type { NettingSetAlias } from '../domain/netting_set_alias.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface NettingSetAliasKey {
    id_value: string;
}

export interface NettingSetAliasLookup {
    key: NettingSetAliasKey;
    netting_set_alias: NettingSetAlias | null;
}

export interface NettingSetAliasesFilter {
    id_value_one_of: string[] | null;
}

export interface NettingSetAliasEvent {
    event_id: string;
    key: NettingSetAliasKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListNettingSetAliasesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NettingSetAliasesFilter | null;
}

export interface ListNettingSetAliasesResponse {
    result: Result;
    netting_set_aliases: NettingSetAlias[];
    total: number;
}

export interface GetNettingSetAliasRequest {
    key: NettingSetAliasKey;
}

export interface GetNettingSetAliasResponse {
    result: Result;
    netting_set_alias: NettingSetAlias | null;
}

export interface GetManyNettingSetAliasesRequest {
    keys: NettingSetAliasKey[];
}

export interface GetManyNettingSetAliasesResponse {
    result: Result;
    entries: NettingSetAliasLookup[];
}

export const subjects = {
    list_netting_set_aliases_request: 'dq.v1.netting_set_aliases.list',
    get_netting_set_alias_request: 'dq.v1.netting_set_aliases.get',
    get_many_netting_set_aliases_request: 'dq.v1.netting_set_aliases.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_netting_set_aliases_request: true,
    get_netting_set_alias_request: true,
    get_many_netting_set_aliases_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.netting_set_aliases_events.created',
    updated: 'dq.v1.netting_set_aliases_events.updated',
    deleted: 'dq.v1.netting_set_aliases_events.deleted',
} as const;
