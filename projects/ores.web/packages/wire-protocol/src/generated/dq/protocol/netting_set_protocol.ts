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
import type { NettingSet } from '../domain/netting_set.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface NettingSetKey {
    code: string;
}

export interface NettingSetLookup {
    key: NettingSetKey;
    netting_set: NettingSet | null;
}

export interface NettingSetsFilter {
    code_one_of: string[] | null;
}

export interface NettingSetEvent {
    event_id: string;
    key: NettingSetKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListNettingSetsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NettingSetsFilter | null;
}

export interface ListNettingSetsResponse {
    result: Result;
    netting_sets: NettingSet[];
    total: number;
}

export interface GetNettingSetRequest {
    key: NettingSetKey;
}

export interface GetNettingSetResponse {
    result: Result;
    netting_set: NettingSet | null;
}

export interface GetManyNettingSetsRequest {
    keys: NettingSetKey[];
}

export interface GetManyNettingSetsResponse {
    result: Result;
    entries: NettingSetLookup[];
}

export const subjects = {
    list_netting_sets_request: 'dq.v1.netting_sets.list',
    get_netting_set_request: 'dq.v1.netting_sets.get',
    get_many_netting_sets_request: 'dq.v1.netting_sets.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_netting_sets_request: true,
    get_netting_set_request: true,
    get_many_netting_sets_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.netting_sets_events.created',
    updated: 'dq.v1.netting_sets_events.updated',
    deleted: 'dq.v1.netting_sets_events.deleted',
} as const;
