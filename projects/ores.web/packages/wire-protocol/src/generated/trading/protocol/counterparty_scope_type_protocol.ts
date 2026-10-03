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
import type { CounterpartyScopeType } from '../domain/counterparty_scope_type.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CounterpartyScopeTypeKey {
    code: string;
}

export interface CounterpartyScopeTypeLookup {
    key: CounterpartyScopeTypeKey;
    counterparty_scope_type: CounterpartyScopeType | null;
}

export interface CounterpartyScopeTypeEvent {
    event_id: string;
    key: CounterpartyScopeTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListCounterpartyScopeTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCounterpartyScopeTypesResponse {
    result: Result;
    counterparty_scope_types: CounterpartyScopeType[];
    total: number;
}

export interface GetCounterpartyScopeTypeRequest {
    key: CounterpartyScopeTypeKey;
}

export interface GetCounterpartyScopeTypeResponse {
    result: Result;
    counterparty_scope_type: CounterpartyScopeType | null;
}

export interface GetManyCounterpartyScopeTypesRequest {
    keys: CounterpartyScopeTypeKey[];
}

export interface GetManyCounterpartyScopeTypesResponse {
    result: Result;
    entries: CounterpartyScopeTypeLookup[];
}

export const subjects = {
    list_counterparty_scope_types_request: 'trading.v1.counterparty_scope_types.list',
    get_counterparty_scope_type_request: 'trading.v1.counterparty_scope_types.get',
    get_many_counterparty_scope_types_request: 'trading.v1.counterparty_scope_types.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_counterparty_scope_types_request: true,
    get_counterparty_scope_type_request: true,
    get_many_counterparty_scope_types_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.counterparty_scope_types_events.created',
    updated: 'trading.v1.counterparty_scope_types_events.updated',
    deleted: 'trading.v1.counterparty_scope_types_events.deleted',
} as const;
