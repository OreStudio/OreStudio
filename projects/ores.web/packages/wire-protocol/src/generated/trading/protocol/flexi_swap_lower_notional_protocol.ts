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
import type { FlexiSwapLowerNotional } from '../domain/flexi_swap_lower_notional.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FlexiSwapLowerNotionalKey {
    trade_id: string;
    sequence_number: number;
}

export interface FlexiSwapLowerNotionalWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    bound_number: number;
    currency: string | null;
    start_date: string | null;
    notional: string;
}

export interface FlexiSwapLowerNotionalChange {
    write: FlexiSwapLowerNotionalWrite;
    precondition: Precondition;
}

export interface FlexiSwapLowerNotionalRemoval {
    key: FlexiSwapLowerNotionalKey;
    precondition: Precondition;
}

export interface FlexiSwapLowerNotionalLookup {
    key: FlexiSwapLowerNotionalKey;
    flexi_swap_lower_notional: FlexiSwapLowerNotional | null;
}

export interface FlexiSwapLowerNotionalEvent {
    event_id: string;
    key: FlexiSwapLowerNotionalKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FlexiSwapLowerNotionalVersionKey {
    flexi_swap_lower_notional: FlexiSwapLowerNotionalKey;
    version: number;
}

export interface FlexiSwapLowerNotionalVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFlexiSwapLowerNotionalsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListFlexiSwapLowerNotionalsResponse {
    result: Result;
    flexi_swap_lower_notionals: FlexiSwapLowerNotional[];
    total: number;
}

export interface GetFlexiSwapLowerNotionalRequest {
    key: FlexiSwapLowerNotionalKey;
}

export interface GetFlexiSwapLowerNotionalResponse {
    result: Result;
    flexi_swap_lower_notional: FlexiSwapLowerNotional | null;
}

export interface GetManyFlexiSwapLowerNotionalsRequest {
    keys: FlexiSwapLowerNotionalKey[];
}

export interface GetManyFlexiSwapLowerNotionalsResponse {
    result: Result;
    entries: FlexiSwapLowerNotionalLookup[];
}

export interface PutFlexiSwapLowerNotionalRequest {
    change: FlexiSwapLowerNotionalChange;
    intent: ChangeIntent;
}

export interface PutFlexiSwapLowerNotionalResponse {
    result: Result;
    flexi_swap_lower_notional: FlexiSwapLowerNotional | null;
}

export interface PutManyFlexiSwapLowerNotionalsRequest {
    changes: FlexiSwapLowerNotionalChange[];
    intent: ChangeIntent;
}

export interface PutManyFlexiSwapLowerNotionalsResponse {
    result: Result;
    flexi_swap_lower_notionals: FlexiSwapLowerNotional[];
}

export interface DeleteFlexiSwapLowerNotionalRequest {
    removal: FlexiSwapLowerNotionalRemoval;
    intent: ChangeIntent;
}

export interface DeleteFlexiSwapLowerNotionalResponse {
    result: Result;
}

export interface DeleteManyFlexiSwapLowerNotionalsRequest {
    removals: FlexiSwapLowerNotionalRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFlexiSwapLowerNotionalsResponse {
    result: Result;
}

export interface ListFlexiSwapLowerNotionalVersionsRequest {
    key: FlexiSwapLowerNotionalKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FlexiSwapLowerNotionalVersionsFilter | null;
}

export interface ListFlexiSwapLowerNotionalVersionsResponse {
    result: Result;
    versions: FlexiSwapLowerNotional[];
    total: number;
}

export interface GetFlexiSwapLowerNotionalVersionRequest {
    key: FlexiSwapLowerNotionalVersionKey;
}

export interface GetFlexiSwapLowerNotionalVersionResponse {
    result: Result;
    version: FlexiSwapLowerNotional | null;
}

export const subjects = {
    list_flexi_swap_lower_notionals_request: 'trading.v1.flexi_swap_lower_notionals.list',
    get_flexi_swap_lower_notional_request: 'trading.v1.flexi_swap_lower_notionals.get',
    get_many_flexi_swap_lower_notionals_request: 'trading.v1.flexi_swap_lower_notionals.get_many',
    put_flexi_swap_lower_notional_request: 'trading.v1.flexi_swap_lower_notionals.put',
    put_many_flexi_swap_lower_notionals_request: 'trading.v1.flexi_swap_lower_notionals.put_many',
    delete_flexi_swap_lower_notional_request: 'trading.v1.flexi_swap_lower_notionals.delete',
    delete_many_flexi_swap_lower_notionals_request:
        'trading.v1.flexi_swap_lower_notionals.delete_many',
    list_flexi_swap_lower_notional_versions_request:
        'trading.v1.flexi_swap_lower_notionals_versions.list',
    get_flexi_swap_lower_notional_version_request:
        'trading.v1.flexi_swap_lower_notionals_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_flexi_swap_lower_notionals_request: true,
    get_flexi_swap_lower_notional_request: true,
    get_many_flexi_swap_lower_notionals_request: true,
    put_flexi_swap_lower_notional_request: true,
    put_many_flexi_swap_lower_notionals_request: true,
    delete_flexi_swap_lower_notional_request: true,
    delete_many_flexi_swap_lower_notionals_request: true,
    list_flexi_swap_lower_notional_versions_request: true,
    get_flexi_swap_lower_notional_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.flexi_swap_lower_notionals_events.created',
    updated: 'trading.v1.flexi_swap_lower_notionals_events.updated',
    deleted: 'trading.v1.flexi_swap_lower_notionals_events.deleted',
} as const;
