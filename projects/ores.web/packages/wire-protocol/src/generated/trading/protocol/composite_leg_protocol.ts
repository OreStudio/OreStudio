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
import type { CompositeLeg } from '../domain/composite_leg.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CompositeLegKey {
    id: string;
}

export interface CompositeLegWrite {
    id: string;
    trade_id: string;
    leg_sequence: number;
    constituent_trade_id: string;
}

export interface CompositeLegChange {
    write: CompositeLegWrite;
    precondition: Precondition;
}

export interface CompositeLegRemoval {
    key: CompositeLegKey;
    precondition: Precondition;
}

export interface CompositeLegLookup {
    key: CompositeLegKey;
    composite_leg: CompositeLeg | null;
}

export interface CompositeLegsFilter {
    trade_id: string | null;
}

export interface CompositeLegEvent {
    event_id: string;
    key: CompositeLegKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CompositeLegVersionKey {
    composite_leg: CompositeLegKey;
    version: number;
}

export interface CompositeLegVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCompositeLegsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CompositeLegsFilter | null;
}

export interface ListCompositeLegsResponse {
    result: Result;
    composite_legs: CompositeLeg[];
    total: number;
}

export interface GetCompositeLegRequest {
    key: CompositeLegKey;
}

export interface GetCompositeLegResponse {
    result: Result;
    composite_leg: CompositeLeg | null;
}

export interface GetManyCompositeLegsRequest {
    keys: CompositeLegKey[];
}

export interface GetManyCompositeLegsResponse {
    result: Result;
    entries: CompositeLegLookup[];
}

export interface PutCompositeLegRequest {
    change: CompositeLegChange;
    intent: ChangeIntent;
}

export interface PutCompositeLegResponse {
    result: Result;
    composite_leg: CompositeLeg | null;
}

export interface PutManyCompositeLegsRequest {
    changes: CompositeLegChange[];
    intent: ChangeIntent;
}

export interface PutManyCompositeLegsResponse {
    result: Result;
    composite_legs: CompositeLeg[];
}

export interface DeleteCompositeLegRequest {
    removal: CompositeLegRemoval;
    intent: ChangeIntent;
}

export interface DeleteCompositeLegResponse {
    result: Result;
}

export interface DeleteManyCompositeLegsRequest {
    removals: CompositeLegRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCompositeLegsResponse {
    result: Result;
}

export interface ListByTradeIdCompositeLegsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CompositeLegsFilter | null;
}

export interface ListByTradeIdCompositeLegsResponse {
    result: Result;
    composite_legs: CompositeLeg[];
    total: number;
}

export interface ListCompositeLegVersionsRequest {
    key: CompositeLegKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CompositeLegVersionsFilter | null;
}

export interface ListCompositeLegVersionsResponse {
    result: Result;
    versions: CompositeLeg[];
    total: number;
}

export interface GetCompositeLegVersionRequest {
    key: CompositeLegVersionKey;
}

export interface GetCompositeLegVersionResponse {
    result: Result;
    version: CompositeLeg | null;
}

export const subjects = {
    list_composite_legs_request: 'trading.v1.composite_legs.list',
    get_composite_leg_request: 'trading.v1.composite_legs.get',
    get_many_composite_legs_request: 'trading.v1.composite_legs.get_many',
    put_composite_leg_request: 'trading.v1.composite_legs.put',
    put_many_composite_legs_request: 'trading.v1.composite_legs.put_many',
    delete_composite_leg_request: 'trading.v1.composite_legs.delete',
    delete_many_composite_legs_request: 'trading.v1.composite_legs.delete_many',
    list_by_trade_id_composite_legs_request: 'trading.v1.composite_legs.list_by_trade_id',
    list_composite_leg_versions_request: 'trading.v1.composite_legs_versions.list',
    get_composite_leg_version_request: 'trading.v1.composite_legs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_composite_legs_request: true,
    get_composite_leg_request: true,
    get_many_composite_legs_request: true,
    put_composite_leg_request: true,
    put_many_composite_legs_request: true,
    delete_composite_leg_request: true,
    delete_many_composite_legs_request: true,
    list_by_trade_id_composite_legs_request: true,
    list_composite_leg_versions_request: true,
    get_composite_leg_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.composite_legs_events.created',
    updated: 'trading.v1.composite_legs_events.updated',
    deleted: 'trading.v1.composite_legs_events.deleted',
} as const;
