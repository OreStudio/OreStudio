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
import type { BondFuture } from '../domain/bond_future.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondFutureKey {
    trade_id: string;
}

export interface BondFutureWrite {
    trade_id: string;
    trade_activity_id: string;
    contract_name: string;
    contract_notional: string;
    long_short: string;
    apply_conversion_factor: boolean | null;
    use_future_price: boolean | null;
}

export interface BondFutureChange {
    write: BondFutureWrite;
    precondition: Precondition;
}

export interface BondFutureRemoval {
    key: BondFutureKey;
    precondition: Precondition;
}

export interface BondFutureLookup {
    key: BondFutureKey;
    bond_future: BondFuture | null;
}

export interface BondFuturesFilter {
    trade_id_one_of: string[] | null;
}

export interface BondFutureEvent {
    event_id: string;
    key: BondFutureKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondFutureVersionKey {
    bond_future: BondFutureKey;
    version: number;
}

export interface BondFutureVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondFuturesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BondFuturesFilter | null;
    as_of: string | null;
}

export interface ListBondFuturesResponse {
    result: Result;
    futures: BondFuture[];
    total: number;
}

export interface GetBondFutureRequest {
    key: BondFutureKey;
}

export interface GetBondFutureResponse {
    result: Result;
    bond_future: BondFuture | null;
}

export interface GetManyBondFuturesRequest {
    keys: BondFutureKey[];
}

export interface GetManyBondFuturesResponse {
    result: Result;
    entries: BondFutureLookup[];
}

export interface PutBondFutureRequest {
    change: BondFutureChange;
    intent: ChangeIntent;
}

export interface PutBondFutureResponse {
    result: Result;
    bond_future: BondFuture | null;
}

export interface PutManyBondFuturesRequest {
    changes: BondFutureChange[];
    intent: ChangeIntent;
}

export interface PutManyBondFuturesResponse {
    result: Result;
    futures: BondFuture[];
}

export interface DeleteBondFutureRequest {
    removal: BondFutureRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondFutureResponse {
    result: Result;
}

export interface DeleteManyBondFuturesRequest {
    removals: BondFutureRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondFuturesResponse {
    result: Result;
}

export interface ListBondFutureVersionsRequest {
    key: BondFutureKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondFutureVersionsFilter | null;
}

export interface ListBondFutureVersionsResponse {
    result: Result;
    versions: BondFuture[];
    total: number;
}

export interface GetBondFutureVersionRequest {
    key: BondFutureVersionKey;
}

export interface GetBondFutureVersionResponse {
    result: Result;
    version: BondFuture | null;
}

export const subjects = {
    list_bond_futures_request: 'trading.v1.bond_futures.list',
    get_bond_future_request: 'trading.v1.bond_futures.get',
    get_many_bond_futures_request: 'trading.v1.bond_futures.get_many',
    put_bond_future_request: 'trading.v1.bond_futures.put',
    put_many_bond_futures_request: 'trading.v1.bond_futures.put_many',
    delete_bond_future_request: 'trading.v1.bond_futures.delete',
    delete_many_bond_futures_request: 'trading.v1.bond_futures.delete_many',
    list_bond_future_versions_request: 'trading.v1.bond_futures_versions.list',
    get_bond_future_version_request: 'trading.v1.bond_futures_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_futures_request: true,
    get_bond_future_request: true,
    get_many_bond_futures_request: true,
    put_bond_future_request: true,
    put_many_bond_futures_request: true,
    delete_bond_future_request: true,
    delete_many_bond_futures_request: true,
    list_bond_future_versions_request: true,
    get_bond_future_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_futures_events.created',
    updated: 'trading.v1.bond_futures_events.updated',
    deleted: 'trading.v1.bond_futures_events.deleted',
} as const;
