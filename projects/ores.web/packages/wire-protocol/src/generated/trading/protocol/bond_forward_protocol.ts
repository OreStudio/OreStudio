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
import type { BondForward } from '../domain/bond_forward.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondForwardKey {
    trade_id: string;
}

export interface BondForwardWrite {
    trade_id: string;
    long_in_forward: string | null;
    forward_maturity_date: string | null;
    forward_settlement_date: string | null;
    settlement: string | null;
    amount: string | null;
    lock_rate: number | null;
    dv01: string | null;
    lock_rate_day_counter: string | null;
    settlement_dirty: string | null;
    premium_amount: string | null;
    premium_date: string | null;
}

export interface BondForwardChange {
    write: BondForwardWrite;
    precondition: Precondition;
}

export interface BondForwardRemoval {
    key: BondForwardKey;
    precondition: Precondition;
}

export interface BondForwardLookup {
    key: BondForwardKey;
    bond_forward: BondForward | null;
}

export interface BondForwardsFilter {
    trade_id_one_of: string[] | null;
}

export interface BondForwardEvent {
    event_id: string;
    key: BondForwardKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondForwardVersionKey {
    bond_forward: BondForwardKey;
    version: number;
}

export interface BondForwardVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondForwardsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BondForwardsFilter | null;
}

export interface ListBondForwardsResponse {
    result: Result;
    bond_forwards: BondForward[];
    total: number;
}

export interface GetBondForwardRequest {
    key: BondForwardKey;
}

export interface GetBondForwardResponse {
    result: Result;
    bond_forward: BondForward | null;
}

export interface GetManyBondForwardsRequest {
    keys: BondForwardKey[];
}

export interface GetManyBondForwardsResponse {
    result: Result;
    entries: BondForwardLookup[];
}

export interface PutBondForwardRequest {
    change: BondForwardChange;
    intent: ChangeIntent;
}

export interface PutBondForwardResponse {
    result: Result;
    bond_forward: BondForward | null;
}

export interface PutManyBondForwardsRequest {
    changes: BondForwardChange[];
    intent: ChangeIntent;
}

export interface PutManyBondForwardsResponse {
    result: Result;
    bond_forwards: BondForward[];
}

export interface DeleteBondForwardRequest {
    removal: BondForwardRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondForwardResponse {
    result: Result;
}

export interface DeleteManyBondForwardsRequest {
    removals: BondForwardRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondForwardsResponse {
    result: Result;
}

export interface ListBondForwardVersionsRequest {
    key: BondForwardKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondForwardVersionsFilter | null;
}

export interface ListBondForwardVersionsResponse {
    result: Result;
    versions: BondForward[];
    total: number;
}

export interface GetBondForwardVersionRequest {
    key: BondForwardVersionKey;
}

export interface GetBondForwardVersionResponse {
    result: Result;
    version: BondForward | null;
}

export const subjects = {
    list_bond_forwards_request: 'trading.v1.bond_forwards.list',
    get_bond_forward_request: 'trading.v1.bond_forwards.get',
    get_many_bond_forwards_request: 'trading.v1.bond_forwards.get_many',
    put_bond_forward_request: 'trading.v1.bond_forwards.put',
    put_many_bond_forwards_request: 'trading.v1.bond_forwards.put_many',
    delete_bond_forward_request: 'trading.v1.bond_forwards.delete',
    delete_many_bond_forwards_request: 'trading.v1.bond_forwards.delete_many',
    list_bond_forward_versions_request: 'trading.v1.bond_forwards_versions.list',
    get_bond_forward_version_request: 'trading.v1.bond_forwards_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_forwards_request: true,
    get_bond_forward_request: true,
    get_many_bond_forwards_request: true,
    put_bond_forward_request: true,
    put_many_bond_forwards_request: true,
    delete_bond_forward_request: true,
    delete_many_bond_forwards_request: true,
    list_bond_forward_versions_request: true,
    get_bond_forward_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_forwards_events.created',
    updated: 'trading.v1.bond_forwards_events.updated',
    deleted: 'trading.v1.bond_forwards_events.deleted',
} as const;
