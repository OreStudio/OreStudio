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
import type { BondTrs } from '../domain/bond_trs.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondTrsKey {
    trade_id: string;
}

export interface BondTrsWrite {
    trade_id: string;
    trade_activity_id: string;
    return_type: string;
    funding_leg_type: string;
    funding_rate: string | null;
    funding_index: string;
    payer: string | null;
    price_type: string | null;
    initial_price: string | null;
}

export interface BondTrsChange {
    write: BondTrsWrite;
    precondition: Precondition;
}

export interface BondTrsRemoval {
    key: BondTrsKey;
    precondition: Precondition;
}

export interface BondTrsLookup {
    key: BondTrsKey;
    bond_trs: BondTrs | null;
}

export interface BondTrsFilter {
    trade_id_one_of: string[] | null;
}

export interface BondTrsEvent {
    event_id: string;
    key: BondTrsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondTrsVersionKey {
    bond_trs: BondTrsKey;
    version: number;
}

export interface BondTrsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondTrsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BondTrsFilter | null;
    as_of: string | null;
}

export interface ListBondTrsResponse {
    result: Result;
    trs: BondTrs[];
    total: number;
}

export interface GetBondTrsRequest {
    key: BondTrsKey;
}

export interface GetBondTrsResponse {
    result: Result;
    bond_trs: BondTrs | null;
}

export interface GetManyBondTrsRequest {
    keys: BondTrsKey[];
}

export interface GetManyBondTrsResponse {
    result: Result;
    entries: BondTrsLookup[];
}

export interface PutBondTrsRequest {
    change: BondTrsChange;
    intent: ChangeIntent;
}

export interface PutBondTrsResponse {
    result: Result;
    bond_trs: BondTrs | null;
}

export interface PutManyBondTrsRequest {
    changes: BondTrsChange[];
    intent: ChangeIntent;
}

export interface PutManyBondTrsResponse {
    result: Result;
    trs: BondTrs[];
}

export interface DeleteBondTrsRequest {
    removal: BondTrsRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondTrsResponse {
    result: Result;
}

export interface DeleteManyBondTrsRequest {
    removals: BondTrsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondTrsResponse {
    result: Result;
}

export interface ListBondTrsVersionsRequest {
    key: BondTrsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondTrsVersionsFilter | null;
}

export interface ListBondTrsVersionsResponse {
    result: Result;
    versions: BondTrs[];
    total: number;
}

export interface GetBondTrsVersionRequest {
    key: BondTrsVersionKey;
}

export interface GetBondTrsVersionResponse {
    result: Result;
    version: BondTrs | null;
}

export const subjects = {
    list_bond_trs_request: 'trading.v1.bond_trs.list',
    get_bond_trs_request: 'trading.v1.bond_trs.get',
    get_many_bond_trs_request: 'trading.v1.bond_trs.get_many',
    put_bond_trs_request: 'trading.v1.bond_trs.put',
    put_many_bond_trs_request: 'trading.v1.bond_trs.put_many',
    delete_bond_trs_request: 'trading.v1.bond_trs.delete',
    delete_many_bond_trs_request: 'trading.v1.bond_trs.delete_many',
    list_bond_trs_versions_request: 'trading.v1.bond_trs_versions.list',
    get_bond_trs_version_request: 'trading.v1.bond_trs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_trs_request: true,
    get_bond_trs_request: true,
    get_many_bond_trs_request: true,
    put_bond_trs_request: true,
    put_many_bond_trs_request: true,
    delete_bond_trs_request: true,
    delete_many_bond_trs_request: true,
    list_bond_trs_versions_request: true,
    get_bond_trs_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_trs_events.created',
    updated: 'trading.v1.bond_trs_events.updated',
    deleted: 'trading.v1.bond_trs_events.deleted',
} as const;
