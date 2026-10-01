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
import type { BondLegAmount } from '../domain/bond_leg_amount.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondLegAmountKey {
    trade_id: string;
    leg_role: string;
    leg_number: number;
    amount_role: string;
    sequence_number: number;
}

export interface BondLegAmountWrite {
    trade_id: string;
    leg_role: string;
    leg_number: number;
    amount_role: string;
    sequence_number: number;
    value: string;
    start_date: string | null;
}

export interface BondLegAmountChange {
    write: BondLegAmountWrite;
    precondition: Precondition;
}

export interface BondLegAmountRemoval {
    key: BondLegAmountKey;
    precondition: Precondition;
}

export interface BondLegAmountLookup {
    key: BondLegAmountKey;
    bond_leg_amount: BondLegAmount | null;
}

export interface BondLegAmountEvent {
    event_id: string;
    key: BondLegAmountKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondLegAmountVersionKey {
    bond_leg_amount: BondLegAmountKey;
    version: number;
}

export interface BondLegAmountVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondLegAmountsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondLegAmountsResponse {
    result: Result;
    bond_leg_amounts: BondLegAmount[];
    total: number;
}

export interface GetBondLegAmountRequest {
    key: BondLegAmountKey;
}

export interface GetBondLegAmountResponse {
    result: Result;
    bond_leg_amount: BondLegAmount | null;
}

export interface GetManyBondLegAmountsRequest {
    keys: BondLegAmountKey[];
}

export interface GetManyBondLegAmountsResponse {
    result: Result;
    entries: BondLegAmountLookup[];
}

export interface PutBondLegAmountRequest {
    change: BondLegAmountChange;
    intent: ChangeIntent;
}

export interface PutBondLegAmountResponse {
    result: Result;
    bond_leg_amount: BondLegAmount | null;
}

export interface PutManyBondLegAmountsRequest {
    changes: BondLegAmountChange[];
    intent: ChangeIntent;
}

export interface PutManyBondLegAmountsResponse {
    result: Result;
    bond_leg_amounts: BondLegAmount[];
}

export interface DeleteBondLegAmountRequest {
    removal: BondLegAmountRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondLegAmountResponse {
    result: Result;
}

export interface DeleteManyBondLegAmountsRequest {
    removals: BondLegAmountRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondLegAmountsResponse {
    result: Result;
}

export interface ListBondLegAmountVersionsRequest {
    key: BondLegAmountKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondLegAmountVersionsFilter | null;
}

export interface ListBondLegAmountVersionsResponse {
    result: Result;
    versions: BondLegAmount[];
    total: number;
}

export interface GetBondLegAmountVersionRequest {
    key: BondLegAmountVersionKey;
}

export interface GetBondLegAmountVersionResponse {
    result: Result;
    version: BondLegAmount | null;
}

export const subjects = {
    list_bond_leg_amounts_request: 'trading.v1.bond_leg_amounts.list',
    get_bond_leg_amount_request: 'trading.v1.bond_leg_amounts.get',
    get_many_bond_leg_amounts_request: 'trading.v1.bond_leg_amounts.get_many',
    put_bond_leg_amount_request: 'trading.v1.bond_leg_amounts.put',
    put_many_bond_leg_amounts_request: 'trading.v1.bond_leg_amounts.put_many',
    delete_bond_leg_amount_request: 'trading.v1.bond_leg_amounts.delete',
    delete_many_bond_leg_amounts_request: 'trading.v1.bond_leg_amounts.delete_many',
    list_bond_leg_amount_versions_request: 'trading.v1.bond_leg_amounts_versions.list',
    get_bond_leg_amount_version_request: 'trading.v1.bond_leg_amounts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_leg_amounts_request: true,
    get_bond_leg_amount_request: true,
    get_many_bond_leg_amounts_request: true,
    put_bond_leg_amount_request: true,
    put_many_bond_leg_amounts_request: true,
    delete_bond_leg_amount_request: true,
    delete_many_bond_leg_amounts_request: true,
    list_bond_leg_amount_versions_request: true,
    get_bond_leg_amount_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_leg_amounts_events.created',
    updated: 'trading.v1.bond_leg_amounts_events.updated',
    deleted: 'trading.v1.bond_leg_amounts_events.deleted',
} as const;
