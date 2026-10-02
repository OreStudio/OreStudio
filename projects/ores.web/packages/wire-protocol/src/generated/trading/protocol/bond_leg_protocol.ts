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
import type { BondLeg } from '../domain/bond_leg.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondLegKey {
    trade_id: string;
    leg_role: string;
    leg_number: number;
}

export interface BondLegWrite {
    trade_id: string;
    leg_role: string;
    leg_number: number;
    payer: boolean | null;
    leg_type: string | null;
    currency: string | null;
    payment_convention: string | null;
    payment_lag: string | null;
    payment_calendar: string | null;
    day_counter: string | null;
    last_period_day_counter: string | null;
    notional_payment_lag: number | null;
    strict_notional_dates: boolean | null;
    indexings_from_asset_leg: boolean | null;
    settlement_fx_index: string | null;
    settlement_fixing_date: string | null;
}

export interface BondLegChange {
    write: BondLegWrite;
    precondition: Precondition;
}

export interface BondLegRemoval {
    key: BondLegKey;
    precondition: Precondition;
}

export interface BondLegLookup {
    key: BondLegKey;
    bond_leg: BondLeg | null;
}

export interface BondLegEvent {
    event_id: string;
    key: BondLegKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondLegVersionKey {
    bond_leg: BondLegKey;
    version: number;
}

export interface BondLegVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondLegsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondLegsResponse {
    result: Result;
    bond_legs: BondLeg[];
    total: number;
}

export interface GetBondLegRequest {
    key: BondLegKey;
}

export interface GetBondLegResponse {
    result: Result;
    bond_leg: BondLeg | null;
}

export interface GetManyBondLegsRequest {
    keys: BondLegKey[];
}

export interface GetManyBondLegsResponse {
    result: Result;
    entries: BondLegLookup[];
}

export interface PutBondLegRequest {
    change: BondLegChange;
    intent: ChangeIntent;
}

export interface PutBondLegResponse {
    result: Result;
    bond_leg: BondLeg | null;
}

export interface PutManyBondLegsRequest {
    changes: BondLegChange[];
    intent: ChangeIntent;
}

export interface PutManyBondLegsResponse {
    result: Result;
    bond_legs: BondLeg[];
}

export interface DeleteBondLegRequest {
    removal: BondLegRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondLegResponse {
    result: Result;
}

export interface DeleteManyBondLegsRequest {
    removals: BondLegRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondLegsResponse {
    result: Result;
}

export interface ListBondLegVersionsRequest {
    key: BondLegKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondLegVersionsFilter | null;
}

export interface ListBondLegVersionsResponse {
    result: Result;
    versions: BondLeg[];
    total: number;
}

export interface GetBondLegVersionRequest {
    key: BondLegVersionKey;
}

export interface GetBondLegVersionResponse {
    result: Result;
    version: BondLeg | null;
}

export const subjects = {
    list_bond_legs_request: 'trading.v1.bond_legs.list',
    get_bond_leg_request: 'trading.v1.bond_legs.get',
    get_many_bond_legs_request: 'trading.v1.bond_legs.get_many',
    put_bond_leg_request: 'trading.v1.bond_legs.put',
    put_many_bond_legs_request: 'trading.v1.bond_legs.put_many',
    delete_bond_leg_request: 'trading.v1.bond_legs.delete',
    delete_many_bond_legs_request: 'trading.v1.bond_legs.delete_many',
    list_bond_leg_versions_request: 'trading.v1.bond_legs_versions.list',
    get_bond_leg_version_request: 'trading.v1.bond_legs_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_legs_request: true,
    get_bond_leg_request: true,
    get_many_bond_legs_request: true,
    put_bond_leg_request: true,
    put_many_bond_legs_request: true,
    delete_bond_leg_request: true,
    delete_many_bond_legs_request: true,
    list_bond_leg_versions_request: true,
    get_bond_leg_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_legs_events.created',
    updated: 'trading.v1.bond_legs_events.updated',
    deleted: 'trading.v1.bond_legs_events.deleted',
} as const;
