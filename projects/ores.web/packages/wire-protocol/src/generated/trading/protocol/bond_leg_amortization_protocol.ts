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
import type { BondLegAmortization } from '../domain/bond_leg_amortization.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondLegAmortizationKey {
    trade_id: string;
    leg_role: string;
    leg_number: number;
    sequence_number: number;
}

export interface BondLegAmortizationWrite {
    trade_id: string;
    leg_role: string;
    leg_number: number;
    sequence_number: number;
    amortization_type: string;
    value: string | null;
    start_date: string | null;
    end_date: string | null;
    frequency: string | null;
    underflow: boolean | null;
}

export interface BondLegAmortizationChange {
    write: BondLegAmortizationWrite;
    precondition: Precondition;
}

export interface BondLegAmortizationRemoval {
    key: BondLegAmortizationKey;
    precondition: Precondition;
}

export interface BondLegAmortizationLookup {
    key: BondLegAmortizationKey;
    bond_leg_amortization: BondLegAmortization | null;
}

export interface BondLegAmortizationEvent {
    event_id: string;
    key: BondLegAmortizationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondLegAmortizationVersionKey {
    bond_leg_amortization: BondLegAmortizationKey;
    version: number;
}

export interface BondLegAmortizationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondLegAmortizationsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondLegAmortizationsResponse {
    result: Result;
    bond_leg_amortizations: BondLegAmortization[];
    total: number;
}

export interface GetBondLegAmortizationRequest {
    key: BondLegAmortizationKey;
}

export interface GetBondLegAmortizationResponse {
    result: Result;
    bond_leg_amortization: BondLegAmortization | null;
}

export interface GetManyBondLegAmortizationsRequest {
    keys: BondLegAmortizationKey[];
}

export interface GetManyBondLegAmortizationsResponse {
    result: Result;
    entries: BondLegAmortizationLookup[];
}

export interface PutBondLegAmortizationRequest {
    change: BondLegAmortizationChange;
    intent: ChangeIntent;
}

export interface PutBondLegAmortizationResponse {
    result: Result;
    bond_leg_amortization: BondLegAmortization | null;
}

export interface PutManyBondLegAmortizationsRequest {
    changes: BondLegAmortizationChange[];
    intent: ChangeIntent;
}

export interface PutManyBondLegAmortizationsResponse {
    result: Result;
    bond_leg_amortizations: BondLegAmortization[];
}

export interface DeleteBondLegAmortizationRequest {
    removal: BondLegAmortizationRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondLegAmortizationResponse {
    result: Result;
}

export interface DeleteManyBondLegAmortizationsRequest {
    removals: BondLegAmortizationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondLegAmortizationsResponse {
    result: Result;
}

export interface ListBondLegAmortizationVersionsRequest {
    key: BondLegAmortizationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondLegAmortizationVersionsFilter | null;
}

export interface ListBondLegAmortizationVersionsResponse {
    result: Result;
    versions: BondLegAmortization[];
    total: number;
}

export interface GetBondLegAmortizationVersionRequest {
    key: BondLegAmortizationVersionKey;
}

export interface GetBondLegAmortizationVersionResponse {
    result: Result;
    version: BondLegAmortization | null;
}

export const subjects = {
    list_bond_leg_amortizations_request: 'trading.v1.bond_leg_amortizations.list',
    get_bond_leg_amortization_request: 'trading.v1.bond_leg_amortizations.get',
    get_many_bond_leg_amortizations_request: 'trading.v1.bond_leg_amortizations.get_many',
    put_bond_leg_amortization_request: 'trading.v1.bond_leg_amortizations.put',
    put_many_bond_leg_amortizations_request: 'trading.v1.bond_leg_amortizations.put_many',
    delete_bond_leg_amortization_request: 'trading.v1.bond_leg_amortizations.delete',
    delete_many_bond_leg_amortizations_request: 'trading.v1.bond_leg_amortizations.delete_many',
    list_bond_leg_amortization_versions_request: 'trading.v1.bond_leg_amortizations_versions.list',
    get_bond_leg_amortization_version_request: 'trading.v1.bond_leg_amortizations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_leg_amortizations_request: true,
    get_bond_leg_amortization_request: true,
    get_many_bond_leg_amortizations_request: true,
    put_bond_leg_amortization_request: true,
    put_many_bond_leg_amortizations_request: true,
    delete_bond_leg_amortization_request: true,
    delete_many_bond_leg_amortizations_request: true,
    list_bond_leg_amortization_versions_request: true,
    get_bond_leg_amortization_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_leg_amortizations_events.created',
    updated: 'trading.v1.bond_leg_amortizations_events.updated',
    deleted: 'trading.v1.bond_leg_amortizations_events.deleted',
} as const;
