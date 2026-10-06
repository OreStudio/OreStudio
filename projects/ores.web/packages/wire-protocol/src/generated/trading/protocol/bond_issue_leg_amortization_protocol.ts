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
import type { BondIssueLegAmortization } from '../domain/bond_issue_leg_amortization.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondIssueLegAmortizationKey {
    issue_id: string;
    leg_number: number;
    sequence_number: number;
}

export interface BondIssueLegAmortizationWrite {
    issue_id: string;
    leg_number: number;
    sequence_number: number;
    amortization_type: string;
    value: string | null;
    start_date: string | null;
    end_date: string | null;
    frequency: string | null;
    underflow: boolean | null;
}

export interface BondIssueLegAmortizationChange {
    write: BondIssueLegAmortizationWrite;
    precondition: Precondition;
}

export interface BondIssueLegAmortizationRemoval {
    key: BondIssueLegAmortizationKey;
    precondition: Precondition;
}

export interface BondIssueLegAmortizationLookup {
    key: BondIssueLegAmortizationKey;
    bond_issue_leg_amortization: BondIssueLegAmortization | null;
}

export interface BondIssueLegAmortizationEvent {
    event_id: string;
    key: BondIssueLegAmortizationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondIssueLegAmortizationVersionKey {
    bond_issue_leg_amortization: BondIssueLegAmortizationKey;
    version: number;
}

export interface BondIssueLegAmortizationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondIssueLegAmortizationsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListBondIssueLegAmortizationsResponse {
    result: Result;
    bond_issue_leg_amortizations: BondIssueLegAmortization[];
    total: number;
}

export interface GetBondIssueLegAmortizationRequest {
    key: BondIssueLegAmortizationKey;
}

export interface GetBondIssueLegAmortizationResponse {
    result: Result;
    bond_issue_leg_amortization: BondIssueLegAmortization | null;
}

export interface GetManyBondIssueLegAmortizationsRequest {
    keys: BondIssueLegAmortizationKey[];
}

export interface GetManyBondIssueLegAmortizationsResponse {
    result: Result;
    entries: BondIssueLegAmortizationLookup[];
}

export interface PutBondIssueLegAmortizationRequest {
    change: BondIssueLegAmortizationChange;
    intent: ChangeIntent;
}

export interface PutBondIssueLegAmortizationResponse {
    result: Result;
    bond_issue_leg_amortization: BondIssueLegAmortization | null;
}

export interface PutManyBondIssueLegAmortizationsRequest {
    changes: BondIssueLegAmortizationChange[];
    intent: ChangeIntent;
}

export interface PutManyBondIssueLegAmortizationsResponse {
    result: Result;
    bond_issue_leg_amortizations: BondIssueLegAmortization[];
}

export interface DeleteBondIssueLegAmortizationRequest {
    removal: BondIssueLegAmortizationRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondIssueLegAmortizationResponse {
    result: Result;
}

export interface DeleteManyBondIssueLegAmortizationsRequest {
    removals: BondIssueLegAmortizationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondIssueLegAmortizationsResponse {
    result: Result;
}

export interface ListBondIssueLegAmortizationVersionsRequest {
    key: BondIssueLegAmortizationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondIssueLegAmortizationVersionsFilter | null;
}

export interface ListBondIssueLegAmortizationVersionsResponse {
    result: Result;
    versions: BondIssueLegAmortization[];
    total: number;
}

export interface GetBondIssueLegAmortizationVersionRequest {
    key: BondIssueLegAmortizationVersionKey;
}

export interface GetBondIssueLegAmortizationVersionResponse {
    result: Result;
    version: BondIssueLegAmortization | null;
}

export const subjects = {
    list_bond_issue_leg_amortizations_request: 'trading.v1.bond_issue_leg_amortizations.list',
    get_bond_issue_leg_amortization_request: 'trading.v1.bond_issue_leg_amortizations.get',
    get_many_bond_issue_leg_amortizations_request:
        'trading.v1.bond_issue_leg_amortizations.get_many',
    put_bond_issue_leg_amortization_request: 'trading.v1.bond_issue_leg_amortizations.put',
    put_many_bond_issue_leg_amortizations_request:
        'trading.v1.bond_issue_leg_amortizations.put_many',
    delete_bond_issue_leg_amortization_request: 'trading.v1.bond_issue_leg_amortizations.delete',
    delete_many_bond_issue_leg_amortizations_request:
        'trading.v1.bond_issue_leg_amortizations.delete_many',
    list_bond_issue_leg_amortization_versions_request:
        'trading.v1.bond_issue_leg_amortizations_versions.list',
    get_bond_issue_leg_amortization_version_request:
        'trading.v1.bond_issue_leg_amortizations_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_issue_leg_amortizations_request: true,
    get_bond_issue_leg_amortization_request: true,
    get_many_bond_issue_leg_amortizations_request: true,
    put_bond_issue_leg_amortization_request: true,
    put_many_bond_issue_leg_amortizations_request: true,
    delete_bond_issue_leg_amortization_request: true,
    delete_many_bond_issue_leg_amortizations_request: true,
    list_bond_issue_leg_amortization_versions_request: true,
    get_bond_issue_leg_amortization_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_issue_leg_amortizations_events.created',
    updated: 'trading.v1.bond_issue_leg_amortizations_events.updated',
    deleted: 'trading.v1.bond_issue_leg_amortizations_events.deleted',
} as const;
