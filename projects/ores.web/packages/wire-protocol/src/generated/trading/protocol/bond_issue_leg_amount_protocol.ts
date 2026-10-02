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
import type { BondIssueLegAmount } from '../domain/bond_issue_leg_amount.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondIssueLegAmountKey {
    issue_id: string;
    leg_number: number;
    amount_role: string;
    sequence_number: number;
}

export interface BondIssueLegAmountWrite {
    issue_id: string;
    leg_number: number;
    amount_role: string;
    sequence_number: number;
    value: string;
    start_date: string | null;
}

export interface BondIssueLegAmountChange {
    write: BondIssueLegAmountWrite;
    precondition: Precondition;
}

export interface BondIssueLegAmountRemoval {
    key: BondIssueLegAmountKey;
    precondition: Precondition;
}

export interface BondIssueLegAmountLookup {
    key: BondIssueLegAmountKey;
    bond_issue_leg_amount: BondIssueLegAmount | null;
}

export interface BondIssueLegAmountEvent {
    event_id: string;
    key: BondIssueLegAmountKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondIssueLegAmountVersionKey {
    bond_issue_leg_amount: BondIssueLegAmountKey;
    version: number;
}

export interface BondIssueLegAmountVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondIssueLegAmountsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondIssueLegAmountsResponse {
    result: Result;
    bond_issue_leg_amounts: BondIssueLegAmount[];
    total: number;
}

export interface GetBondIssueLegAmountRequest {
    key: BondIssueLegAmountKey;
}

export interface GetBondIssueLegAmountResponse {
    result: Result;
    bond_issue_leg_amount: BondIssueLegAmount | null;
}

export interface GetManyBondIssueLegAmountsRequest {
    keys: BondIssueLegAmountKey[];
}

export interface GetManyBondIssueLegAmountsResponse {
    result: Result;
    entries: BondIssueLegAmountLookup[];
}

export interface PutBondIssueLegAmountRequest {
    change: BondIssueLegAmountChange;
    intent: ChangeIntent;
}

export interface PutBondIssueLegAmountResponse {
    result: Result;
    bond_issue_leg_amount: BondIssueLegAmount | null;
}

export interface PutManyBondIssueLegAmountsRequest {
    changes: BondIssueLegAmountChange[];
    intent: ChangeIntent;
}

export interface PutManyBondIssueLegAmountsResponse {
    result: Result;
    bond_issue_leg_amounts: BondIssueLegAmount[];
}

export interface DeleteBondIssueLegAmountRequest {
    removal: BondIssueLegAmountRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondIssueLegAmountResponse {
    result: Result;
}

export interface DeleteManyBondIssueLegAmountsRequest {
    removals: BondIssueLegAmountRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondIssueLegAmountsResponse {
    result: Result;
}

export interface ListBondIssueLegAmountVersionsRequest {
    key: BondIssueLegAmountKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondIssueLegAmountVersionsFilter | null;
}

export interface ListBondIssueLegAmountVersionsResponse {
    result: Result;
    versions: BondIssueLegAmount[];
    total: number;
}

export interface GetBondIssueLegAmountVersionRequest {
    key: BondIssueLegAmountVersionKey;
}

export interface GetBondIssueLegAmountVersionResponse {
    result: Result;
    version: BondIssueLegAmount | null;
}

export const subjects = {
    list_bond_issue_leg_amounts_request: 'trading.v1.bond_issue_leg_amounts.list',
    get_bond_issue_leg_amount_request: 'trading.v1.bond_issue_leg_amounts.get',
    get_many_bond_issue_leg_amounts_request: 'trading.v1.bond_issue_leg_amounts.get_many',
    put_bond_issue_leg_amount_request: 'trading.v1.bond_issue_leg_amounts.put',
    put_many_bond_issue_leg_amounts_request: 'trading.v1.bond_issue_leg_amounts.put_many',
    delete_bond_issue_leg_amount_request: 'trading.v1.bond_issue_leg_amounts.delete',
    delete_many_bond_issue_leg_amounts_request: 'trading.v1.bond_issue_leg_amounts.delete_many',
    list_bond_issue_leg_amount_versions_request: 'trading.v1.bond_issue_leg_amounts_versions.list',
    get_bond_issue_leg_amount_version_request: 'trading.v1.bond_issue_leg_amounts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_issue_leg_amounts_request: true,
    get_bond_issue_leg_amount_request: true,
    get_many_bond_issue_leg_amounts_request: true,
    put_bond_issue_leg_amount_request: true,
    put_many_bond_issue_leg_amounts_request: true,
    delete_bond_issue_leg_amount_request: true,
    delete_many_bond_issue_leg_amounts_request: true,
    list_bond_issue_leg_amount_versions_request: true,
    get_bond_issue_leg_amount_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_issue_leg_amounts_events.created',
    updated: 'trading.v1.bond_issue_leg_amounts_events.updated',
    deleted: 'trading.v1.bond_issue_leg_amounts_events.deleted',
} as const;
