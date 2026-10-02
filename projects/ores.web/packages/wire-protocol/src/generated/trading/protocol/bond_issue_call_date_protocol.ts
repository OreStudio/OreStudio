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
import type { BondIssueCallDate } from '../domain/bond_issue_call_date.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondIssueCallDateKey {
    issue_id: string;
    sequence_number: number;
}

export interface BondIssueCallDateWrite {
    issue_id: string;
    sequence_number: number;
    call_date: string;
}

export interface BondIssueCallDateChange {
    write: BondIssueCallDateWrite;
    precondition: Precondition;
}

export interface BondIssueCallDateRemoval {
    key: BondIssueCallDateKey;
    precondition: Precondition;
}

export interface BondIssueCallDateLookup {
    key: BondIssueCallDateKey;
    bond_issue_call_date: BondIssueCallDate | null;
}

export interface BondIssueCallDateEvent {
    event_id: string;
    key: BondIssueCallDateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondIssueCallDateVersionKey {
    bond_issue_call_date: BondIssueCallDateKey;
    version: number;
}

export interface BondIssueCallDateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondIssueCallDatesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondIssueCallDatesResponse {
    result: Result;
    call_dates: BondIssueCallDate[];
    total: number;
}

export interface GetBondIssueCallDateRequest {
    key: BondIssueCallDateKey;
}

export interface GetBondIssueCallDateResponse {
    result: Result;
    bond_issue_call_date: BondIssueCallDate | null;
}

export interface GetManyBondIssueCallDatesRequest {
    keys: BondIssueCallDateKey[];
}

export interface GetManyBondIssueCallDatesResponse {
    result: Result;
    entries: BondIssueCallDateLookup[];
}

export interface PutBondIssueCallDateRequest {
    change: BondIssueCallDateChange;
    intent: ChangeIntent;
}

export interface PutBondIssueCallDateResponse {
    result: Result;
    bond_issue_call_date: BondIssueCallDate | null;
}

export interface PutManyBondIssueCallDatesRequest {
    changes: BondIssueCallDateChange[];
    intent: ChangeIntent;
}

export interface PutManyBondIssueCallDatesResponse {
    result: Result;
    call_dates: BondIssueCallDate[];
}

export interface DeleteBondIssueCallDateRequest {
    removal: BondIssueCallDateRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondIssueCallDateResponse {
    result: Result;
}

export interface DeleteManyBondIssueCallDatesRequest {
    removals: BondIssueCallDateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondIssueCallDatesResponse {
    result: Result;
}

export interface ListBondIssueCallDateVersionsRequest {
    key: BondIssueCallDateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondIssueCallDateVersionsFilter | null;
}

export interface ListBondIssueCallDateVersionsResponse {
    result: Result;
    versions: BondIssueCallDate[];
    total: number;
}

export interface GetBondIssueCallDateVersionRequest {
    key: BondIssueCallDateVersionKey;
}

export interface GetBondIssueCallDateVersionResponse {
    result: Result;
    version: BondIssueCallDate | null;
}

export const subjects = {
    list_bond_issue_call_dates_request: 'trading.v1.bond_issue_call_dates.list',
    get_bond_issue_call_date_request: 'trading.v1.bond_issue_call_dates.get',
    get_many_bond_issue_call_dates_request: 'trading.v1.bond_issue_call_dates.get_many',
    put_bond_issue_call_date_request: 'trading.v1.bond_issue_call_dates.put',
    put_many_bond_issue_call_dates_request: 'trading.v1.bond_issue_call_dates.put_many',
    delete_bond_issue_call_date_request: 'trading.v1.bond_issue_call_dates.delete',
    delete_many_bond_issue_call_dates_request: 'trading.v1.bond_issue_call_dates.delete_many',
    list_bond_issue_call_date_versions_request: 'trading.v1.bond_issue_call_dates_versions.list',
    get_bond_issue_call_date_version_request: 'trading.v1.bond_issue_call_dates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_issue_call_dates_request: true,
    get_bond_issue_call_date_request: true,
    get_many_bond_issue_call_dates_request: true,
    put_bond_issue_call_date_request: true,
    put_many_bond_issue_call_dates_request: true,
    delete_bond_issue_call_date_request: true,
    delete_many_bond_issue_call_dates_request: true,
    list_bond_issue_call_date_versions_request: true,
    get_bond_issue_call_date_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_issue_call_dates_events.created',
    updated: 'trading.v1.bond_issue_call_dates_events.updated',
    deleted: 'trading.v1.bond_issue_call_dates_events.deleted',
} as const;
