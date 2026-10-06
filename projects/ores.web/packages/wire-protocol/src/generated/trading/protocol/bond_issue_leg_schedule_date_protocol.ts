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
import type { BondIssueLegScheduleDate } from '../domain/bond_issue_leg_schedule_date.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondIssueLegScheduleDateKey {
    issue_id: string;
    leg_number: number;
    schedule_role: string;
    schedule_sequence_number: number;
    sequence_number: number;
}

export interface BondIssueLegScheduleDateWrite {
    issue_id: string;
    leg_number: number;
    schedule_role: string;
    schedule_sequence_number: number;
    sequence_number: number;
    schedule_date: string;
}

export interface BondIssueLegScheduleDateChange {
    write: BondIssueLegScheduleDateWrite;
    precondition: Precondition;
}

export interface BondIssueLegScheduleDateRemoval {
    key: BondIssueLegScheduleDateKey;
    precondition: Precondition;
}

export interface BondIssueLegScheduleDateLookup {
    key: BondIssueLegScheduleDateKey;
    bond_issue_leg_schedule_date: BondIssueLegScheduleDate | null;
}

export interface BondIssueLegScheduleDateEvent {
    event_id: string;
    key: BondIssueLegScheduleDateKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondIssueLegScheduleDateVersionKey {
    bond_issue_leg_schedule_date: BondIssueLegScheduleDateKey;
    version: number;
}

export interface BondIssueLegScheduleDateVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondIssueLegScheduleDatesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListBondIssueLegScheduleDatesResponse {
    result: Result;
    bond_issue_leg_schedule_dates: BondIssueLegScheduleDate[];
    total: number;
}

export interface GetBondIssueLegScheduleDateRequest {
    key: BondIssueLegScheduleDateKey;
}

export interface GetBondIssueLegScheduleDateResponse {
    result: Result;
    bond_issue_leg_schedule_date: BondIssueLegScheduleDate | null;
}

export interface GetManyBondIssueLegScheduleDatesRequest {
    keys: BondIssueLegScheduleDateKey[];
}

export interface GetManyBondIssueLegScheduleDatesResponse {
    result: Result;
    entries: BondIssueLegScheduleDateLookup[];
}

export interface PutBondIssueLegScheduleDateRequest {
    change: BondIssueLegScheduleDateChange;
    intent: ChangeIntent;
}

export interface PutBondIssueLegScheduleDateResponse {
    result: Result;
    bond_issue_leg_schedule_date: BondIssueLegScheduleDate | null;
}

export interface PutManyBondIssueLegScheduleDatesRequest {
    changes: BondIssueLegScheduleDateChange[];
    intent: ChangeIntent;
}

export interface PutManyBondIssueLegScheduleDatesResponse {
    result: Result;
    bond_issue_leg_schedule_dates: BondIssueLegScheduleDate[];
}

export interface DeleteBondIssueLegScheduleDateRequest {
    removal: BondIssueLegScheduleDateRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondIssueLegScheduleDateResponse {
    result: Result;
}

export interface DeleteManyBondIssueLegScheduleDatesRequest {
    removals: BondIssueLegScheduleDateRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondIssueLegScheduleDatesResponse {
    result: Result;
}

export interface ListBondIssueLegScheduleDateVersionsRequest {
    key: BondIssueLegScheduleDateKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondIssueLegScheduleDateVersionsFilter | null;
}

export interface ListBondIssueLegScheduleDateVersionsResponse {
    result: Result;
    versions: BondIssueLegScheduleDate[];
    total: number;
}

export interface GetBondIssueLegScheduleDateVersionRequest {
    key: BondIssueLegScheduleDateVersionKey;
}

export interface GetBondIssueLegScheduleDateVersionResponse {
    result: Result;
    version: BondIssueLegScheduleDate | null;
}

export const subjects = {
    list_bond_issue_leg_schedule_dates_request: 'trading.v1.bond_issue_leg_schedule_dates.list',
    get_bond_issue_leg_schedule_date_request: 'trading.v1.bond_issue_leg_schedule_dates.get',
    get_many_bond_issue_leg_schedule_dates_request:
        'trading.v1.bond_issue_leg_schedule_dates.get_many',
    put_bond_issue_leg_schedule_date_request: 'trading.v1.bond_issue_leg_schedule_dates.put',
    put_many_bond_issue_leg_schedule_dates_request:
        'trading.v1.bond_issue_leg_schedule_dates.put_many',
    delete_bond_issue_leg_schedule_date_request: 'trading.v1.bond_issue_leg_schedule_dates.delete',
    delete_many_bond_issue_leg_schedule_dates_request:
        'trading.v1.bond_issue_leg_schedule_dates.delete_many',
    list_bond_issue_leg_schedule_date_versions_request:
        'trading.v1.bond_issue_leg_schedule_dates_versions.list',
    get_bond_issue_leg_schedule_date_version_request:
        'trading.v1.bond_issue_leg_schedule_dates_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_issue_leg_schedule_dates_request: true,
    get_bond_issue_leg_schedule_date_request: true,
    get_many_bond_issue_leg_schedule_dates_request: true,
    put_bond_issue_leg_schedule_date_request: true,
    put_many_bond_issue_leg_schedule_dates_request: true,
    delete_bond_issue_leg_schedule_date_request: true,
    delete_many_bond_issue_leg_schedule_dates_request: true,
    list_bond_issue_leg_schedule_date_versions_request: true,
    get_bond_issue_leg_schedule_date_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_issue_leg_schedule_dates_events.created',
    updated: 'trading.v1.bond_issue_leg_schedule_dates_events.updated',
    deleted: 'trading.v1.bond_issue_leg_schedule_dates_events.deleted',
} as const;
