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
import type { BondIssueLegSchedule } from '../domain/bond_issue_leg_schedule.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BondIssueLegScheduleKey {
    issue_id: string;
    leg_number: number;
    schedule_role: string;
    sequence_number: number;
}

export interface BondIssueLegScheduleWrite {
    issue_id: string;
    leg_number: number;
    schedule_role: string;
    sequence_number: number;
    schedule_kind: string;
    start_date: string | null;
    end_date: string | null;
    adjust_end_date_to_previous_month_end: string | null;
    tenor: string | null;
    calendar: string | null;
    convention: string | null;
    term_convention: string | null;
    rule: string | null;
    end_of_month: string | null;
    end_of_month_convention: string | null;
    first_date: string | null;
    last_date: string | null;
    remove_first_date: boolean | null;
    remove_last_date: boolean | null;
    include_duplicate_dates: string | null;
}

export interface BondIssueLegScheduleChange {
    write: BondIssueLegScheduleWrite;
    precondition: Precondition;
}

export interface BondIssueLegScheduleRemoval {
    key: BondIssueLegScheduleKey;
    precondition: Precondition;
}

export interface BondIssueLegScheduleLookup {
    key: BondIssueLegScheduleKey;
    bond_issue_leg_schedule: BondIssueLegSchedule | null;
}

export interface BondIssueLegScheduleEvent {
    event_id: string;
    key: BondIssueLegScheduleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BondIssueLegScheduleVersionKey {
    bond_issue_leg_schedule: BondIssueLegScheduleKey;
    version: number;
}

export interface BondIssueLegScheduleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBondIssueLegSchedulesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBondIssueLegSchedulesResponse {
    result: Result;
    bond_issue_leg_schedules: BondIssueLegSchedule[];
    total: number;
}

export interface GetBondIssueLegScheduleRequest {
    key: BondIssueLegScheduleKey;
}

export interface GetBondIssueLegScheduleResponse {
    result: Result;
    bond_issue_leg_schedule: BondIssueLegSchedule | null;
}

export interface GetManyBondIssueLegSchedulesRequest {
    keys: BondIssueLegScheduleKey[];
}

export interface GetManyBondIssueLegSchedulesResponse {
    result: Result;
    entries: BondIssueLegScheduleLookup[];
}

export interface PutBondIssueLegScheduleRequest {
    change: BondIssueLegScheduleChange;
    intent: ChangeIntent;
}

export interface PutBondIssueLegScheduleResponse {
    result: Result;
    bond_issue_leg_schedule: BondIssueLegSchedule | null;
}

export interface PutManyBondIssueLegSchedulesRequest {
    changes: BondIssueLegScheduleChange[];
    intent: ChangeIntent;
}

export interface PutManyBondIssueLegSchedulesResponse {
    result: Result;
    bond_issue_leg_schedules: BondIssueLegSchedule[];
}

export interface DeleteBondIssueLegScheduleRequest {
    removal: BondIssueLegScheduleRemoval;
    intent: ChangeIntent;
}

export interface DeleteBondIssueLegScheduleResponse {
    result: Result;
}

export interface DeleteManyBondIssueLegSchedulesRequest {
    removals: BondIssueLegScheduleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBondIssueLegSchedulesResponse {
    result: Result;
}

export interface ListBondIssueLegScheduleVersionsRequest {
    key: BondIssueLegScheduleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BondIssueLegScheduleVersionsFilter | null;
}

export interface ListBondIssueLegScheduleVersionsResponse {
    result: Result;
    versions: BondIssueLegSchedule[];
    total: number;
}

export interface GetBondIssueLegScheduleVersionRequest {
    key: BondIssueLegScheduleVersionKey;
}

export interface GetBondIssueLegScheduleVersionResponse {
    result: Result;
    version: BondIssueLegSchedule | null;
}

export const subjects = {
    list_bond_issue_leg_schedules_request: 'trading.v1.bond_issue_leg_schedules.list',
    get_bond_issue_leg_schedule_request: 'trading.v1.bond_issue_leg_schedules.get',
    get_many_bond_issue_leg_schedules_request: 'trading.v1.bond_issue_leg_schedules.get_many',
    put_bond_issue_leg_schedule_request: 'trading.v1.bond_issue_leg_schedules.put',
    put_many_bond_issue_leg_schedules_request: 'trading.v1.bond_issue_leg_schedules.put_many',
    delete_bond_issue_leg_schedule_request: 'trading.v1.bond_issue_leg_schedules.delete',
    delete_many_bond_issue_leg_schedules_request: 'trading.v1.bond_issue_leg_schedules.delete_many',
    list_bond_issue_leg_schedule_versions_request:
        'trading.v1.bond_issue_leg_schedules_versions.list',
    get_bond_issue_leg_schedule_version_request: 'trading.v1.bond_issue_leg_schedules_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_bond_issue_leg_schedules_request: true,
    get_bond_issue_leg_schedule_request: true,
    get_many_bond_issue_leg_schedules_request: true,
    put_bond_issue_leg_schedule_request: true,
    put_many_bond_issue_leg_schedules_request: true,
    delete_bond_issue_leg_schedule_request: true,
    delete_many_bond_issue_leg_schedules_request: true,
    list_bond_issue_leg_schedule_versions_request: true,
    get_bond_issue_leg_schedule_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.bond_issue_leg_schedules_events.created',
    updated: 'trading.v1.bond_issue_leg_schedules_events.updated',
    deleted: 'trading.v1.bond_issue_leg_schedules_events.deleted',
} as const;
