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
import type { StressTestShift } from '../domain/stress_test_shift.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StressTestShiftKey {
    id: string;
}

export interface StressTestShiftWrite {
    id: string;
    stress_test_scenario_id: string;
    family: string;
    object_key: string;
    shift_type: string | null;
    shifts: string | null;
    shift_tenors: string | null;
    shift_expiries: string | null;
    extras: string | null;
    position: number;
}

export interface StressTestShiftChange {
    write: StressTestShiftWrite;
    precondition: Precondition;
}

export interface StressTestShiftRemoval {
    key: StressTestShiftKey;
    precondition: Precondition;
}

export interface StressTestShiftLookup {
    key: StressTestShiftKey;
    stress_test_shift: StressTestShift | null;
}

export interface StressTestShiftsFilter {
    id_one_of: string[] | null;
}

export interface StressTestShiftEvent {
    event_id: string;
    key: StressTestShiftKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StressTestShiftVersionKey {
    stress_test_shift: StressTestShiftKey;
    version: number;
}

export interface StressTestShiftVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStressTestShiftsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StressTestShiftsFilter | null;
}

export interface ListStressTestShiftsResponse {
    result: Result;
    stress_test_shifts: StressTestShift[];
    total: number;
}

export interface GetStressTestShiftRequest {
    key: StressTestShiftKey;
}

export interface GetStressTestShiftResponse {
    result: Result;
    stress_test_shift: StressTestShift | null;
}

export interface GetManyStressTestShiftsRequest {
    keys: StressTestShiftKey[];
}

export interface GetManyStressTestShiftsResponse {
    result: Result;
    entries: StressTestShiftLookup[];
}

export interface PutStressTestShiftRequest {
    change: StressTestShiftChange;
    intent: ChangeIntent;
}

export interface PutStressTestShiftResponse {
    result: Result;
    stress_test_shift: StressTestShift | null;
}

export interface PutManyStressTestShiftsRequest {
    changes: StressTestShiftChange[];
    intent: ChangeIntent;
}

export interface PutManyStressTestShiftsResponse {
    result: Result;
    stress_test_shifts: StressTestShift[];
}

export interface DeleteStressTestShiftRequest {
    removal: StressTestShiftRemoval;
    intent: ChangeIntent;
}

export interface DeleteStressTestShiftResponse {
    result: Result;
}

export interface DeleteManyStressTestShiftsRequest {
    removals: StressTestShiftRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStressTestShiftsResponse {
    result: Result;
}

export interface ListStressTestShiftVersionsRequest {
    key: StressTestShiftKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StressTestShiftVersionsFilter | null;
}

export interface ListStressTestShiftVersionsResponse {
    result: Result;
    versions: StressTestShift[];
    total: number;
}

export interface GetStressTestShiftVersionRequest {
    key: StressTestShiftVersionKey;
}

export interface GetStressTestShiftVersionResponse {
    result: Result;
    version: StressTestShift | null;
}

export const subjects = {
    list_stress_test_shifts_request: 'analytics.v1.stress_test_shifts.list',
    get_stress_test_shift_request: 'analytics.v1.stress_test_shifts.get',
    get_many_stress_test_shifts_request: 'analytics.v1.stress_test_shifts.get_many',
    put_stress_test_shift_request: 'analytics.v1.stress_test_shifts.put',
    put_many_stress_test_shifts_request: 'analytics.v1.stress_test_shifts.put_many',
    delete_stress_test_shift_request: 'analytics.v1.stress_test_shifts.delete',
    delete_many_stress_test_shifts_request: 'analytics.v1.stress_test_shifts.delete_many',
    list_stress_test_shift_versions_request: 'analytics.v1.stress_test_shifts_versions.list',
    get_stress_test_shift_version_request: 'analytics.v1.stress_test_shifts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_stress_test_shifts_request: true,
    get_stress_test_shift_request: true,
    get_many_stress_test_shifts_request: true,
    put_stress_test_shift_request: true,
    put_many_stress_test_shifts_request: true,
    delete_stress_test_shift_request: true,
    delete_many_stress_test_shifts_request: true,
    list_stress_test_shift_versions_request: true,
    get_stress_test_shift_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.stress_test_shifts_events.created',
    updated: 'analytics.v1.stress_test_shifts_events.updated',
    deleted: 'analytics.v1.stress_test_shifts_events.deleted',
} as const;
