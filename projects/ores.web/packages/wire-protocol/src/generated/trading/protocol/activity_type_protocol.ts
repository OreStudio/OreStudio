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
import type { ActivityType } from '../domain/activity_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ActivityTypeKey {
    code: string;
}

export interface ActivityTypeWrite {
    code: string;
    category: string;
    requires_confirmation: boolean;
    description: string;
    fpml_event_type_code: string;
    fsm_transition_id: string | null;
}

export interface ActivityTypeChange {
    write: ActivityTypeWrite;
    precondition: Precondition;
}

export interface ActivityTypeRemoval {
    key: ActivityTypeKey;
    precondition: Precondition;
}

export interface ActivityTypeLookup {
    key: ActivityTypeKey;
    activity_type: ActivityType | null;
}

export interface ActivityTypesFilter {
    code_one_of: string[] | null;
}

export interface ActivityTypeEvent {
    event_id: string;
    key: ActivityTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ActivityTypeVersionKey {
    activity_type: ActivityTypeKey;
    version: number;
}

export interface ActivityTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListActivityTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ActivityTypesFilter | null;
}

export interface ListActivityTypesResponse {
    result: Result;
    activity_types: ActivityType[];
    total: number;
}

export interface GetActivityTypeRequest {
    key: ActivityTypeKey;
}

export interface GetActivityTypeResponse {
    result: Result;
    activity_type: ActivityType | null;
}

export interface GetManyActivityTypesRequest {
    keys: ActivityTypeKey[];
}

export interface GetManyActivityTypesResponse {
    result: Result;
    entries: ActivityTypeLookup[];
}

export interface PutActivityTypeRequest {
    change: ActivityTypeChange;
    intent: ChangeIntent;
}

export interface PutActivityTypeResponse {
    result: Result;
    activity_type: ActivityType | null;
}

export interface PutManyActivityTypesRequest {
    changes: ActivityTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyActivityTypesResponse {
    result: Result;
    activity_types: ActivityType[];
}

export interface DeleteActivityTypeRequest {
    removal: ActivityTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteActivityTypeResponse {
    result: Result;
}

export interface DeleteManyActivityTypesRequest {
    removals: ActivityTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyActivityTypesResponse {
    result: Result;
}

export interface ListActivityTypeVersionsRequest {
    key: ActivityTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ActivityTypeVersionsFilter | null;
}

export interface ListActivityTypeVersionsResponse {
    result: Result;
    versions: ActivityType[];
    total: number;
}

export interface GetActivityTypeVersionRequest {
    key: ActivityTypeVersionKey;
}

export interface GetActivityTypeVersionResponse {
    result: Result;
    version: ActivityType | null;
}

export const subjects = {
    list_activity_types_request: 'trading.v1.activity_types.list',
    get_activity_type_request: 'trading.v1.activity_types.get',
    get_many_activity_types_request: 'trading.v1.activity_types.get_many',
    put_activity_type_request: 'trading.v1.activity_types.put',
    put_many_activity_types_request: 'trading.v1.activity_types.put_many',
    delete_activity_type_request: 'trading.v1.activity_types.delete',
    delete_many_activity_types_request: 'trading.v1.activity_types.delete_many',
    list_activity_type_versions_request: 'trading.v1.activity_types_versions.list',
    get_activity_type_version_request: 'trading.v1.activity_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_activity_types_request: true,
    get_activity_type_request: true,
    get_many_activity_types_request: true,
    put_activity_type_request: true,
    put_many_activity_types_request: true,
    delete_activity_type_request: true,
    delete_many_activity_types_request: true,
    list_activity_type_versions_request: true,
    get_activity_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.activity_types_events.created',
    updated: 'trading.v1.activity_types_events.updated',
    deleted: 'trading.v1.activity_types_events.deleted',
} as const;
