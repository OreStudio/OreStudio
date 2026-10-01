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
import type { LifecycleEvent } from '../domain/lifecycle_event.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LifecycleEventKey {
    code: string;
}

export interface LifecycleEventWrite {
    code: string;
    description: string;
    fsm_state_id: string | null;
}

export interface LifecycleEventChange {
    write: LifecycleEventWrite;
    precondition: Precondition;
}

export interface LifecycleEventRemoval {
    key: LifecycleEventKey;
    precondition: Precondition;
}

export interface LifecycleEventLookup {
    key: LifecycleEventKey;
    lifecycle_event: LifecycleEvent | null;
}

export interface LifecycleEventEvent {
    event_id: string;
    key: LifecycleEventKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface LifecycleEventVersionKey {
    lifecycle_event: LifecycleEventKey;
    version: number;
}

export interface LifecycleEventVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListLifecycleEventsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListLifecycleEventsResponse {
    result: Result;
    events: LifecycleEvent[];
    total: number;
}

export interface GetLifecycleEventRequest {
    key: LifecycleEventKey;
}

export interface GetLifecycleEventResponse {
    result: Result;
    lifecycle_event: LifecycleEvent | null;
}

export interface GetManyLifecycleEventsRequest {
    keys: LifecycleEventKey[];
}

export interface GetManyLifecycleEventsResponse {
    result: Result;
    entries: LifecycleEventLookup[];
}

export interface PutLifecycleEventRequest {
    change: LifecycleEventChange;
    intent: ChangeIntent;
}

export interface PutLifecycleEventResponse {
    result: Result;
    lifecycle_event: LifecycleEvent | null;
}

export interface PutManyLifecycleEventsRequest {
    changes: LifecycleEventChange[];
    intent: ChangeIntent;
}

export interface PutManyLifecycleEventsResponse {
    result: Result;
    events: LifecycleEvent[];
}

export interface DeleteLifecycleEventRequest {
    removal: LifecycleEventRemoval;
    intent: ChangeIntent;
}

export interface DeleteLifecycleEventResponse {
    result: Result;
}

export interface DeleteManyLifecycleEventsRequest {
    removals: LifecycleEventRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLifecycleEventsResponse {
    result: Result;
}

export interface ListLifecycleEventVersionsRequest {
    key: LifecycleEventKey;
    offset: number;
    limit: number;
    order: Order;
    filter: LifecycleEventVersionsFilter | null;
}

export interface ListLifecycleEventVersionsResponse {
    result: Result;
    versions: LifecycleEvent[];
    total: number;
}

export interface GetLifecycleEventVersionRequest {
    key: LifecycleEventVersionKey;
}

export interface GetLifecycleEventVersionResponse {
    result: Result;
    version: LifecycleEvent | null;
}

export const subjects = {
    list_lifecycle_events_request: 'trading.v1.lifecycle_events.list',
    get_lifecycle_event_request: 'trading.v1.lifecycle_events.get',
    get_many_lifecycle_events_request: 'trading.v1.lifecycle_events.get_many',
    put_lifecycle_event_request: 'trading.v1.lifecycle_events.put',
    put_many_lifecycle_events_request: 'trading.v1.lifecycle_events.put_many',
    delete_lifecycle_event_request: 'trading.v1.lifecycle_events.delete',
    delete_many_lifecycle_events_request: 'trading.v1.lifecycle_events.delete_many',
    list_lifecycle_event_versions_request: 'trading.v1.lifecycle_events_versions.list',
    get_lifecycle_event_version_request: 'trading.v1.lifecycle_events_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_lifecycle_events_request: true,
    get_lifecycle_event_request: true,
    get_many_lifecycle_events_request: true,
    put_lifecycle_event_request: true,
    put_many_lifecycle_events_request: true,
    delete_lifecycle_event_request: true,
    delete_many_lifecycle_events_request: true,
    list_lifecycle_event_versions_request: true,
    get_lifecycle_event_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.lifecycle_events_events.created',
    updated: 'trading.v1.lifecycle_events_events.updated',
    deleted: 'trading.v1.lifecycle_events_events.deleted',
} as const;
