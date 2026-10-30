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
import type { SubscriptionInterval } from '../domain/subscription_interval.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SubscriptionIntervalKey {
    provider_code: string;
}

export interface SubscriptionIntervalWrite {
    id: string;
    party_id: string;
    provider_code: string;
    interval_from: string;
    feed_id: string;
    display: boolean;
    non_display: boolean;
    derived: boolean;
    interval_to: string | null;
}

export interface SubscriptionIntervalChange {
    write: SubscriptionIntervalWrite;
    precondition: Precondition;
}

export interface SubscriptionIntervalRemoval {
    key: SubscriptionIntervalKey;
    precondition: Precondition;
}

export interface SubscriptionIntervalLookup {
    key: SubscriptionIntervalKey;
    subscription_interval: SubscriptionInterval | null;
}

export interface SubscriptionIntervalsFilter {
    party_id: string | null;
    provider_code: string | null;
    feed_id: string | null;
    id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    provider_code_one_of: string[] | null;
    feed_id_one_of: string[] | null;
}

export interface SubscriptionIntervalEvent {
    event_id: string;
    key: SubscriptionIntervalKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SubscriptionIntervalVersionKey {
    subscription_interval: SubscriptionIntervalKey;
    version: number;
}

export interface SubscriptionIntervalVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSubscriptionIntervalsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SubscriptionIntervalsFilter | null;
    as_of: string | null;
}

export interface ListSubscriptionIntervalsResponse {
    result: Result;
    subscription_intervals: SubscriptionInterval[];
    total: number;
}

export interface GetSubscriptionIntervalRequest {
    key: SubscriptionIntervalKey;
}

export interface GetSubscriptionIntervalResponse {
    result: Result;
    subscription_interval: SubscriptionInterval | null;
}

export interface GetManySubscriptionIntervalsRequest {
    keys: SubscriptionIntervalKey[];
}

export interface GetManySubscriptionIntervalsResponse {
    result: Result;
    entries: SubscriptionIntervalLookup[];
}

export interface PutSubscriptionIntervalRequest {
    change: SubscriptionIntervalChange;
    intent: ChangeIntent;
}

export interface PutSubscriptionIntervalResponse {
    result: Result;
    subscription_interval: SubscriptionInterval | null;
}

export interface PutManySubscriptionIntervalsRequest {
    changes: SubscriptionIntervalChange[];
    intent: ChangeIntent;
}

export interface PutManySubscriptionIntervalsResponse {
    result: Result;
    subscription_intervals: SubscriptionInterval[];
}

export interface DeleteSubscriptionIntervalRequest {
    removal: SubscriptionIntervalRemoval;
    intent: ChangeIntent;
}

export interface DeleteSubscriptionIntervalResponse {
    result: Result;
}

export interface DeleteManySubscriptionIntervalsRequest {
    removals: SubscriptionIntervalRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySubscriptionIntervalsResponse {
    result: Result;
}

export interface ListSubscriptionIntervalVersionsRequest {
    key: SubscriptionIntervalKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SubscriptionIntervalVersionsFilter | null;
}

export interface ListSubscriptionIntervalVersionsResponse {
    result: Result;
    versions: SubscriptionInterval[];
    total: number;
}

export interface GetSubscriptionIntervalVersionRequest {
    key: SubscriptionIntervalVersionKey;
}

export interface GetSubscriptionIntervalVersionResponse {
    result: Result;
    version: SubscriptionInterval | null;
}

export const subjects = {
    list_subscription_intervals_request: 'marketdata.v1.subscription_intervals.list',
    get_subscription_interval_request: 'marketdata.v1.subscription_intervals.get',
    get_many_subscription_intervals_request: 'marketdata.v1.subscription_intervals.get_many',
    put_subscription_interval_request: 'marketdata.v1.subscription_intervals.put',
    put_many_subscription_intervals_request: 'marketdata.v1.subscription_intervals.put_many',
    delete_subscription_interval_request: 'marketdata.v1.subscription_intervals.delete',
    delete_many_subscription_intervals_request: 'marketdata.v1.subscription_intervals.delete_many',
    list_subscription_interval_versions_request:
        'marketdata.v1.subscription_intervals_versions.list',
    get_subscription_interval_version_request: 'marketdata.v1.subscription_intervals_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_subscription_intervals_request: true,
    get_subscription_interval_request: true,
    get_many_subscription_intervals_request: true,
    put_subscription_interval_request: true,
    put_many_subscription_intervals_request: true,
    delete_subscription_interval_request: true,
    delete_many_subscription_intervals_request: true,
    list_subscription_interval_versions_request: true,
    get_subscription_interval_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.subscription_intervals_events.created',
    updated: 'marketdata.v1.subscription_intervals_events.updated',
    deleted: 'marketdata.v1.subscription_intervals_events.deleted',
} as const;
