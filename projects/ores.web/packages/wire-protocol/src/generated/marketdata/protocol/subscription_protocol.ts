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
import type { Subscription } from '../domain/subscription.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SubscriptionKey {
    provider_code: string;
}

export interface SubscriptionWrite {
    id: string;
    party_id: string;
    provider_code: string;
    display: boolean;
    non_display: boolean;
    derived: boolean;
}

export interface SubscriptionChange {
    write: SubscriptionWrite;
    precondition: Precondition;
}

export interface SubscriptionRemoval {
    key: SubscriptionKey;
    precondition: Precondition;
}

export interface SubscriptionLookup {
    key: SubscriptionKey;
    subscription: Subscription | null;
}

export interface SubscriptionsFilter {
    party_id: string | null;
    provider_code: string | null;
    id_one_of: string[] | null;
    party_id_one_of: string[] | null;
    provider_code_one_of: string[] | null;
}

export interface SubscriptionEvent {
    event_id: string;
    key: SubscriptionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SubscriptionVersionKey {
    subscription: SubscriptionKey;
    version: number;
}

export interface SubscriptionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSubscriptionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SubscriptionsFilter | null;
    as_of: string | null;
}

export interface ListSubscriptionsResponse {
    result: Result;
    subscriptions: Subscription[];
    total: number;
}

export interface GetSubscriptionRequest {
    key: SubscriptionKey;
}

export interface GetSubscriptionResponse {
    result: Result;
    subscription: Subscription | null;
}

export interface GetManySubscriptionsRequest {
    keys: SubscriptionKey[];
}

export interface GetManySubscriptionsResponse {
    result: Result;
    entries: SubscriptionLookup[];
}

export interface PutSubscriptionRequest {
    change: SubscriptionChange;
    intent: ChangeIntent;
}

export interface PutSubscriptionResponse {
    result: Result;
    subscription: Subscription | null;
}

export interface PutManySubscriptionsRequest {
    changes: SubscriptionChange[];
    intent: ChangeIntent;
}

export interface PutManySubscriptionsResponse {
    result: Result;
    subscriptions: Subscription[];
}

export interface DeleteSubscriptionRequest {
    removal: SubscriptionRemoval;
    intent: ChangeIntent;
}

export interface DeleteSubscriptionResponse {
    result: Result;
}

export interface DeleteManySubscriptionsRequest {
    removals: SubscriptionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySubscriptionsResponse {
    result: Result;
}

export interface ListSubscriptionVersionsRequest {
    key: SubscriptionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SubscriptionVersionsFilter | null;
}

export interface ListSubscriptionVersionsResponse {
    result: Result;
    versions: Subscription[];
    total: number;
}

export interface GetSubscriptionVersionRequest {
    key: SubscriptionVersionKey;
}

export interface GetSubscriptionVersionResponse {
    result: Result;
    version: Subscription | null;
}

export const subjects = {
    list_subscriptions_request: 'marketdata.v1.subscriptions.list',
    get_subscription_request: 'marketdata.v1.subscriptions.get',
    get_many_subscriptions_request: 'marketdata.v1.subscriptions.get_many',
    put_subscription_request: 'marketdata.v1.subscriptions.put',
    put_many_subscriptions_request: 'marketdata.v1.subscriptions.put_many',
    delete_subscription_request: 'marketdata.v1.subscriptions.delete',
    delete_many_subscriptions_request: 'marketdata.v1.subscriptions.delete_many',
    list_subscription_versions_request: 'marketdata.v1.subscriptions_versions.list',
    get_subscription_version_request: 'marketdata.v1.subscriptions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_subscriptions_request: true,
    get_subscription_request: true,
    get_many_subscriptions_request: true,
    put_subscription_request: true,
    put_many_subscriptions_request: true,
    delete_subscription_request: true,
    delete_many_subscriptions_request: true,
    list_subscription_versions_request: true,
    get_subscription_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.subscriptions_events.created',
    updated: 'marketdata.v1.subscriptions_events.updated',
    deleted: 'marketdata.v1.subscriptions_events.deleted',
} as const;
