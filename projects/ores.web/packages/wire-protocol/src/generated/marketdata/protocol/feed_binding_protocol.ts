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
import type { FeedBinding } from '../domain/feed_binding.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FeedBindingKey {
    source_name: string;
}

export interface FeedBindingWrite {
    id: string;
    party_id: string;
    source_name: string;
    enabled: boolean;
}

export interface FeedBindingChange {
    write: FeedBindingWrite;
    precondition: Precondition;
}

export interface FeedBindingRemoval {
    key: FeedBindingKey;
    precondition: Precondition;
}

export interface FeedBindingLookup {
    key: FeedBindingKey;
    feed_binding: FeedBinding | null;
}

export interface FeedBindingsFilter {
    id_one_of: string[] | null;
}

export interface FeedBindingEvent {
    event_id: string;
    key: FeedBindingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FeedBindingVersionKey {
    feed_binding: FeedBindingKey;
    version: number;
}

export interface FeedBindingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFeedBindingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FeedBindingsFilter | null;
    as_of: string | null;
}

export interface ListFeedBindingsResponse {
    result: Result;
    feed_bindings: FeedBinding[];
    total: number;
}

export interface GetFeedBindingRequest {
    key: FeedBindingKey;
}

export interface GetFeedBindingResponse {
    result: Result;
    feed_binding: FeedBinding | null;
}

export interface GetManyFeedBindingsRequest {
    keys: FeedBindingKey[];
}

export interface GetManyFeedBindingsResponse {
    result: Result;
    entries: FeedBindingLookup[];
}

export interface PutFeedBindingRequest {
    change: FeedBindingChange;
    intent: ChangeIntent;
}

export interface PutFeedBindingResponse {
    result: Result;
    feed_binding: FeedBinding | null;
}

export interface PutManyFeedBindingsRequest {
    changes: FeedBindingChange[];
    intent: ChangeIntent;
}

export interface PutManyFeedBindingsResponse {
    result: Result;
    feed_bindings: FeedBinding[];
}

export interface DeleteFeedBindingRequest {
    removal: FeedBindingRemoval;
    intent: ChangeIntent;
}

export interface DeleteFeedBindingResponse {
    result: Result;
}

export interface DeleteManyFeedBindingsRequest {
    removals: FeedBindingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFeedBindingsResponse {
    result: Result;
}

export interface ListFeedBindingVersionsRequest {
    key: FeedBindingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FeedBindingVersionsFilter | null;
}

export interface ListFeedBindingVersionsResponse {
    result: Result;
    versions: FeedBinding[];
    total: number;
}

export interface GetFeedBindingVersionRequest {
    key: FeedBindingVersionKey;
}

export interface GetFeedBindingVersionResponse {
    result: Result;
    version: FeedBinding | null;
}

export const subjects = {
    list_feed_bindings_request: 'marketdata.v1.feed_bindings.list',
    get_feed_binding_request: 'marketdata.v1.feed_bindings.get',
    get_many_feed_bindings_request: 'marketdata.v1.feed_bindings.get_many',
    put_feed_binding_request: 'marketdata.v1.feed_bindings.put',
    put_many_feed_bindings_request: 'marketdata.v1.feed_bindings.put_many',
    delete_feed_binding_request: 'marketdata.v1.feed_bindings.delete',
    delete_many_feed_bindings_request: 'marketdata.v1.feed_bindings.delete_many',
    list_feed_binding_versions_request: 'marketdata.v1.feed_bindings_versions.list',
    get_feed_binding_version_request: 'marketdata.v1.feed_bindings_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_feed_bindings_request: true,
    get_feed_binding_request: true,
    get_many_feed_bindings_request: true,
    put_feed_binding_request: true,
    put_many_feed_bindings_request: true,
    delete_feed_binding_request: true,
    delete_many_feed_bindings_request: true,
    list_feed_binding_versions_request: true,
    get_feed_binding_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.feed_bindings_events.created',
    updated: 'marketdata.v1.feed_bindings_events.updated',
    deleted: 'marketdata.v1.feed_bindings_events.deleted',
} as const;
