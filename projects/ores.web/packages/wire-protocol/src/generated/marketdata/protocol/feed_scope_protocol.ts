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
import type { FeedScope } from '../domain/feed_scope.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FeedScopeKey {
    item: string;
}

export interface FeedScopeWrite {
    id: string;
    feed_id: string;
    asset_class: string;
    item: string;
    vendor_instrument_id: string;
}

export interface FeedScopeChange {
    write: FeedScopeWrite;
    precondition: Precondition;
}

export interface FeedScopeRemoval {
    key: FeedScopeKey;
    precondition: Precondition;
}

export interface FeedScopeLookup {
    key: FeedScopeKey;
    feed_scope: FeedScope | null;
}

export interface FeedScopesFilter {
    feed_id: string | null;
    asset_class: string | null;
    item: string | null;
    vendor_instrument_id: string | null;
    id_one_of: string[] | null;
    feed_id_one_of: string[] | null;
    asset_class_one_of: string[] | null;
    item_one_of: string[] | null;
    vendor_instrument_id_one_of: string[] | null;
    search: string | null;
}

export interface FeedScopeEvent {
    event_id: string;
    key: FeedScopeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FeedScopeVersionKey {
    feed_scope: FeedScopeKey;
    version: number;
}

export interface FeedScopeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFeedScopesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FeedScopesFilter | null;
    as_of: string | null;
}

export interface ListFeedScopesResponse {
    result: Result;
    scopes: FeedScope[];
    total: number;
}

export interface GetFeedScopeRequest {
    key: FeedScopeKey;
}

export interface GetFeedScopeResponse {
    result: Result;
    feed_scope: FeedScope | null;
}

export interface GetManyFeedScopesRequest {
    keys: FeedScopeKey[];
}

export interface GetManyFeedScopesResponse {
    result: Result;
    entries: FeedScopeLookup[];
}

export interface PutFeedScopeRequest {
    change: FeedScopeChange;
    intent: ChangeIntent;
}

export interface PutFeedScopeResponse {
    result: Result;
    feed_scope: FeedScope | null;
}

export interface PutManyFeedScopesRequest {
    changes: FeedScopeChange[];
    intent: ChangeIntent;
}

export interface PutManyFeedScopesResponse {
    result: Result;
    scopes: FeedScope[];
}

export interface DeleteFeedScopeRequest {
    removal: FeedScopeRemoval;
    intent: ChangeIntent;
}

export interface DeleteFeedScopeResponse {
    result: Result;
}

export interface DeleteManyFeedScopesRequest {
    removals: FeedScopeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFeedScopesResponse {
    result: Result;
}

export interface ListFeedScopeVersionsRequest {
    key: FeedScopeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FeedScopeVersionsFilter | null;
}

export interface ListFeedScopeVersionsResponse {
    result: Result;
    versions: FeedScope[];
    total: number;
}

export interface GetFeedScopeVersionRequest {
    key: FeedScopeVersionKey;
}

export interface GetFeedScopeVersionResponse {
    result: Result;
    version: FeedScope | null;
}

export const subjects = {
    list_feed_scopes_request: 'marketdata.v1.feed_scopes.list',
    get_feed_scope_request: 'marketdata.v1.feed_scopes.get',
    get_many_feed_scopes_request: 'marketdata.v1.feed_scopes.get_many',
    put_feed_scope_request: 'marketdata.v1.feed_scopes.put',
    put_many_feed_scopes_request: 'marketdata.v1.feed_scopes.put_many',
    delete_feed_scope_request: 'marketdata.v1.feed_scopes.delete',
    delete_many_feed_scopes_request: 'marketdata.v1.feed_scopes.delete_many',
    list_feed_scope_versions_request: 'marketdata.v1.feed_scopes_versions.list',
    get_feed_scope_version_request: 'marketdata.v1.feed_scopes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_feed_scopes_request: true,
    get_feed_scope_request: true,
    get_many_feed_scopes_request: true,
    put_feed_scope_request: true,
    put_many_feed_scopes_request: true,
    delete_feed_scope_request: true,
    delete_many_feed_scopes_request: true,
    list_feed_scope_versions_request: true,
    get_feed_scope_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.feed_scopes_events.created',
    updated: 'marketdata.v1.feed_scopes_events.updated',
    deleted: 'marketdata.v1.feed_scopes_events.deleted',
} as const;
