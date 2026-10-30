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
import type { Feed } from '../domain/feed.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FeedKey {
    name: string;
}

export interface FeedWrite {
    id: string;
    name: string;
    publisher_code: string;
    session_code: string;
    service: string;
    provider_key: string;
    is_default: boolean;
    description: string;
}

export interface FeedChange {
    write: FeedWrite;
    precondition: Precondition;
}

export interface FeedRemoval {
    key: FeedKey;
    precondition: Precondition;
}

export interface FeedLookup {
    key: FeedKey;
    feed: Feed | null;
}

export interface FeedsFilter {
    publisher_code: string | null;
    session_code: string | null;
    service: string | null;
    id_one_of: string[] | null;
    publisher_code_one_of: string[] | null;
    session_code_one_of: string[] | null;
    service_one_of: string[] | null;
    search: string | null;
}

export interface FeedEvent {
    event_id: string;
    key: FeedKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FeedVersionKey {
    feed: FeedKey;
    version: number;
}

export interface FeedVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFeedsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FeedsFilter | null;
    as_of: string | null;
}

export interface ListFeedsResponse {
    result: Result;
    feeds: Feed[];
    total: number;
}

export interface GetFeedRequest {
    key: FeedKey;
}

export interface GetFeedResponse {
    result: Result;
    feed: Feed | null;
}

export interface GetManyFeedsRequest {
    keys: FeedKey[];
}

export interface GetManyFeedsResponse {
    result: Result;
    entries: FeedLookup[];
}

export interface PutFeedRequest {
    change: FeedChange;
    intent: ChangeIntent;
}

export interface PutFeedResponse {
    result: Result;
    feed: Feed | null;
}

export interface PutManyFeedsRequest {
    changes: FeedChange[];
    intent: ChangeIntent;
}

export interface PutManyFeedsResponse {
    result: Result;
    feeds: Feed[];
}

export interface DeleteFeedRequest {
    removal: FeedRemoval;
    intent: ChangeIntent;
}

export interface DeleteFeedResponse {
    result: Result;
}

export interface DeleteManyFeedsRequest {
    removals: FeedRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFeedsResponse {
    result: Result;
}

export interface ListFeedVersionsRequest {
    key: FeedKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FeedVersionsFilter | null;
}

export interface ListFeedVersionsResponse {
    result: Result;
    versions: Feed[];
    total: number;
}

export interface GetFeedVersionRequest {
    key: FeedVersionKey;
}

export interface GetFeedVersionResponse {
    result: Result;
    version: Feed | null;
}

export const subjects = {
    list_feeds_request: 'marketdata.v1.feeds.list',
    get_feed_request: 'marketdata.v1.feeds.get',
    get_many_feeds_request: 'marketdata.v1.feeds.get_many',
    put_feed_request: 'marketdata.v1.feeds.put',
    put_many_feeds_request: 'marketdata.v1.feeds.put_many',
    delete_feed_request: 'marketdata.v1.feeds.delete',
    delete_many_feeds_request: 'marketdata.v1.feeds.delete_many',
    list_feed_versions_request: 'marketdata.v1.feeds_versions.list',
    get_feed_version_request: 'marketdata.v1.feeds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_feeds_request: true,
    get_feed_request: true,
    get_many_feeds_request: true,
    put_feed_request: true,
    put_many_feeds_request: true,
    delete_feed_request: true,
    delete_many_feeds_request: true,
    list_feed_versions_request: true,
    get_feed_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.feeds_events.created',
    updated: 'marketdata.v1.feeds_events.updated',
    deleted: 'marketdata.v1.feeds_events.deleted',
} as const;
