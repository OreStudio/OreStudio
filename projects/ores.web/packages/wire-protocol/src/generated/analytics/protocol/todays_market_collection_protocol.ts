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
import type { TodaysMarketCollection } from '../domain/todays_market_collection.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TodaysMarketCollectionKey {
    collection: string;
}

export interface TodaysMarketCollectionWrite {
    id: string;
    todays_market_config_id: string;
    collection: string;
    collection_id: string | null;
    position: number;
}

export interface TodaysMarketCollectionChange {
    write: TodaysMarketCollectionWrite;
    precondition: Precondition;
}

export interface TodaysMarketCollectionRemoval {
    key: TodaysMarketCollectionKey;
    precondition: Precondition;
}

export interface TodaysMarketCollectionLookup {
    key: TodaysMarketCollectionKey;
    todays_market_collection: TodaysMarketCollection | null;
}

export interface TodaysMarketCollectionsFilter {
    todays_market_config_id: string | null;
    id_one_of: string[] | null;
    todays_market_config_id_one_of: string[] | null;
}

export interface TodaysMarketCollectionEvent {
    event_id: string;
    key: TodaysMarketCollectionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TodaysMarketCollectionVersionKey {
    todays_market_collection: TodaysMarketCollectionKey;
    version: number;
}

export interface TodaysMarketCollectionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTodaysMarketCollectionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketCollectionsFilter | null;
    as_of: string | null;
}

export interface ListTodaysMarketCollectionsResponse {
    result: Result;
    collections: TodaysMarketCollection[];
    total: number;
}

export interface GetTodaysMarketCollectionRequest {
    key: TodaysMarketCollectionKey;
}

export interface GetTodaysMarketCollectionResponse {
    result: Result;
    todays_market_collection: TodaysMarketCollection | null;
}

export interface GetManyTodaysMarketCollectionsRequest {
    keys: TodaysMarketCollectionKey[];
}

export interface GetManyTodaysMarketCollectionsResponse {
    result: Result;
    entries: TodaysMarketCollectionLookup[];
}

export interface PutTodaysMarketCollectionRequest {
    change: TodaysMarketCollectionChange;
    intent: ChangeIntent;
}

export interface PutTodaysMarketCollectionResponse {
    result: Result;
    todays_market_collection: TodaysMarketCollection | null;
}

export interface PutManyTodaysMarketCollectionsRequest {
    changes: TodaysMarketCollectionChange[];
    intent: ChangeIntent;
}

export interface PutManyTodaysMarketCollectionsResponse {
    result: Result;
    collections: TodaysMarketCollection[];
}

export interface DeleteTodaysMarketCollectionRequest {
    removal: TodaysMarketCollectionRemoval;
    intent: ChangeIntent;
}

export interface DeleteTodaysMarketCollectionResponse {
    result: Result;
}

export interface DeleteManyTodaysMarketCollectionsRequest {
    removals: TodaysMarketCollectionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTodaysMarketCollectionsResponse {
    result: Result;
}

export interface ListByTodaysMarketConfigIdTodaysMarketCollectionsRequest {
    todays_market_config_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketCollectionsFilter | null;
}

export interface ListByTodaysMarketConfigIdTodaysMarketCollectionsResponse {
    result: Result;
    collections: TodaysMarketCollection[];
    total: number;
}

export interface ListTodaysMarketCollectionVersionsRequest {
    key: TodaysMarketCollectionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketCollectionVersionsFilter | null;
}

export interface ListTodaysMarketCollectionVersionsResponse {
    result: Result;
    versions: TodaysMarketCollection[];
    total: number;
}

export interface GetTodaysMarketCollectionVersionRequest {
    key: TodaysMarketCollectionVersionKey;
}

export interface GetTodaysMarketCollectionVersionResponse {
    result: Result;
    version: TodaysMarketCollection | null;
}

export const subjects = {
    list_todays_market_collections_request: 'analytics.v1.todays_market_collections.list',
    get_todays_market_collection_request: 'analytics.v1.todays_market_collections.get',
    get_many_todays_market_collections_request: 'analytics.v1.todays_market_collections.get_many',
    put_todays_market_collection_request: 'analytics.v1.todays_market_collections.put',
    put_many_todays_market_collections_request: 'analytics.v1.todays_market_collections.put_many',
    delete_todays_market_collection_request: 'analytics.v1.todays_market_collections.delete',
    delete_many_todays_market_collections_request:
        'analytics.v1.todays_market_collections.delete_many',
    list_by_todays_market_config_id_todays_market_collections_request:
        'analytics.v1.todays_market_collections.list_by_todays_market_config_id',
    list_todays_market_collection_versions_request:
        'analytics.v1.todays_market_collections_versions.list',
    get_todays_market_collection_version_request:
        'analytics.v1.todays_market_collections_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_todays_market_collections_request: true,
    get_todays_market_collection_request: true,
    get_many_todays_market_collections_request: true,
    put_todays_market_collection_request: true,
    put_many_todays_market_collections_request: true,
    delete_todays_market_collection_request: true,
    delete_many_todays_market_collections_request: true,
    list_by_todays_market_config_id_todays_market_collections_request: true,
    list_todays_market_collection_versions_request: true,
    get_todays_market_collection_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.todays_market_collections_events.created',
    updated: 'analytics.v1.todays_market_collections_events.updated',
    deleted: 'analytics.v1.todays_market_collections_events.deleted',
} as const;
