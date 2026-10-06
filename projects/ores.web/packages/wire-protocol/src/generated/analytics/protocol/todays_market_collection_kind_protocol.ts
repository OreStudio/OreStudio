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
import type { TodaysMarketCollectionKind } from '../domain/todays_market_collection_kind.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TodaysMarketCollectionKindKey {
    code: string;
}

export interface TodaysMarketCollectionKindWrite {
    code: string;
    entry_element: string;
    key_attribute: string;
    key_attribute_2: string | null;
    description: string;
}

export interface TodaysMarketCollectionKindChange {
    write: TodaysMarketCollectionKindWrite;
    precondition: Precondition;
}

export interface TodaysMarketCollectionKindRemoval {
    key: TodaysMarketCollectionKindKey;
    precondition: Precondition;
}

export interface TodaysMarketCollectionKindLookup {
    key: TodaysMarketCollectionKindKey;
    todays_market_collection_kind: TodaysMarketCollectionKind | null;
}

export interface TodaysMarketCollectionKindsFilter {
    code_one_of: string[] | null;
}

export interface TodaysMarketCollectionKindEvent {
    event_id: string;
    key: TodaysMarketCollectionKindKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TodaysMarketCollectionKindVersionKey {
    todays_market_collection_kind: TodaysMarketCollectionKindKey;
    version: number;
}

export interface TodaysMarketCollectionKindVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTodaysMarketCollectionKindsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketCollectionKindsFilter | null;
    as_of: string | null;
}

export interface ListTodaysMarketCollectionKindsResponse {
    result: Result;
    kinds: TodaysMarketCollectionKind[];
    total: number;
}

export interface GetTodaysMarketCollectionKindRequest {
    key: TodaysMarketCollectionKindKey;
}

export interface GetTodaysMarketCollectionKindResponse {
    result: Result;
    todays_market_collection_kind: TodaysMarketCollectionKind | null;
}

export interface GetManyTodaysMarketCollectionKindsRequest {
    keys: TodaysMarketCollectionKindKey[];
}

export interface GetManyTodaysMarketCollectionKindsResponse {
    result: Result;
    entries: TodaysMarketCollectionKindLookup[];
}

export interface PutTodaysMarketCollectionKindRequest {
    change: TodaysMarketCollectionKindChange;
    intent: ChangeIntent;
}

export interface PutTodaysMarketCollectionKindResponse {
    result: Result;
    todays_market_collection_kind: TodaysMarketCollectionKind | null;
}

export interface PutManyTodaysMarketCollectionKindsRequest {
    changes: TodaysMarketCollectionKindChange[];
    intent: ChangeIntent;
}

export interface PutManyTodaysMarketCollectionKindsResponse {
    result: Result;
    kinds: TodaysMarketCollectionKind[];
}

export interface DeleteTodaysMarketCollectionKindRequest {
    removal: TodaysMarketCollectionKindRemoval;
    intent: ChangeIntent;
}

export interface DeleteTodaysMarketCollectionKindResponse {
    result: Result;
}

export interface DeleteManyTodaysMarketCollectionKindsRequest {
    removals: TodaysMarketCollectionKindRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTodaysMarketCollectionKindsResponse {
    result: Result;
}

export interface ListTodaysMarketCollectionKindVersionsRequest {
    key: TodaysMarketCollectionKindKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketCollectionKindVersionsFilter | null;
}

export interface ListTodaysMarketCollectionKindVersionsResponse {
    result: Result;
    versions: TodaysMarketCollectionKind[];
    total: number;
}

export interface GetTodaysMarketCollectionKindVersionRequest {
    key: TodaysMarketCollectionKindVersionKey;
}

export interface GetTodaysMarketCollectionKindVersionResponse {
    result: Result;
    version: TodaysMarketCollectionKind | null;
}

export const subjects = {
    list_todays_market_collection_kinds_request: 'analytics.v1.todays_market_collection_kinds.list',
    get_todays_market_collection_kind_request: 'analytics.v1.todays_market_collection_kinds.get',
    get_many_todays_market_collection_kinds_request:
        'analytics.v1.todays_market_collection_kinds.get_many',
    put_todays_market_collection_kind_request: 'analytics.v1.todays_market_collection_kinds.put',
    put_many_todays_market_collection_kinds_request:
        'analytics.v1.todays_market_collection_kinds.put_many',
    delete_todays_market_collection_kind_request:
        'analytics.v1.todays_market_collection_kinds.delete',
    delete_many_todays_market_collection_kinds_request:
        'analytics.v1.todays_market_collection_kinds.delete_many',
    list_todays_market_collection_kind_versions_request:
        'analytics.v1.todays_market_collection_kinds_versions.list',
    get_todays_market_collection_kind_version_request:
        'analytics.v1.todays_market_collection_kinds_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_todays_market_collection_kinds_request: true,
    get_todays_market_collection_kind_request: true,
    get_many_todays_market_collection_kinds_request: true,
    put_todays_market_collection_kind_request: true,
    put_many_todays_market_collection_kinds_request: true,
    delete_todays_market_collection_kind_request: true,
    delete_many_todays_market_collection_kinds_request: true,
    list_todays_market_collection_kind_versions_request: true,
    get_todays_market_collection_kind_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.todays_market_collection_kinds_events.created',
    updated: 'analytics.v1.todays_market_collection_kinds_events.updated',
    deleted: 'analytics.v1.todays_market_collection_kinds_events.deleted',
} as const;
