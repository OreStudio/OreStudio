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
import type { TodaysMarketEntry } from '../domain/todays_market_entry.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TodaysMarketEntryKey {
    id: string;
}

export interface TodaysMarketEntryWrite {
    id: string;
    todays_market_config_id: string;
    todays_market_collection_id: string;
    key_value: string | null;
    key_value_2: string | null;
    target: string;
    discounting: string | null;
    position: number;
}

export interface TodaysMarketEntryChange {
    write: TodaysMarketEntryWrite;
    precondition: Precondition;
}

export interface TodaysMarketEntryRemoval {
    key: TodaysMarketEntryKey;
    precondition: Precondition;
}

export interface TodaysMarketEntryLookup {
    key: TodaysMarketEntryKey;
    todays_market_entry: TodaysMarketEntry | null;
}

export interface TodaysMarketEntriesFilter {
    todays_market_config_id: string | null;
    todays_market_collection_id: string | null;
}

export interface TodaysMarketEntryEvent {
    event_id: string;
    key: TodaysMarketEntryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TodaysMarketEntryVersionKey {
    todays_market_entry: TodaysMarketEntryKey;
    version: number;
}

export interface TodaysMarketEntryVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTodaysMarketEntriesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketEntriesFilter | null;
}

export interface ListTodaysMarketEntriesResponse {
    result: Result;
    entries: TodaysMarketEntry[];
    total: number;
}

export interface GetTodaysMarketEntryRequest {
    key: TodaysMarketEntryKey;
}

export interface GetTodaysMarketEntryResponse {
    result: Result;
    todays_market_entry: TodaysMarketEntry | null;
}

export interface GetManyTodaysMarketEntriesRequest {
    keys: TodaysMarketEntryKey[];
}

export interface GetManyTodaysMarketEntriesResponse {
    result: Result;
    entries: TodaysMarketEntryLookup[];
}

export interface PutTodaysMarketEntryRequest {
    change: TodaysMarketEntryChange;
    intent: ChangeIntent;
}

export interface PutTodaysMarketEntryResponse {
    result: Result;
    todays_market_entry: TodaysMarketEntry | null;
}

export interface PutManyTodaysMarketEntriesRequest {
    changes: TodaysMarketEntryChange[];
    intent: ChangeIntent;
}

export interface PutManyTodaysMarketEntriesResponse {
    result: Result;
    entries: TodaysMarketEntry[];
}

export interface DeleteTodaysMarketEntryRequest {
    removal: TodaysMarketEntryRemoval;
    intent: ChangeIntent;
}

export interface DeleteTodaysMarketEntryResponse {
    result: Result;
}

export interface DeleteManyTodaysMarketEntriesRequest {
    removals: TodaysMarketEntryRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTodaysMarketEntriesResponse {
    result: Result;
}

export interface ListByTodaysMarketConfigIdTodaysMarketEntriesRequest {
    todays_market_config_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketEntriesFilter | null;
}

export interface ListByTodaysMarketConfigIdTodaysMarketEntriesResponse {
    result: Result;
    entries: TodaysMarketEntry[];
    total: number;
}

export interface ListByTodaysMarketCollectionIdTodaysMarketEntriesRequest {
    todays_market_collection_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketEntriesFilter | null;
}

export interface ListByTodaysMarketCollectionIdTodaysMarketEntriesResponse {
    result: Result;
    entries: TodaysMarketEntry[];
    total: number;
}

export interface ListTodaysMarketEntryVersionsRequest {
    key: TodaysMarketEntryKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TodaysMarketEntryVersionsFilter | null;
}

export interface ListTodaysMarketEntryVersionsResponse {
    result: Result;
    versions: TodaysMarketEntry[];
    total: number;
}

export interface GetTodaysMarketEntryVersionRequest {
    key: TodaysMarketEntryVersionKey;
}

export interface GetTodaysMarketEntryVersionResponse {
    result: Result;
    version: TodaysMarketEntry | null;
}

export const subjects = {
    list_todays_market_entries_request: 'analytics.v1.todays_market_entries.list',
    get_todays_market_entry_request: 'analytics.v1.todays_market_entries.get',
    get_many_todays_market_entries_request: 'analytics.v1.todays_market_entries.get_many',
    put_todays_market_entry_request: 'analytics.v1.todays_market_entries.put',
    put_many_todays_market_entries_request: 'analytics.v1.todays_market_entries.put_many',
    delete_todays_market_entry_request: 'analytics.v1.todays_market_entries.delete',
    delete_many_todays_market_entries_request: 'analytics.v1.todays_market_entries.delete_many',
    list_by_todays_market_config_id_todays_market_entries_request:
        'analytics.v1.todays_market_entries.list_by_todays_market_config_id',
    list_by_todays_market_collection_id_todays_market_entries_request:
        'analytics.v1.todays_market_entries.list_by_todays_market_collection_id',
    list_todays_market_entry_versions_request: 'analytics.v1.todays_market_entries_versions.list',
    get_todays_market_entry_version_request: 'analytics.v1.todays_market_entries_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_todays_market_entries_request: true,
    get_todays_market_entry_request: true,
    get_many_todays_market_entries_request: true,
    put_todays_market_entry_request: true,
    put_many_todays_market_entries_request: true,
    delete_todays_market_entry_request: true,
    delete_many_todays_market_entries_request: true,
    list_by_todays_market_config_id_todays_market_entries_request: true,
    list_by_todays_market_collection_id_todays_market_entries_request: true,
    list_todays_market_entry_versions_request: true,
    get_todays_market_entry_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.todays_market_entries_events.created',
    updated: 'analytics.v1.todays_market_entries_events.updated',
    deleted: 'analytics.v1.todays_market_entries_events.deleted',
} as const;
