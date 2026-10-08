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
import type { EntryChannelType } from '../domain/entry_channel_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EntryChannelTypeKey {
    code: string;
}

export interface EntryChannelTypeWrite {
    code: string;
    description: string;
}

export interface EntryChannelTypeChange {
    write: EntryChannelTypeWrite;
    precondition: Precondition;
}

export interface EntryChannelTypeRemoval {
    key: EntryChannelTypeKey;
    precondition: Precondition;
}

export interface EntryChannelTypeLookup {
    key: EntryChannelTypeKey;
    entry_channel_type: EntryChannelType | null;
}

export interface EntryChannelTypesFilter {
    code_one_of: string[] | null;
}

export interface EntryChannelTypeEvent {
    event_id: string;
    key: EntryChannelTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EntryChannelTypeVersionKey {
    entry_channel_type: EntryChannelTypeKey;
    version: number;
}

export interface EntryChannelTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEntryChannelTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EntryChannelTypesFilter | null;
    as_of: string | null;
}

export interface ListEntryChannelTypesResponse {
    result: Result;
    entry_channel_types: EntryChannelType[];
    total: number;
}

export interface GetEntryChannelTypeRequest {
    key: EntryChannelTypeKey;
}

export interface GetEntryChannelTypeResponse {
    result: Result;
    entry_channel_type: EntryChannelType | null;
}

export interface GetManyEntryChannelTypesRequest {
    keys: EntryChannelTypeKey[];
}

export interface GetManyEntryChannelTypesResponse {
    result: Result;
    entries: EntryChannelTypeLookup[];
}

export interface PutEntryChannelTypeRequest {
    change: EntryChannelTypeChange;
    intent: ChangeIntent;
}

export interface PutEntryChannelTypeResponse {
    result: Result;
    entry_channel_type: EntryChannelType | null;
}

export interface PutManyEntryChannelTypesRequest {
    changes: EntryChannelTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyEntryChannelTypesResponse {
    result: Result;
    entry_channel_types: EntryChannelType[];
}

export interface DeleteEntryChannelTypeRequest {
    removal: EntryChannelTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteEntryChannelTypeResponse {
    result: Result;
}

export interface DeleteManyEntryChannelTypesRequest {
    removals: EntryChannelTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEntryChannelTypesResponse {
    result: Result;
}

export interface ListEntryChannelTypeVersionsRequest {
    key: EntryChannelTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EntryChannelTypeVersionsFilter | null;
}

export interface ListEntryChannelTypeVersionsResponse {
    result: Result;
    versions: EntryChannelType[];
    total: number;
}

export interface GetEntryChannelTypeVersionRequest {
    key: EntryChannelTypeVersionKey;
}

export interface GetEntryChannelTypeVersionResponse {
    result: Result;
    version: EntryChannelType | null;
}

export const subjects = {
    list_entry_channel_types_request: 'trading.v1.entry_channel_types.list',
    get_entry_channel_type_request: 'trading.v1.entry_channel_types.get',
    get_many_entry_channel_types_request: 'trading.v1.entry_channel_types.get_many',
    put_entry_channel_type_request: 'trading.v1.entry_channel_types.put',
    put_many_entry_channel_types_request: 'trading.v1.entry_channel_types.put_many',
    delete_entry_channel_type_request: 'trading.v1.entry_channel_types.delete',
    delete_many_entry_channel_types_request: 'trading.v1.entry_channel_types.delete_many',
    list_entry_channel_type_versions_request: 'trading.v1.entry_channel_types_versions.list',
    get_entry_channel_type_version_request: 'trading.v1.entry_channel_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_entry_channel_types_request: true,
    get_entry_channel_type_request: true,
    get_many_entry_channel_types_request: true,
    put_entry_channel_type_request: true,
    put_many_entry_channel_types_request: true,
    delete_entry_channel_type_request: true,
    delete_many_entry_channel_types_request: true,
    list_entry_channel_type_versions_request: true,
    get_entry_channel_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.entry_channel_types_events.created',
    updated: 'trading.v1.entry_channel_types_events.updated',
    deleted: 'trading.v1.entry_channel_types_events.deleted',
} as const;
