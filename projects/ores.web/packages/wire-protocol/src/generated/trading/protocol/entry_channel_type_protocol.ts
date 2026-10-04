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
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EntryChannelTypeKey {
    code: string;
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

export interface ListEntryChannelTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: EntryChannelTypesFilter | null;
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

export const subjects = {
    list_entry_channel_types_request: 'trading.v1.entry_channel_types.list',
    get_entry_channel_type_request: 'trading.v1.entry_channel_types.get',
    get_many_entry_channel_types_request: 'trading.v1.entry_channel_types.get_many',
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
