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
import type { OptionType } from '../domain/option_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface OptionTypeKey {
    code: string;
}

export interface OptionTypeWrite {
    code: string;
    description: string;
}

export interface OptionTypeChange {
    write: OptionTypeWrite;
    precondition: Precondition;
}

export interface OptionTypeRemoval {
    key: OptionTypeKey;
    precondition: Precondition;
}

export interface OptionTypeLookup {
    key: OptionTypeKey;
    option_type: OptionType | null;
}

export interface OptionTypeEvent {
    event_id: string;
    key: OptionTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface OptionTypeVersionKey {
    option_type: OptionTypeKey;
    version: number;
}

export interface OptionTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListOptionTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListOptionTypesResponse {
    result: Result;
    option_types: OptionType[];
    total: number;
}

export interface GetOptionTypeRequest {
    key: OptionTypeKey;
}

export interface GetOptionTypeResponse {
    result: Result;
    option_type: OptionType | null;
}

export interface GetManyOptionTypesRequest {
    keys: OptionTypeKey[];
}

export interface GetManyOptionTypesResponse {
    result: Result;
    entries: OptionTypeLookup[];
}

export interface PutOptionTypeRequest {
    change: OptionTypeChange;
    intent: ChangeIntent;
}

export interface PutOptionTypeResponse {
    result: Result;
    option_type: OptionType | null;
}

export interface PutManyOptionTypesRequest {
    changes: OptionTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyOptionTypesResponse {
    result: Result;
    option_types: OptionType[];
}

export interface DeleteOptionTypeRequest {
    removal: OptionTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteOptionTypeResponse {
    result: Result;
}

export interface DeleteManyOptionTypesRequest {
    removals: OptionTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyOptionTypesResponse {
    result: Result;
}

export interface ListOptionTypeVersionsRequest {
    key: OptionTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: OptionTypeVersionsFilter | null;
}

export interface ListOptionTypeVersionsResponse {
    result: Result;
    versions: OptionType[];
    total: number;
}

export interface GetOptionTypeVersionRequest {
    key: OptionTypeVersionKey;
}

export interface GetOptionTypeVersionResponse {
    result: Result;
    version: OptionType | null;
}

export const subjects = {
    list_option_types_request: 'trading.v1.option_types.list',
    get_option_type_request: 'trading.v1.option_types.get',
    get_many_option_types_request: 'trading.v1.option_types.get_many',
    put_option_type_request: 'trading.v1.option_types.put',
    put_many_option_types_request: 'trading.v1.option_types.put_many',
    delete_option_type_request: 'trading.v1.option_types.delete',
    delete_many_option_types_request: 'trading.v1.option_types.delete_many',
    list_option_type_versions_request: 'trading.v1.option_types_versions.list',
    get_option_type_version_request: 'trading.v1.option_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_option_types_request: true,
    get_option_type_request: true,
    get_many_option_types_request: true,
    put_option_type_request: true,
    put_many_option_types_request: true,
    delete_option_type_request: true,
    delete_many_option_types_request: true,
    list_option_type_versions_request: true,
    get_option_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.option_types_events.created',
    updated: 'trading.v1.option_types_events.updated',
    deleted: 'trading.v1.option_types_events.deleted',
} as const;
