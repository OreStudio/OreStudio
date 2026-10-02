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
import type { PriceType } from '../domain/price_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PriceTypeKey {
    code: string;
}

export interface PriceTypeWrite {
    code: string;
    description: string;
}

export interface PriceTypeChange {
    write: PriceTypeWrite;
    precondition: Precondition;
}

export interface PriceTypeRemoval {
    key: PriceTypeKey;
    precondition: Precondition;
}

export interface PriceTypeLookup {
    key: PriceTypeKey;
    price_type: PriceType | null;
}

export interface PriceTypeEvent {
    event_id: string;
    key: PriceTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PriceTypeVersionKey {
    price_type: PriceTypeKey;
    version: number;
}

export interface PriceTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPriceTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPriceTypesResponse {
    result: Result;
    price_types: PriceType[];
    total: number;
}

export interface GetPriceTypeRequest {
    key: PriceTypeKey;
}

export interface GetPriceTypeResponse {
    result: Result;
    price_type: PriceType | null;
}

export interface GetManyPriceTypesRequest {
    keys: PriceTypeKey[];
}

export interface GetManyPriceTypesResponse {
    result: Result;
    entries: PriceTypeLookup[];
}

export interface PutPriceTypeRequest {
    change: PriceTypeChange;
    intent: ChangeIntent;
}

export interface PutPriceTypeResponse {
    result: Result;
    price_type: PriceType | null;
}

export interface PutManyPriceTypesRequest {
    changes: PriceTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyPriceTypesResponse {
    result: Result;
    price_types: PriceType[];
}

export interface DeletePriceTypeRequest {
    removal: PriceTypeRemoval;
    intent: ChangeIntent;
}

export interface DeletePriceTypeResponse {
    result: Result;
}

export interface DeleteManyPriceTypesRequest {
    removals: PriceTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPriceTypesResponse {
    result: Result;
}

export interface ListPriceTypeVersionsRequest {
    key: PriceTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PriceTypeVersionsFilter | null;
}

export interface ListPriceTypeVersionsResponse {
    result: Result;
    versions: PriceType[];
    total: number;
}

export interface GetPriceTypeVersionRequest {
    key: PriceTypeVersionKey;
}

export interface GetPriceTypeVersionResponse {
    result: Result;
    version: PriceType | null;
}

export const subjects = {
    list_price_types_request: 'trading.v1.price_types.list',
    get_price_type_request: 'trading.v1.price_types.get',
    get_many_price_types_request: 'trading.v1.price_types.get_many',
    put_price_type_request: 'trading.v1.price_types.put',
    put_many_price_types_request: 'trading.v1.price_types.put_many',
    delete_price_type_request: 'trading.v1.price_types.delete',
    delete_many_price_types_request: 'trading.v1.price_types.delete_many',
    list_price_type_versions_request: 'trading.v1.price_types_versions.list',
    get_price_type_version_request: 'trading.v1.price_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_price_types_request: true,
    get_price_type_request: true,
    get_many_price_types_request: true,
    put_price_type_request: true,
    put_many_price_types_request: true,
    delete_price_type_request: true,
    delete_many_price_types_request: true,
    list_price_type_versions_request: true,
    get_price_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.price_types_events.created',
    updated: 'trading.v1.price_types_events.updated',
    deleted: 'trading.v1.price_types_events.deleted',
} as const;
