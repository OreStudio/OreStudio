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
import type { PayoffType } from '../domain/payoff_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PayoffTypeKey {
    code: string;
}

export interface PayoffTypeWrite {
    code: string;
    description: string;
}

export interface PayoffTypeChange {
    write: PayoffTypeWrite;
    precondition: Precondition;
}

export interface PayoffTypeRemoval {
    key: PayoffTypeKey;
    precondition: Precondition;
}

export interface PayoffTypeLookup {
    key: PayoffTypeKey;
    payoff_type: PayoffType | null;
}

export interface PayoffTypesFilter {
    code_one_of: string[] | null;
}

export interface PayoffTypeEvent {
    event_id: string;
    key: PayoffTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PayoffTypeVersionKey {
    payoff_type: PayoffTypeKey;
    version: number;
}

export interface PayoffTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPayoffTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: PayoffTypesFilter | null;
}

export interface ListPayoffTypesResponse {
    result: Result;
    payoff_types: PayoffType[];
    total: number;
}

export interface GetPayoffTypeRequest {
    key: PayoffTypeKey;
}

export interface GetPayoffTypeResponse {
    result: Result;
    payoff_type: PayoffType | null;
}

export interface GetManyPayoffTypesRequest {
    keys: PayoffTypeKey[];
}

export interface GetManyPayoffTypesResponse {
    result: Result;
    entries: PayoffTypeLookup[];
}

export interface PutPayoffTypeRequest {
    change: PayoffTypeChange;
    intent: ChangeIntent;
}

export interface PutPayoffTypeResponse {
    result: Result;
    payoff_type: PayoffType | null;
}

export interface PutManyPayoffTypesRequest {
    changes: PayoffTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyPayoffTypesResponse {
    result: Result;
    payoff_types: PayoffType[];
}

export interface DeletePayoffTypeRequest {
    removal: PayoffTypeRemoval;
    intent: ChangeIntent;
}

export interface DeletePayoffTypeResponse {
    result: Result;
}

export interface DeleteManyPayoffTypesRequest {
    removals: PayoffTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPayoffTypesResponse {
    result: Result;
}

export interface ListPayoffTypeVersionsRequest {
    key: PayoffTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PayoffTypeVersionsFilter | null;
}

export interface ListPayoffTypeVersionsResponse {
    result: Result;
    versions: PayoffType[];
    total: number;
}

export interface GetPayoffTypeVersionRequest {
    key: PayoffTypeVersionKey;
}

export interface GetPayoffTypeVersionResponse {
    result: Result;
    version: PayoffType | null;
}

export const subjects = {
    list_payoff_types_request: 'trading.v1.payoff_types.list',
    get_payoff_type_request: 'trading.v1.payoff_types.get',
    get_many_payoff_types_request: 'trading.v1.payoff_types.get_many',
    put_payoff_type_request: 'trading.v1.payoff_types.put',
    put_many_payoff_types_request: 'trading.v1.payoff_types.put_many',
    delete_payoff_type_request: 'trading.v1.payoff_types.delete',
    delete_many_payoff_types_request: 'trading.v1.payoff_types.delete_many',
    list_payoff_type_versions_request: 'trading.v1.payoff_types_versions.list',
    get_payoff_type_version_request: 'trading.v1.payoff_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_payoff_types_request: true,
    get_payoff_type_request: true,
    get_many_payoff_types_request: true,
    put_payoff_type_request: true,
    put_many_payoff_types_request: true,
    delete_payoff_type_request: true,
    delete_many_payoff_types_request: true,
    list_payoff_type_versions_request: true,
    get_payoff_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.payoff_types_events.created',
    updated: 'trading.v1.payoff_types_events.updated',
    deleted: 'trading.v1.payoff_types_events.deleted',
} as const;
