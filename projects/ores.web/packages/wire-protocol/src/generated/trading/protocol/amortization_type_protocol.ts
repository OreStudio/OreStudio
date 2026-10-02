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
import type { AmortizationType } from '../domain/amortization_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AmortizationTypeKey {
    code: string;
}

export interface AmortizationTypeWrite {
    code: string;
    description: string;
}

export interface AmortizationTypeChange {
    write: AmortizationTypeWrite;
    precondition: Precondition;
}

export interface AmortizationTypeRemoval {
    key: AmortizationTypeKey;
    precondition: Precondition;
}

export interface AmortizationTypeLookup {
    key: AmortizationTypeKey;
    amortization_type: AmortizationType | null;
}

export interface AmortizationTypeEvent {
    event_id: string;
    key: AmortizationTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AmortizationTypeVersionKey {
    amortization_type: AmortizationTypeKey;
    version: number;
}

export interface AmortizationTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAmortizationTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListAmortizationTypesResponse {
    result: Result;
    amortization_types: AmortizationType[];
    total: number;
}

export interface GetAmortizationTypeRequest {
    key: AmortizationTypeKey;
}

export interface GetAmortizationTypeResponse {
    result: Result;
    amortization_type: AmortizationType | null;
}

export interface GetManyAmortizationTypesRequest {
    keys: AmortizationTypeKey[];
}

export interface GetManyAmortizationTypesResponse {
    result: Result;
    entries: AmortizationTypeLookup[];
}

export interface PutAmortizationTypeRequest {
    change: AmortizationTypeChange;
    intent: ChangeIntent;
}

export interface PutAmortizationTypeResponse {
    result: Result;
    amortization_type: AmortizationType | null;
}

export interface PutManyAmortizationTypesRequest {
    changes: AmortizationTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyAmortizationTypesResponse {
    result: Result;
    amortization_types: AmortizationType[];
}

export interface DeleteAmortizationTypeRequest {
    removal: AmortizationTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteAmortizationTypeResponse {
    result: Result;
}

export interface DeleteManyAmortizationTypesRequest {
    removals: AmortizationTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAmortizationTypesResponse {
    result: Result;
}

export interface ListAmortizationTypeVersionsRequest {
    key: AmortizationTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AmortizationTypeVersionsFilter | null;
}

export interface ListAmortizationTypeVersionsResponse {
    result: Result;
    versions: AmortizationType[];
    total: number;
}

export interface GetAmortizationTypeVersionRequest {
    key: AmortizationTypeVersionKey;
}

export interface GetAmortizationTypeVersionResponse {
    result: Result;
    version: AmortizationType | null;
}

export const subjects = {
    list_amortization_types_request: 'trading.v1.amortization_types.list',
    get_amortization_type_request: 'trading.v1.amortization_types.get',
    get_many_amortization_types_request: 'trading.v1.amortization_types.get_many',
    put_amortization_type_request: 'trading.v1.amortization_types.put',
    put_many_amortization_types_request: 'trading.v1.amortization_types.put_many',
    delete_amortization_type_request: 'trading.v1.amortization_types.delete',
    delete_many_amortization_types_request: 'trading.v1.amortization_types.delete_many',
    list_amortization_type_versions_request: 'trading.v1.amortization_types_versions.list',
    get_amortization_type_version_request: 'trading.v1.amortization_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_amortization_types_request: true,
    get_amortization_type_request: true,
    get_many_amortization_types_request: true,
    put_amortization_type_request: true,
    put_many_amortization_types_request: true,
    delete_amortization_type_request: true,
    delete_many_amortization_types_request: true,
    list_amortization_type_versions_request: true,
    get_amortization_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.amortization_types_events.created',
    updated: 'trading.v1.amortization_types_events.updated',
    deleted: 'trading.v1.amortization_types_events.deleted',
} as const;
