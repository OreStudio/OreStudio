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
import type { CashflowType } from '../domain/cashflow_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CashflowTypeKey {
    code: string;
    coding_scheme_code: string;
}

export interface CashflowTypeWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface CashflowTypeChange {
    write: CashflowTypeWrite;
    precondition: Precondition;
}

export interface CashflowTypeRemoval {
    key: CashflowTypeKey;
    precondition: Precondition;
}

export interface CashflowTypeLookup {
    key: CashflowTypeKey;
    cashflow_type: CashflowType | null;
}

export interface CashflowTypeEvent {
    event_id: string;
    key: CashflowTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CashflowTypeVersionKey {
    cashflow_type: CashflowTypeKey;
    version: number;
}

export interface CashflowTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCashflowTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListCashflowTypesResponse {
    result: Result;
    types: CashflowType[];
    total: number;
}

export interface GetCashflowTypeRequest {
    key: CashflowTypeKey;
}

export interface GetCashflowTypeResponse {
    result: Result;
    cashflow_type: CashflowType | null;
}

export interface GetManyCashflowTypesRequest {
    keys: CashflowTypeKey[];
}

export interface GetManyCashflowTypesResponse {
    result: Result;
    entries: CashflowTypeLookup[];
}

export interface PutCashflowTypeRequest {
    change: CashflowTypeChange;
    intent: ChangeIntent;
}

export interface PutCashflowTypeResponse {
    result: Result;
    cashflow_type: CashflowType | null;
}

export interface PutManyCashflowTypesRequest {
    changes: CashflowTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyCashflowTypesResponse {
    result: Result;
    types: CashflowType[];
}

export interface DeleteCashflowTypeRequest {
    removal: CashflowTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteCashflowTypeResponse {
    result: Result;
}

export interface DeleteManyCashflowTypesRequest {
    removals: CashflowTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCashflowTypesResponse {
    result: Result;
}

export interface ListCashflowTypeVersionsRequest {
    key: CashflowTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CashflowTypeVersionsFilter | null;
}

export interface ListCashflowTypeVersionsResponse {
    result: Result;
    versions: CashflowType[];
    total: number;
}

export interface GetCashflowTypeVersionRequest {
    key: CashflowTypeVersionKey;
}

export interface GetCashflowTypeVersionResponse {
    result: Result;
    version: CashflowType | null;
}

export const subjects = {
    list_cashflow_types_request: 'refdata.v1.cashflow_types.list',
    get_cashflow_type_request: 'refdata.v1.cashflow_types.get',
    get_many_cashflow_types_request: 'refdata.v1.cashflow_types.get_many',
    put_cashflow_type_request: 'refdata.v1.cashflow_types.put',
    put_many_cashflow_types_request: 'refdata.v1.cashflow_types.put_many',
    delete_cashflow_type_request: 'refdata.v1.cashflow_types.delete',
    delete_many_cashflow_types_request: 'refdata.v1.cashflow_types.delete_many',
    list_cashflow_type_versions_request: 'refdata.v1.cashflow_types_versions.list',
    get_cashflow_type_version_request: 'refdata.v1.cashflow_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cashflow_types_request: true,
    get_cashflow_type_request: true,
    get_many_cashflow_types_request: true,
    put_cashflow_type_request: true,
    put_many_cashflow_types_request: true,
    delete_cashflow_type_request: true,
    delete_many_cashflow_types_request: true,
    list_cashflow_type_versions_request: true,
    get_cashflow_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cashflow_types_events.created',
    updated: 'refdata.v1.cashflow_types_events.updated',
    deleted: 'refdata.v1.cashflow_types_events.deleted',
} as const;
