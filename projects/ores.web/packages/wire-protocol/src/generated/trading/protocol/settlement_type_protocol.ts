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
import type { SettlementType } from '../domain/settlement_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SettlementTypeKey {
    code: string;
}

export interface SettlementTypeWrite {
    code: string;
    description: string;
}

export interface SettlementTypeChange {
    write: SettlementTypeWrite;
    precondition: Precondition;
}

export interface SettlementTypeRemoval {
    key: SettlementTypeKey;
    precondition: Precondition;
}

export interface SettlementTypeLookup {
    key: SettlementTypeKey;
    settlement_type: SettlementType | null;
}

export interface SettlementTypesFilter {
    code_one_of: string[] | null;
}

export interface SettlementTypeEvent {
    event_id: string;
    key: SettlementTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SettlementTypeVersionKey {
    settlement_type: SettlementTypeKey;
    version: number;
}

export interface SettlementTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSettlementTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SettlementTypesFilter | null;
    as_of: string | null;
}

export interface ListSettlementTypesResponse {
    result: Result;
    settlement_types: SettlementType[];
    total: number;
}

export interface GetSettlementTypeRequest {
    key: SettlementTypeKey;
}

export interface GetSettlementTypeResponse {
    result: Result;
    settlement_type: SettlementType | null;
}

export interface GetManySettlementTypesRequest {
    keys: SettlementTypeKey[];
}

export interface GetManySettlementTypesResponse {
    result: Result;
    entries: SettlementTypeLookup[];
}

export interface PutSettlementTypeRequest {
    change: SettlementTypeChange;
    intent: ChangeIntent;
}

export interface PutSettlementTypeResponse {
    result: Result;
    settlement_type: SettlementType | null;
}

export interface PutManySettlementTypesRequest {
    changes: SettlementTypeChange[];
    intent: ChangeIntent;
}

export interface PutManySettlementTypesResponse {
    result: Result;
    settlement_types: SettlementType[];
}

export interface DeleteSettlementTypeRequest {
    removal: SettlementTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteSettlementTypeResponse {
    result: Result;
}

export interface DeleteManySettlementTypesRequest {
    removals: SettlementTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySettlementTypesResponse {
    result: Result;
}

export interface ListSettlementTypeVersionsRequest {
    key: SettlementTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SettlementTypeVersionsFilter | null;
}

export interface ListSettlementTypeVersionsResponse {
    result: Result;
    versions: SettlementType[];
    total: number;
}

export interface GetSettlementTypeVersionRequest {
    key: SettlementTypeVersionKey;
}

export interface GetSettlementTypeVersionResponse {
    result: Result;
    version: SettlementType | null;
}

export const subjects = {
    list_settlement_types_request: 'trading.v1.settlement_types.list',
    get_settlement_type_request: 'trading.v1.settlement_types.get',
    get_many_settlement_types_request: 'trading.v1.settlement_types.get_many',
    put_settlement_type_request: 'trading.v1.settlement_types.put',
    put_many_settlement_types_request: 'trading.v1.settlement_types.put_many',
    delete_settlement_type_request: 'trading.v1.settlement_types.delete',
    delete_many_settlement_types_request: 'trading.v1.settlement_types.delete_many',
    list_settlement_type_versions_request: 'trading.v1.settlement_types_versions.list',
    get_settlement_type_version_request: 'trading.v1.settlement_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_settlement_types_request: true,
    get_settlement_type_request: true,
    get_many_settlement_types_request: true,
    put_settlement_type_request: true,
    put_many_settlement_types_request: true,
    delete_settlement_type_request: true,
    delete_many_settlement_types_request: true,
    list_settlement_type_versions_request: true,
    get_settlement_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.settlement_types_events.created',
    updated: 'trading.v1.settlement_types_events.updated',
    deleted: 'trading.v1.settlement_types_events.deleted',
} as const;
