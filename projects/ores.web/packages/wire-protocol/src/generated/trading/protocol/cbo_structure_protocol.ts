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
import type { CboStructure } from '../domain/cbo_structure.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CboStructureKey {
    trade_id: string;
}

export interface CboStructureWrite {
    trade_id: string;
    trade_activity_id: string;
    day_counter: string;
    payment_convention: string;
    currency: string;
    reinvestment_end_date: string | null;
    senior_fee: string;
    fee_day_counter: string;
    subordinated_fee: string;
    equity_kicker: string;
}

export interface CboStructureChange {
    write: CboStructureWrite;
    precondition: Precondition;
}

export interface CboStructureRemoval {
    key: CboStructureKey;
    precondition: Precondition;
}

export interface CboStructureLookup {
    key: CboStructureKey;
    cbo_structure: CboStructure | null;
}

export interface CboStructuresFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface CboStructureEvent {
    event_id: string;
    key: CboStructureKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CboStructureVersionKey {
    cbo_structure: CboStructureKey;
    version: number;
}

export interface CboStructureVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCboStructuresRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CboStructuresFilter | null;
    as_of: string | null;
}

export interface ListCboStructuresResponse {
    result: Result;
    cbo_structures: CboStructure[];
    total: number;
}

export interface GetCboStructureRequest {
    key: CboStructureKey;
}

export interface GetCboStructureResponse {
    result: Result;
    cbo_structure: CboStructure | null;
}

export interface GetManyCboStructuresRequest {
    keys: CboStructureKey[];
}

export interface GetManyCboStructuresResponse {
    result: Result;
    entries: CboStructureLookup[];
}

export interface PutCboStructureRequest {
    change: CboStructureChange;
    intent: ChangeIntent;
}

export interface PutCboStructureResponse {
    result: Result;
    cbo_structure: CboStructure | null;
}

export interface PutManyCboStructuresRequest {
    changes: CboStructureChange[];
    intent: ChangeIntent;
}

export interface PutManyCboStructuresResponse {
    result: Result;
    cbo_structures: CboStructure[];
}

export interface DeleteCboStructureRequest {
    removal: CboStructureRemoval;
    intent: ChangeIntent;
}

export interface DeleteCboStructureResponse {
    result: Result;
}

export interface DeleteManyCboStructuresRequest {
    removals: CboStructureRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCboStructuresResponse {
    result: Result;
}

export interface ListByTradeIdCboStructuresRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CboStructuresFilter | null;
}

export interface ListByTradeIdCboStructuresResponse {
    result: Result;
    cbo_structures: CboStructure[];
    total: number;
}

export interface ListCboStructureVersionsRequest {
    key: CboStructureKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CboStructureVersionsFilter | null;
}

export interface ListCboStructureVersionsResponse {
    result: Result;
    versions: CboStructure[];
    total: number;
}

export interface GetCboStructureVersionRequest {
    key: CboStructureVersionKey;
}

export interface GetCboStructureVersionResponse {
    result: Result;
    version: CboStructure | null;
}

export const subjects = {
    list_cbo_structures_request: 'trading.v1.cbo_structures.list',
    get_cbo_structure_request: 'trading.v1.cbo_structures.get',
    get_many_cbo_structures_request: 'trading.v1.cbo_structures.get_many',
    put_cbo_structure_request: 'trading.v1.cbo_structures.put',
    put_many_cbo_structures_request: 'trading.v1.cbo_structures.put_many',
    delete_cbo_structure_request: 'trading.v1.cbo_structures.delete',
    delete_many_cbo_structures_request: 'trading.v1.cbo_structures.delete_many',
    list_by_trade_id_cbo_structures_request: 'trading.v1.cbo_structures.list_by_trade_id',
    list_cbo_structure_versions_request: 'trading.v1.cbo_structures_versions.list',
    get_cbo_structure_version_request: 'trading.v1.cbo_structures_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cbo_structures_request: true,
    get_cbo_structure_request: true,
    get_many_cbo_structures_request: true,
    put_cbo_structure_request: true,
    put_many_cbo_structures_request: true,
    delete_cbo_structure_request: true,
    delete_many_cbo_structures_request: true,
    list_by_trade_id_cbo_structures_request: true,
    list_cbo_structure_versions_request: true,
    get_cbo_structure_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.cbo_structures.created',
    updated: 'trading.v1.events.cbo_structures.updated',
    deleted: 'trading.v1.events.cbo_structures.deleted',
} as const;
