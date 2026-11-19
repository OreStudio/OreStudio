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
import type { CboTranche } from '../domain/cbo_tranche.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CboTrancheKey {
    trade_id: string;
    sequence_number: number;
}

export interface CboTrancheWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    name: string;
    notional: string;
    ic_ratio: number;
    oc_ratio: number;
}

export interface CboTrancheChange {
    write: CboTrancheWrite;
    precondition: Precondition;
}

export interface CboTrancheRemoval {
    key: CboTrancheKey;
    precondition: Precondition;
}

export interface CboTrancheLookup {
    key: CboTrancheKey;
    cbo_tranche: CboTranche | null;
}

export interface CboTranchesFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface CboTrancheEvent {
    event_id: string;
    key: CboTrancheKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CboTrancheVersionKey {
    cbo_tranche: CboTrancheKey;
    version: number;
}

export interface CboTrancheVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCboTranchesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CboTranchesFilter | null;
    as_of: string | null;
}

export interface ListCboTranchesResponse {
    result: Result;
    cbo_tranches: CboTranche[];
    total: number;
}

export interface GetCboTrancheRequest {
    key: CboTrancheKey;
}

export interface GetCboTrancheResponse {
    result: Result;
    cbo_tranche: CboTranche | null;
}

export interface GetManyCboTranchesRequest {
    keys: CboTrancheKey[];
}

export interface GetManyCboTranchesResponse {
    result: Result;
    entries: CboTrancheLookup[];
}

export interface PutCboTrancheRequest {
    change: CboTrancheChange;
    intent: ChangeIntent;
}

export interface PutCboTrancheResponse {
    result: Result;
    cbo_tranche: CboTranche | null;
}

export interface PutManyCboTranchesRequest {
    changes: CboTrancheChange[];
    intent: ChangeIntent;
}

export interface PutManyCboTranchesResponse {
    result: Result;
    cbo_tranches: CboTranche[];
}

export interface DeleteCboTrancheRequest {
    removal: CboTrancheRemoval;
    intent: ChangeIntent;
}

export interface DeleteCboTrancheResponse {
    result: Result;
}

export interface DeleteManyCboTranchesRequest {
    removals: CboTrancheRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCboTranchesResponse {
    result: Result;
}

export interface ListByTradeIdCboTranchesRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CboTranchesFilter | null;
}

export interface ListByTradeIdCboTranchesResponse {
    result: Result;
    cbo_tranches: CboTranche[];
    total: number;
}

export interface ListCboTrancheVersionsRequest {
    key: CboTrancheKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CboTrancheVersionsFilter | null;
}

export interface ListCboTrancheVersionsResponse {
    result: Result;
    versions: CboTranche[];
    total: number;
}

export interface GetCboTrancheVersionRequest {
    key: CboTrancheVersionKey;
}

export interface GetCboTrancheVersionResponse {
    result: Result;
    version: CboTranche | null;
}

export const subjects = {
    list_cbo_tranches_request: 'trading.v1.cbo_tranches.list',
    get_cbo_tranche_request: 'trading.v1.cbo_tranches.get',
    get_many_cbo_tranches_request: 'trading.v1.cbo_tranches.get_many',
    put_cbo_tranche_request: 'trading.v1.cbo_tranches.put',
    put_many_cbo_tranches_request: 'trading.v1.cbo_tranches.put_many',
    delete_cbo_tranche_request: 'trading.v1.cbo_tranches.delete',
    delete_many_cbo_tranches_request: 'trading.v1.cbo_tranches.delete_many',
    list_by_trade_id_cbo_tranches_request: 'trading.v1.cbo_tranches.list_by_trade_id',
    list_cbo_tranche_versions_request: 'trading.v1.cbo_tranches_versions.list',
    get_cbo_tranche_version_request: 'trading.v1.cbo_tranches_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cbo_tranches_request: true,
    get_cbo_tranche_request: true,
    get_many_cbo_tranches_request: true,
    put_cbo_tranche_request: true,
    put_many_cbo_tranches_request: true,
    delete_cbo_tranche_request: true,
    delete_many_cbo_tranches_request: true,
    list_by_trade_id_cbo_tranches_request: true,
    list_cbo_tranche_versions_request: true,
    get_cbo_tranche_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.cbo_tranches.created',
    updated: 'trading.v1.events.cbo_tranches.updated',
    deleted: 'trading.v1.events.cbo_tranches.deleted',
} as const;
