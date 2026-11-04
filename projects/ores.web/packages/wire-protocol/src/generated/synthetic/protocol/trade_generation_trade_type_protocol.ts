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
import type { TradeGenerationTradeType } from '../domain/trade_generation_trade_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeGenerationTradeTypeKey {
    id: string;
}

export interface TradeGenerationTradeTypeWrite {
    id: string;
    config_id: string;
    trade_type_code: string;
}

export interface TradeGenerationTradeTypeChange {
    write: TradeGenerationTradeTypeWrite;
    precondition: Precondition;
}

export interface TradeGenerationTradeTypeRemoval {
    key: TradeGenerationTradeTypeKey;
    precondition: Precondition;
}

export interface TradeGenerationTradeTypeLookup {
    key: TradeGenerationTradeTypeKey;
    trade_generation_trade_type: TradeGenerationTradeType | null;
}

export interface TradeGenerationTradeTypesFilter {
    config_id: string | null;
    id_one_of: string[] | null;
    config_id_one_of: string[] | null;
}

export interface TradeGenerationTradeTypeEvent {
    event_id: string;
    key: TradeGenerationTradeTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeGenerationTradeTypeVersionKey {
    trade_generation_trade_type: TradeGenerationTradeTypeKey;
    version: number;
}

export interface TradeGenerationTradeTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeGenerationTradeTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeGenerationTradeTypesFilter | null;
    as_of: string | null;
}

export interface ListTradeGenerationTradeTypesResponse {
    result: Result;
    trade_generation_trade_types: TradeGenerationTradeType[];
    total: number;
}

export interface GetTradeGenerationTradeTypeRequest {
    key: TradeGenerationTradeTypeKey;
}

export interface GetTradeGenerationTradeTypeResponse {
    result: Result;
    trade_generation_trade_type: TradeGenerationTradeType | null;
}

export interface GetManyTradeGenerationTradeTypesRequest {
    keys: TradeGenerationTradeTypeKey[];
}

export interface GetManyTradeGenerationTradeTypesResponse {
    result: Result;
    entries: TradeGenerationTradeTypeLookup[];
}

export interface PutTradeGenerationTradeTypeRequest {
    change: TradeGenerationTradeTypeChange;
    intent: ChangeIntent;
}

export interface PutTradeGenerationTradeTypeResponse {
    result: Result;
    trade_generation_trade_type: TradeGenerationTradeType | null;
}

export interface PutManyTradeGenerationTradeTypesRequest {
    changes: TradeGenerationTradeTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeGenerationTradeTypesResponse {
    result: Result;
    trade_generation_trade_types: TradeGenerationTradeType[];
}

export interface DeleteTradeGenerationTradeTypeRequest {
    removal: TradeGenerationTradeTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeGenerationTradeTypeResponse {
    result: Result;
}

export interface DeleteManyTradeGenerationTradeTypesRequest {
    removals: TradeGenerationTradeTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeGenerationTradeTypesResponse {
    result: Result;
}

export interface ListTradeGenerationTradeTypeVersionsRequest {
    key: TradeGenerationTradeTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeGenerationTradeTypeVersionsFilter | null;
}

export interface ListTradeGenerationTradeTypeVersionsResponse {
    result: Result;
    versions: TradeGenerationTradeType[];
    total: number;
}

export interface GetTradeGenerationTradeTypeVersionRequest {
    key: TradeGenerationTradeTypeVersionKey;
}

export interface GetTradeGenerationTradeTypeVersionResponse {
    result: Result;
    version: TradeGenerationTradeType | null;
}

export const subjects = {
    list_trade_generation_trade_types_request: 'synthetic.v1.trade_generation_trade_types.list',
    get_trade_generation_trade_type_request: 'synthetic.v1.trade_generation_trade_types.get',
    get_many_trade_generation_trade_types_request:
        'synthetic.v1.trade_generation_trade_types.get_many',
    put_trade_generation_trade_type_request: 'synthetic.v1.trade_generation_trade_types.put',
    put_many_trade_generation_trade_types_request:
        'synthetic.v1.trade_generation_trade_types.put_many',
    delete_trade_generation_trade_type_request: 'synthetic.v1.trade_generation_trade_types.delete',
    delete_many_trade_generation_trade_types_request:
        'synthetic.v1.trade_generation_trade_types.delete_many',
    list_trade_generation_trade_type_versions_request:
        'synthetic.v1.trade_generation_trade_types_versions.list',
    get_trade_generation_trade_type_version_request:
        'synthetic.v1.trade_generation_trade_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_generation_trade_types_request: true,
    get_trade_generation_trade_type_request: true,
    get_many_trade_generation_trade_types_request: true,
    put_trade_generation_trade_type_request: true,
    put_many_trade_generation_trade_types_request: true,
    delete_trade_generation_trade_type_request: true,
    delete_many_trade_generation_trade_types_request: true,
    list_trade_generation_trade_type_versions_request: true,
    get_trade_generation_trade_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.trade_generation_trade_types.created',
    updated: 'synthetic.v1.events.trade_generation_trade_types.updated',
    deleted: 'synthetic.v1.events.trade_generation_trade_types.deleted',
} as const;
