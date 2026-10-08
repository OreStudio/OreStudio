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
import type { TradeLinkType } from '../domain/trade_link_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeLinkTypeKey {
    code: string;
}

export interface TradeLinkTypeWrite {
    code: string;
    description: string;
    from_role: string;
    to_role: string;
    has_economic_effect: boolean;
}

export interface TradeLinkTypeChange {
    write: TradeLinkTypeWrite;
    precondition: Precondition;
}

export interface TradeLinkTypeRemoval {
    key: TradeLinkTypeKey;
    precondition: Precondition;
}

export interface TradeLinkTypeLookup {
    key: TradeLinkTypeKey;
    trade_link_type: TradeLinkType | null;
}

export interface TradeLinkTypesFilter {
    code_one_of: string[] | null;
}

export interface TradeLinkTypeEvent {
    event_id: string;
    key: TradeLinkTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeLinkTypeVersionKey {
    trade_link_type: TradeLinkTypeKey;
    version: number;
}

export interface TradeLinkTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeLinkTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeLinkTypesFilter | null;
    as_of: string | null;
}

export interface ListTradeLinkTypesResponse {
    result: Result;
    link_types: TradeLinkType[];
    total: number;
}

export interface GetTradeLinkTypeRequest {
    key: TradeLinkTypeKey;
}

export interface GetTradeLinkTypeResponse {
    result: Result;
    trade_link_type: TradeLinkType | null;
}

export interface GetManyTradeLinkTypesRequest {
    keys: TradeLinkTypeKey[];
}

export interface GetManyTradeLinkTypesResponse {
    result: Result;
    entries: TradeLinkTypeLookup[];
}

export interface PutTradeLinkTypeRequest {
    change: TradeLinkTypeChange;
    intent: ChangeIntent;
}

export interface PutTradeLinkTypeResponse {
    result: Result;
    trade_link_type: TradeLinkType | null;
}

export interface PutManyTradeLinkTypesRequest {
    changes: TradeLinkTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeLinkTypesResponse {
    result: Result;
    link_types: TradeLinkType[];
}

export interface DeleteTradeLinkTypeRequest {
    removal: TradeLinkTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeLinkTypeResponse {
    result: Result;
}

export interface DeleteManyTradeLinkTypesRequest {
    removals: TradeLinkTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeLinkTypesResponse {
    result: Result;
}

export interface ListTradeLinkTypeVersionsRequest {
    key: TradeLinkTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeLinkTypeVersionsFilter | null;
}

export interface ListTradeLinkTypeVersionsResponse {
    result: Result;
    versions: TradeLinkType[];
    total: number;
}

export interface GetTradeLinkTypeVersionRequest {
    key: TradeLinkTypeVersionKey;
}

export interface GetTradeLinkTypeVersionResponse {
    result: Result;
    version: TradeLinkType | null;
}

export const subjects = {
    list_trade_link_types_request: 'trading.v1.trade_link_types.list',
    get_trade_link_type_request: 'trading.v1.trade_link_types.get',
    get_many_trade_link_types_request: 'trading.v1.trade_link_types.get_many',
    put_trade_link_type_request: 'trading.v1.trade_link_types.put',
    put_many_trade_link_types_request: 'trading.v1.trade_link_types.put_many',
    delete_trade_link_type_request: 'trading.v1.trade_link_types.delete',
    delete_many_trade_link_types_request: 'trading.v1.trade_link_types.delete_many',
    list_trade_link_type_versions_request: 'trading.v1.trade_link_types_versions.list',
    get_trade_link_type_version_request: 'trading.v1.trade_link_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_link_types_request: true,
    get_trade_link_type_request: true,
    get_many_trade_link_types_request: true,
    put_trade_link_type_request: true,
    put_many_trade_link_types_request: true,
    delete_trade_link_type_request: true,
    delete_many_trade_link_types_request: true,
    list_trade_link_type_versions_request: true,
    get_trade_link_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_link_types_events.created',
    updated: 'trading.v1.trade_link_types_events.updated',
    deleted: 'trading.v1.trade_link_types_events.deleted',
} as const;
