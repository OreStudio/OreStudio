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
import type { EquityBasketConstituent } from '../domain/equity_basket_constituent.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EquityBasketConstituentKey {
    trade_id: string;
    sequence_number: number;
}

export interface EquityBasketConstituentWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    underlying_name: string;
}

export interface EquityBasketConstituentChange {
    write: EquityBasketConstituentWrite;
    precondition: Precondition;
}

export interface EquityBasketConstituentRemoval {
    key: EquityBasketConstituentKey;
    precondition: Precondition;
}

export interface EquityBasketConstituentLookup {
    key: EquityBasketConstituentKey;
    equity_basket_constituent: EquityBasketConstituent | null;
}

export interface EquityBasketConstituentEvent {
    event_id: string;
    key: EquityBasketConstituentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EquityBasketConstituentVersionKey {
    equity_basket_constituent: EquityBasketConstituentKey;
    version: number;
}

export interface EquityBasketConstituentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEquityBasketConstituentsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListEquityBasketConstituentsResponse {
    result: Result;
    equity_basket_constituents: EquityBasketConstituent[];
    total: number;
}

export interface GetEquityBasketConstituentRequest {
    key: EquityBasketConstituentKey;
}

export interface GetEquityBasketConstituentResponse {
    result: Result;
    equity_basket_constituent: EquityBasketConstituent | null;
}

export interface GetManyEquityBasketConstituentsRequest {
    keys: EquityBasketConstituentKey[];
}

export interface GetManyEquityBasketConstituentsResponse {
    result: Result;
    entries: EquityBasketConstituentLookup[];
}

export interface PutEquityBasketConstituentRequest {
    change: EquityBasketConstituentChange;
    intent: ChangeIntent;
}

export interface PutEquityBasketConstituentResponse {
    result: Result;
    equity_basket_constituent: EquityBasketConstituent | null;
}

export interface PutManyEquityBasketConstituentsRequest {
    changes: EquityBasketConstituentChange[];
    intent: ChangeIntent;
}

export interface PutManyEquityBasketConstituentsResponse {
    result: Result;
    equity_basket_constituents: EquityBasketConstituent[];
}

export interface DeleteEquityBasketConstituentRequest {
    removal: EquityBasketConstituentRemoval;
    intent: ChangeIntent;
}

export interface DeleteEquityBasketConstituentResponse {
    result: Result;
}

export interface DeleteManyEquityBasketConstituentsRequest {
    removals: EquityBasketConstituentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEquityBasketConstituentsResponse {
    result: Result;
}

export interface ListEquityBasketConstituentVersionsRequest {
    key: EquityBasketConstituentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EquityBasketConstituentVersionsFilter | null;
}

export interface ListEquityBasketConstituentVersionsResponse {
    result: Result;
    versions: EquityBasketConstituent[];
    total: number;
}

export interface GetEquityBasketConstituentVersionRequest {
    key: EquityBasketConstituentVersionKey;
}

export interface GetEquityBasketConstituentVersionResponse {
    result: Result;
    version: EquityBasketConstituent | null;
}

export const subjects = {
    list_equity_basket_constituents_request: 'trading.v1.equity_basket_constituents.list',
    get_equity_basket_constituent_request: 'trading.v1.equity_basket_constituents.get',
    get_many_equity_basket_constituents_request: 'trading.v1.equity_basket_constituents.get_many',
    put_equity_basket_constituent_request: 'trading.v1.equity_basket_constituents.put',
    put_many_equity_basket_constituents_request: 'trading.v1.equity_basket_constituents.put_many',
    delete_equity_basket_constituent_request: 'trading.v1.equity_basket_constituents.delete',
    delete_many_equity_basket_constituents_request:
        'trading.v1.equity_basket_constituents.delete_many',
    list_equity_basket_constituent_versions_request:
        'trading.v1.equity_basket_constituents_versions.list',
    get_equity_basket_constituent_version_request:
        'trading.v1.equity_basket_constituents_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_equity_basket_constituents_request: true,
    get_equity_basket_constituent_request: true,
    get_many_equity_basket_constituents_request: true,
    put_equity_basket_constituent_request: true,
    put_many_equity_basket_constituents_request: true,
    delete_equity_basket_constituent_request: true,
    delete_many_equity_basket_constituents_request: true,
    list_equity_basket_constituent_versions_request: true,
    get_equity_basket_constituent_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.equity_basket_constituents_events.created',
    updated: 'trading.v1.equity_basket_constituents_events.updated',
    deleted: 'trading.v1.equity_basket_constituents_events.deleted',
} as const;
