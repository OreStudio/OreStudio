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
import type { TradeAuthorisationRequest } from '../domain/trade_authorisation_request.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TradeAuthorisationRequestKey {
    request_id: string;
}

export interface TradeAuthorisationRequestWrite {
    request_id: string;
    trade_id: string;
    trade_version: number;
}

export interface TradeAuthorisationRequestChange {
    write: TradeAuthorisationRequestWrite;
    precondition: Precondition;
}

export interface TradeAuthorisationRequestRemoval {
    key: TradeAuthorisationRequestKey;
    precondition: Precondition;
}

export interface TradeAuthorisationRequestLookup {
    key: TradeAuthorisationRequestKey;
    trade_authorisation_request: TradeAuthorisationRequest | null;
}

export interface TradeAuthorisationRequestsFilter {
    trade_id: string | null;
    request_id_one_of: string[] | null;
    trade_id_one_of: string[] | null;
}

export interface TradeAuthorisationRequestEvent {
    event_id: string;
    key: TradeAuthorisationRequestKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeAuthorisationRequestVersionKey {
    trade_authorisation_request: TradeAuthorisationRequestKey;
    version: number;
}

export interface TradeAuthorisationRequestVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeAuthorisationRequestsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeAuthorisationRequestsFilter | null;
    as_of: string | null;
}

export interface ListTradeAuthorisationRequestsResponse {
    result: Result;
    trade_authorisation_requests: TradeAuthorisationRequest[];
    total: number;
}

export interface GetTradeAuthorisationRequestRequest {
    key: TradeAuthorisationRequestKey;
}

export interface GetTradeAuthorisationRequestResponse {
    result: Result;
    trade_authorisation_request: TradeAuthorisationRequest | null;
}

export interface GetManyTradeAuthorisationRequestsRequest {
    keys: TradeAuthorisationRequestKey[];
}

export interface GetManyTradeAuthorisationRequestsResponse {
    result: Result;
    entries: TradeAuthorisationRequestLookup[];
}

export interface PutTradeAuthorisationRequestRequest {
    change: TradeAuthorisationRequestChange;
    intent: ChangeIntent;
}

export interface PutTradeAuthorisationRequestResponse {
    result: Result;
    trade_authorisation_request: TradeAuthorisationRequest | null;
}

export interface PutManyTradeAuthorisationRequestsRequest {
    changes: TradeAuthorisationRequestChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeAuthorisationRequestsResponse {
    result: Result;
    trade_authorisation_requests: TradeAuthorisationRequest[];
}

export interface DeleteTradeAuthorisationRequestRequest {
    removal: TradeAuthorisationRequestRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeAuthorisationRequestResponse {
    result: Result;
}

export interface DeleteManyTradeAuthorisationRequestsRequest {
    removals: TradeAuthorisationRequestRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeAuthorisationRequestsResponse {
    result: Result;
}

export interface ListByTradeIdTradeAuthorisationRequestsRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeAuthorisationRequestsFilter | null;
}

export interface ListByTradeIdTradeAuthorisationRequestsResponse {
    result: Result;
    trade_authorisation_requests: TradeAuthorisationRequest[];
    total: number;
}

export interface ListTradeAuthorisationRequestVersionsRequest {
    key: TradeAuthorisationRequestKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeAuthorisationRequestVersionsFilter | null;
}

export interface ListTradeAuthorisationRequestVersionsResponse {
    result: Result;
    versions: TradeAuthorisationRequest[];
    total: number;
}

export interface GetTradeAuthorisationRequestVersionRequest {
    key: TradeAuthorisationRequestVersionKey;
}

export interface GetTradeAuthorisationRequestVersionResponse {
    result: Result;
    version: TradeAuthorisationRequest | null;
}

export const subjects = {
    list_trade_authorisation_requests_request: 'trading.v1.trade_authorisation_requests.list',
    get_trade_authorisation_request_request: 'trading.v1.trade_authorisation_requests.get',
    get_many_trade_authorisation_requests_request:
        'trading.v1.trade_authorisation_requests.get_many',
    put_trade_authorisation_request_request: 'trading.v1.trade_authorisation_requests.put',
    put_many_trade_authorisation_requests_request:
        'trading.v1.trade_authorisation_requests.put_many',
    delete_trade_authorisation_request_request: 'trading.v1.trade_authorisation_requests.delete',
    delete_many_trade_authorisation_requests_request:
        'trading.v1.trade_authorisation_requests.delete_many',
    list_by_trade_id_trade_authorisation_requests_request:
        'trading.v1.trade_authorisation_requests.list_by_trade_id',
    list_trade_authorisation_request_versions_request:
        'trading.v1.trade_authorisation_requests_versions.list',
    get_trade_authorisation_request_version_request:
        'trading.v1.trade_authorisation_requests_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_authorisation_requests_request: true,
    get_trade_authorisation_request_request: true,
    get_many_trade_authorisation_requests_request: true,
    put_trade_authorisation_request_request: true,
    put_many_trade_authorisation_requests_request: true,
    delete_trade_authorisation_request_request: true,
    delete_many_trade_authorisation_requests_request: true,
    list_by_trade_id_trade_authorisation_requests_request: true,
    list_trade_authorisation_request_versions_request: true,
    get_trade_authorisation_request_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_authorisation_requests_events.created',
    updated: 'trading.v1.trade_authorisation_requests_events.updated',
    deleted: 'trading.v1.trade_authorisation_requests_events.deleted',
} as const;
