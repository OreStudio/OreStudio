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
import type { MarketFixing } from '../domain/market_fixing.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketFixingKey {
    id: string;
}

export interface MarketFixingWrite {
    id: string;
    party_id: string;
    series_id: string;
    fixing_date: string;
    value: string;
    source: string;
}

export interface MarketFixingChange {
    write: MarketFixingWrite;
    precondition: Precondition;
}

export interface MarketFixingRemoval {
    key: MarketFixingKey;
    precondition: Precondition;
}

export interface MarketFixingLookup {
    key: MarketFixingKey;
    market_fixing: MarketFixing | null;
}

export interface MarketFixingsFilter {
    id_one_of: string[] | null;
}

export interface MarketFixingEvent {
    event_id: string;
    key: MarketFixingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListMarketFixingsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketFixingsFilter | null;
    as_of: string | null;
}

export interface ListMarketFixingsResponse {
    result: Result;
    market_fixings: MarketFixing[];
    total: number;
}

export interface GetMarketFixingRequest {
    key: MarketFixingKey;
}

export interface GetMarketFixingResponse {
    result: Result;
    market_fixing: MarketFixing | null;
}

export interface GetManyMarketFixingsRequest {
    keys: MarketFixingKey[];
}

export interface GetManyMarketFixingsResponse {
    result: Result;
    entries: MarketFixingLookup[];
}

export interface PutMarketFixingRequest {
    change: MarketFixingChange;
    intent: ChangeIntent;
}

export interface PutMarketFixingResponse {
    result: Result;
    market_fixing: MarketFixing | null;
}

export interface PutManyMarketFixingsRequest {
    changes: MarketFixingChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketFixingsResponse {
    result: Result;
    market_fixings: MarketFixing[];
}

export interface DeleteMarketFixingRequest {
    removal: MarketFixingRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketFixingResponse {
    result: Result;
}

export interface DeleteManyMarketFixingsRequest {
    removals: MarketFixingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketFixingsResponse {
    result: Result;
}

export const subjects = {
    list_market_fixings_request: 'marketdata.v1.market_fixings.list',
    get_market_fixing_request: 'marketdata.v1.market_fixings.get',
    get_many_market_fixings_request: 'marketdata.v1.market_fixings.get_many',
    put_market_fixing_request: 'marketdata.v1.market_fixings.put',
    put_many_market_fixings_request: 'marketdata.v1.market_fixings.put_many',
    delete_market_fixing_request: 'marketdata.v1.market_fixings.delete',
    delete_many_market_fixings_request: 'marketdata.v1.market_fixings.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_fixings_request: true,
    get_market_fixing_request: true,
    get_many_market_fixings_request: true,
    put_market_fixing_request: true,
    put_many_market_fixings_request: true,
    delete_market_fixing_request: true,
    delete_many_market_fixings_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.market_fixings_events.created',
    updated: 'marketdata.v1.market_fixings_events.updated',
    deleted: 'marketdata.v1.market_fixings_events.deleted',
} as const;
