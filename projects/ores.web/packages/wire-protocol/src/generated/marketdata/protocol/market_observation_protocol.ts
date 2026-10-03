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
import type { MarketObservation } from '../domain/market_observation.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface MarketObservationKey {
    id: string;
}

export interface MarketObservationWrite {
    id: string;
    party_id: string;
    series_id: string;
    observation_datetime: string;
    oresmd_uri: string;
    value: string;
    source: string;
}

export interface MarketObservationChange {
    write: MarketObservationWrite;
    precondition: Precondition;
}

export interface MarketObservationRemoval {
    key: MarketObservationKey;
    precondition: Precondition;
}

export interface MarketObservationLookup {
    key: MarketObservationKey;
    market_observation: MarketObservation | null;
}

export interface MarketObservationsFilter {
    series_id: string | null;
}

export interface MarketObservationEvent {
    event_id: string;
    key: MarketObservationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListMarketObservationsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketObservationsFilter | null;
}

export interface ListMarketObservationsResponse {
    result: Result;
    market_observations: MarketObservation[];
    total: number;
}

export interface GetMarketObservationRequest {
    key: MarketObservationKey;
}

export interface GetMarketObservationResponse {
    result: Result;
    market_observation: MarketObservation | null;
}

export interface GetManyMarketObservationsRequest {
    keys: MarketObservationKey[];
}

export interface GetManyMarketObservationsResponse {
    result: Result;
    entries: MarketObservationLookup[];
}

export interface PutMarketObservationRequest {
    change: MarketObservationChange;
    intent: ChangeIntent;
}

export interface PutMarketObservationResponse {
    result: Result;
    market_observation: MarketObservation | null;
}

export interface PutManyMarketObservationsRequest {
    changes: MarketObservationChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketObservationsResponse {
    result: Result;
    market_observations: MarketObservation[];
}

export interface DeleteMarketObservationRequest {
    removal: MarketObservationRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketObservationResponse {
    result: Result;
}

export interface DeleteManyMarketObservationsRequest {
    removals: MarketObservationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketObservationsResponse {
    result: Result;
}

export interface ListBySeriesIdMarketObservationsRequest {
    series_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: MarketObservationsFilter | null;
}

export interface ListBySeriesIdMarketObservationsResponse {
    result: Result;
    market_observations: MarketObservation[];
    total: number;
}

export const subjects = {
    list_market_observations_request: 'marketdata.v1.market_observations.list',
    get_market_observation_request: 'marketdata.v1.market_observations.get',
    get_many_market_observations_request: 'marketdata.v1.market_observations.get_many',
    put_market_observation_request: 'marketdata.v1.market_observations.put',
    put_many_market_observations_request: 'marketdata.v1.market_observations.put_many',
    delete_market_observation_request: 'marketdata.v1.market_observations.delete',
    delete_many_market_observations_request: 'marketdata.v1.market_observations.delete_many',
    list_by_series_id_market_observations_request:
        'marketdata.v1.market_observations.list_by_series_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_observations_request: true,
    get_market_observation_request: true,
    get_many_market_observations_request: true,
    put_market_observation_request: true,
    put_many_market_observations_request: true,
    delete_market_observation_request: true,
    delete_many_market_observations_request: true,
    list_by_series_id_market_observations_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.market_observations_events.created',
    updated: 'marketdata.v1.market_observations_events.updated',
    deleted: 'marketdata.v1.market_observations_events.deleted',
} as const;
