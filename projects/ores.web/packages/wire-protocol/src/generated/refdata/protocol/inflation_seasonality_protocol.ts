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
import type { InflationSeasonality } from '../domain/inflation_seasonality.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationSeasonalityKey {
    id: string;
}

export interface InflationSeasonalityWrite {
    id: string;
    curve_definition_id: string;
    base_date: string;
    frequency: string;
}

export interface InflationSeasonalityChange {
    write: InflationSeasonalityWrite;
    precondition: Precondition;
}

export interface InflationSeasonalityRemoval {
    key: InflationSeasonalityKey;
    precondition: Precondition;
}

export interface InflationSeasonalityLookup {
    key: InflationSeasonalityKey;
    inflation_seasonality: InflationSeasonality | null;
}

export interface InflationSeasonalitiesFilter {
    id_one_of: string[] | null;
}

export interface InflationSeasonalityEvent {
    event_id: string;
    key: InflationSeasonalityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationSeasonalityVersionKey {
    inflation_seasonality: InflationSeasonalityKey;
    version: number;
}

export interface InflationSeasonalityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationSeasonalitiesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InflationSeasonalitiesFilter | null;
    as_of: string | null;
}

export interface ListInflationSeasonalitiesResponse {
    result: Result;
    inflation_seasonalities: InflationSeasonality[];
    total: number;
}

export interface GetInflationSeasonalityRequest {
    key: InflationSeasonalityKey;
}

export interface GetInflationSeasonalityResponse {
    result: Result;
    inflation_seasonality: InflationSeasonality | null;
}

export interface GetManyInflationSeasonalitiesRequest {
    keys: InflationSeasonalityKey[];
}

export interface GetManyInflationSeasonalitiesResponse {
    result: Result;
    entries: InflationSeasonalityLookup[];
}

export interface PutInflationSeasonalityRequest {
    change: InflationSeasonalityChange;
    intent: ChangeIntent;
}

export interface PutInflationSeasonalityResponse {
    result: Result;
    inflation_seasonality: InflationSeasonality | null;
}

export interface PutManyInflationSeasonalitiesRequest {
    changes: InflationSeasonalityChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationSeasonalitiesResponse {
    result: Result;
    inflation_seasonalities: InflationSeasonality[];
}

export interface DeleteInflationSeasonalityRequest {
    removal: InflationSeasonalityRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationSeasonalityResponse {
    result: Result;
}

export interface DeleteManyInflationSeasonalitiesRequest {
    removals: InflationSeasonalityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationSeasonalitiesResponse {
    result: Result;
}

export interface ListInflationSeasonalityVersionsRequest {
    key: InflationSeasonalityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationSeasonalityVersionsFilter | null;
}

export interface ListInflationSeasonalityVersionsResponse {
    result: Result;
    versions: InflationSeasonality[];
    total: number;
}

export interface GetInflationSeasonalityVersionRequest {
    key: InflationSeasonalityVersionKey;
}

export interface GetInflationSeasonalityVersionResponse {
    result: Result;
    version: InflationSeasonality | null;
}

export const subjects = {
    list_inflation_seasonalities_request: 'refdata.v1.inflation_seasonalities.list',
    get_inflation_seasonality_request: 'refdata.v1.inflation_seasonalities.get',
    get_many_inflation_seasonalities_request: 'refdata.v1.inflation_seasonalities.get_many',
    put_inflation_seasonality_request: 'refdata.v1.inflation_seasonalities.put',
    put_many_inflation_seasonalities_request: 'refdata.v1.inflation_seasonalities.put_many',
    delete_inflation_seasonality_request: 'refdata.v1.inflation_seasonalities.delete',
    delete_many_inflation_seasonalities_request: 'refdata.v1.inflation_seasonalities.delete_many',
    list_inflation_seasonality_versions_request: 'refdata.v1.inflation_seasonalities_versions.list',
    get_inflation_seasonality_version_request: 'refdata.v1.inflation_seasonalities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_seasonalities_request: true,
    get_inflation_seasonality_request: true,
    get_many_inflation_seasonalities_request: true,
    put_inflation_seasonality_request: true,
    put_many_inflation_seasonalities_request: true,
    delete_inflation_seasonality_request: true,
    delete_many_inflation_seasonalities_request: true,
    list_inflation_seasonality_versions_request: true,
    get_inflation_seasonality_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.inflation_seasonalities_events.created',
    updated: 'refdata.v1.inflation_seasonalities_events.updated',
    deleted: 'refdata.v1.inflation_seasonalities_events.deleted',
} as const;
