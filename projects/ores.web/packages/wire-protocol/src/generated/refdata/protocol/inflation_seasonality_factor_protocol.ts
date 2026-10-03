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
import type { InflationSeasonalityFactor } from '../domain/inflation_seasonality_factor.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InflationSeasonalityFactorKey {
    id: string;
}

export interface InflationSeasonalityFactorWrite {
    id: string;
    curve_definition_id: string;
    factor: string;
    position: number;
}

export interface InflationSeasonalityFactorChange {
    write: InflationSeasonalityFactorWrite;
    precondition: Precondition;
}

export interface InflationSeasonalityFactorRemoval {
    key: InflationSeasonalityFactorKey;
    precondition: Precondition;
}

export interface InflationSeasonalityFactorLookup {
    key: InflationSeasonalityFactorKey;
    inflation_seasonality_factor: InflationSeasonalityFactor | null;
}

export interface InflationSeasonalityFactorEvent {
    event_id: string;
    key: InflationSeasonalityFactorKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InflationSeasonalityFactorVersionKey {
    inflation_seasonality_factor: InflationSeasonalityFactorKey;
    version: number;
}

export interface InflationSeasonalityFactorVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInflationSeasonalityFactorsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListInflationSeasonalityFactorsResponse {
    result: Result;
    seasonality_factors: InflationSeasonalityFactor[];
    total: number;
}

export interface GetInflationSeasonalityFactorRequest {
    key: InflationSeasonalityFactorKey;
}

export interface GetInflationSeasonalityFactorResponse {
    result: Result;
    inflation_seasonality_factor: InflationSeasonalityFactor | null;
}

export interface GetManyInflationSeasonalityFactorsRequest {
    keys: InflationSeasonalityFactorKey[];
}

export interface GetManyInflationSeasonalityFactorsResponse {
    result: Result;
    entries: InflationSeasonalityFactorLookup[];
}

export interface PutInflationSeasonalityFactorRequest {
    change: InflationSeasonalityFactorChange;
    intent: ChangeIntent;
}

export interface PutInflationSeasonalityFactorResponse {
    result: Result;
    inflation_seasonality_factor: InflationSeasonalityFactor | null;
}

export interface PutManyInflationSeasonalityFactorsRequest {
    changes: InflationSeasonalityFactorChange[];
    intent: ChangeIntent;
}

export interface PutManyInflationSeasonalityFactorsResponse {
    result: Result;
    seasonality_factors: InflationSeasonalityFactor[];
}

export interface DeleteInflationSeasonalityFactorRequest {
    removal: InflationSeasonalityFactorRemoval;
    intent: ChangeIntent;
}

export interface DeleteInflationSeasonalityFactorResponse {
    result: Result;
}

export interface DeleteManyInflationSeasonalityFactorsRequest {
    removals: InflationSeasonalityFactorRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInflationSeasonalityFactorsResponse {
    result: Result;
}

export interface ListInflationSeasonalityFactorVersionsRequest {
    key: InflationSeasonalityFactorKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InflationSeasonalityFactorVersionsFilter | null;
}

export interface ListInflationSeasonalityFactorVersionsResponse {
    result: Result;
    versions: InflationSeasonalityFactor[];
    total: number;
}

export interface GetInflationSeasonalityFactorVersionRequest {
    key: InflationSeasonalityFactorVersionKey;
}

export interface GetInflationSeasonalityFactorVersionResponse {
    result: Result;
    version: InflationSeasonalityFactor | null;
}

export const subjects = {
    list_inflation_seasonality_factors_request: 'refdata.v1.inflation_seasonality_factors.list',
    get_inflation_seasonality_factor_request: 'refdata.v1.inflation_seasonality_factors.get',
    get_many_inflation_seasonality_factors_request:
        'refdata.v1.inflation_seasonality_factors.get_many',
    put_inflation_seasonality_factor_request: 'refdata.v1.inflation_seasonality_factors.put',
    put_many_inflation_seasonality_factors_request:
        'refdata.v1.inflation_seasonality_factors.put_many',
    delete_inflation_seasonality_factor_request: 'refdata.v1.inflation_seasonality_factors.delete',
    delete_many_inflation_seasonality_factors_request:
        'refdata.v1.inflation_seasonality_factors.delete_many',
    list_inflation_seasonality_factor_versions_request:
        'refdata.v1.inflation_seasonality_factors_versions.list',
    get_inflation_seasonality_factor_version_request:
        'refdata.v1.inflation_seasonality_factors_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_inflation_seasonality_factors_request: true,
    get_inflation_seasonality_factor_request: true,
    get_many_inflation_seasonality_factors_request: true,
    put_inflation_seasonality_factor_request: true,
    put_many_inflation_seasonality_factors_request: true,
    delete_inflation_seasonality_factor_request: true,
    delete_many_inflation_seasonality_factors_request: true,
    list_inflation_seasonality_factor_versions_request: true,
    get_inflation_seasonality_factor_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.inflation_seasonality_factors_events.created',
    updated: 'refdata.v1.inflation_seasonality_factors_events.updated',
    deleted: 'refdata.v1.inflation_seasonality_factors_events.deleted',
} as const;
