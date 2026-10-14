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
import type { AssetMeasure } from '../domain/asset_measure.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AssetMeasureKey {
    code: string;
    coding_scheme_code: string;
}

export interface AssetMeasureWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface AssetMeasureChange {
    write: AssetMeasureWrite;
    precondition: Precondition;
}

export interface AssetMeasureRemoval {
    key: AssetMeasureKey;
    precondition: Precondition;
}

export interface AssetMeasureLookup {
    key: AssetMeasureKey;
    asset_measure: AssetMeasure | null;
}

export interface AssetMeasureEvent {
    event_id: string;
    key: AssetMeasureKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AssetMeasureVersionKey {
    asset_measure: AssetMeasureKey;
    version: number;
}

export interface AssetMeasureVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAssetMeasuresRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListAssetMeasuresResponse {
    result: Result;
    measures: AssetMeasure[];
    total: number;
}

export interface GetAssetMeasureRequest {
    key: AssetMeasureKey;
}

export interface GetAssetMeasureResponse {
    result: Result;
    asset_measure: AssetMeasure | null;
}

export interface GetManyAssetMeasuresRequest {
    keys: AssetMeasureKey[];
}

export interface GetManyAssetMeasuresResponse {
    result: Result;
    entries: AssetMeasureLookup[];
}

export interface PutAssetMeasureRequest {
    change: AssetMeasureChange;
    intent: ChangeIntent;
}

export interface PutAssetMeasureResponse {
    result: Result;
    asset_measure: AssetMeasure | null;
}

export interface PutManyAssetMeasuresRequest {
    changes: AssetMeasureChange[];
    intent: ChangeIntent;
}

export interface PutManyAssetMeasuresResponse {
    result: Result;
    measures: AssetMeasure[];
}

export interface DeleteAssetMeasureRequest {
    removal: AssetMeasureRemoval;
    intent: ChangeIntent;
}

export interface DeleteAssetMeasureResponse {
    result: Result;
}

export interface DeleteManyAssetMeasuresRequest {
    removals: AssetMeasureRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAssetMeasuresResponse {
    result: Result;
}

export interface ListAssetMeasureVersionsRequest {
    key: AssetMeasureKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AssetMeasureVersionsFilter | null;
}

export interface ListAssetMeasureVersionsResponse {
    result: Result;
    versions: AssetMeasure[];
    total: number;
}

export interface GetAssetMeasureVersionRequest {
    key: AssetMeasureVersionKey;
}

export interface GetAssetMeasureVersionResponse {
    result: Result;
    version: AssetMeasure | null;
}

export const subjects = {
    list_asset_measures_request: 'refdata.v1.asset_measures.list',
    get_asset_measure_request: 'refdata.v1.asset_measures.get',
    get_many_asset_measures_request: 'refdata.v1.asset_measures.get_many',
    put_asset_measure_request: 'refdata.v1.asset_measures.put',
    put_many_asset_measures_request: 'refdata.v1.asset_measures.put_many',
    delete_asset_measure_request: 'refdata.v1.asset_measures.delete',
    delete_many_asset_measures_request: 'refdata.v1.asset_measures.delete_many',
    list_asset_measure_versions_request: 'refdata.v1.asset_measures_versions.list',
    get_asset_measure_version_request: 'refdata.v1.asset_measures_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_asset_measures_request: true,
    get_asset_measure_request: true,
    get_many_asset_measures_request: true,
    put_asset_measure_request: true,
    put_many_asset_measures_request: true,
    delete_asset_measure_request: true,
    delete_many_asset_measures_request: true,
    list_asset_measure_versions_request: true,
    get_asset_measure_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.asset_measures_events.created',
    updated: 'refdata.v1.asset_measures_events.updated',
    deleted: 'refdata.v1.asset_measures_events.deleted',
} as const;
