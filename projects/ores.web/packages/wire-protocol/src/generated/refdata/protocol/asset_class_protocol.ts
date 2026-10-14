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
import type { AssetClass } from '../domain/asset_class.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AssetClassKey {
    code: string;
    coding_scheme_code: string;
}

export interface AssetClassWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface AssetClassChange {
    write: AssetClassWrite;
    precondition: Precondition;
}

export interface AssetClassRemoval {
    key: AssetClassKey;
    precondition: Precondition;
}

export interface AssetClassLookup {
    key: AssetClassKey;
    asset_class: AssetClass | null;
}

export interface AssetClassEvent {
    event_id: string;
    key: AssetClassKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AssetClassVersionKey {
    asset_class: AssetClassKey;
    version: number;
}

export interface AssetClassVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAssetClassesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListAssetClassesResponse {
    result: Result;
    classes: AssetClass[];
    total: number;
}

export interface GetAssetClassRequest {
    key: AssetClassKey;
}

export interface GetAssetClassResponse {
    result: Result;
    asset_class: AssetClass | null;
}

export interface GetManyAssetClassesRequest {
    keys: AssetClassKey[];
}

export interface GetManyAssetClassesResponse {
    result: Result;
    entries: AssetClassLookup[];
}

export interface PutAssetClassRequest {
    change: AssetClassChange;
    intent: ChangeIntent;
}

export interface PutAssetClassResponse {
    result: Result;
    asset_class: AssetClass | null;
}

export interface PutManyAssetClassesRequest {
    changes: AssetClassChange[];
    intent: ChangeIntent;
}

export interface PutManyAssetClassesResponse {
    result: Result;
    classes: AssetClass[];
}

export interface DeleteAssetClassRequest {
    removal: AssetClassRemoval;
    intent: ChangeIntent;
}

export interface DeleteAssetClassResponse {
    result: Result;
}

export interface DeleteManyAssetClassesRequest {
    removals: AssetClassRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAssetClassesResponse {
    result: Result;
}

export interface ListAssetClassVersionsRequest {
    key: AssetClassKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AssetClassVersionsFilter | null;
}

export interface ListAssetClassVersionsResponse {
    result: Result;
    versions: AssetClass[];
    total: number;
}

export interface GetAssetClassVersionRequest {
    key: AssetClassVersionKey;
}

export interface GetAssetClassVersionResponse {
    result: Result;
    version: AssetClass | null;
}

export const subjects = {
    list_asset_classes_request: 'refdata.v1.asset_classes.list',
    get_asset_class_request: 'refdata.v1.asset_classes.get',
    get_many_asset_classes_request: 'refdata.v1.asset_classes.get_many',
    put_asset_class_request: 'refdata.v1.asset_classes.put',
    put_many_asset_classes_request: 'refdata.v1.asset_classes.put_many',
    delete_asset_class_request: 'refdata.v1.asset_classes.delete',
    delete_many_asset_classes_request: 'refdata.v1.asset_classes.delete_many',
    list_asset_class_versions_request: 'refdata.v1.asset_classes_versions.list',
    get_asset_class_version_request: 'refdata.v1.asset_classes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_asset_classes_request: true,
    get_asset_class_request: true,
    get_many_asset_classes_request: true,
    put_asset_class_request: true,
    put_many_asset_classes_request: true,
    delete_asset_class_request: true,
    delete_many_asset_classes_request: true,
    list_asset_class_versions_request: true,
    get_asset_class_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.asset_classes_events.created',
    updated: 'refdata.v1.asset_classes_events.updated',
    deleted: 'refdata.v1.asset_classes_events.deleted',
} as const;
