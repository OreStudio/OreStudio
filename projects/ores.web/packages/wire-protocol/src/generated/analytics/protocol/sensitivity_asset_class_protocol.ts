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
import type { SensitivityAssetClass } from '../domain/sensitivity_asset_class.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SensitivityAssetClassKey {
    asset_class_code: string;
}

export interface SensitivityAssetClassWrite {
    asset_class_code: string;
    template_prefix: string;
    name: string;
    description: string;
    display_order: number;
}

export interface SensitivityAssetClassChange {
    write: SensitivityAssetClassWrite;
    precondition: Precondition;
}

export interface SensitivityAssetClassRemoval {
    key: SensitivityAssetClassKey;
    precondition: Precondition;
}

export interface SensitivityAssetClassLookup {
    key: SensitivityAssetClassKey;
    sensitivity_asset_class: SensitivityAssetClass | null;
}

export interface SensitivityAssetClassesFilter {
    asset_class_code_one_of: string[] | null;
}

export interface SensitivityAssetClassEvent {
    event_id: string;
    key: SensitivityAssetClassKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SensitivityAssetClassVersionKey {
    sensitivity_asset_class: SensitivityAssetClassKey;
    version: number;
}

export interface SensitivityAssetClassVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSensitivityAssetClassesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SensitivityAssetClassesFilter | null;
    as_of: string | null;
}

export interface ListSensitivityAssetClassesResponse {
    result: Result;
    asset_classes: SensitivityAssetClass[];
    total: number;
}

export interface GetSensitivityAssetClassRequest {
    key: SensitivityAssetClassKey;
}

export interface GetSensitivityAssetClassResponse {
    result: Result;
    sensitivity_asset_class: SensitivityAssetClass | null;
}

export interface GetManySensitivityAssetClassesRequest {
    keys: SensitivityAssetClassKey[];
}

export interface GetManySensitivityAssetClassesResponse {
    result: Result;
    entries: SensitivityAssetClassLookup[];
}

export interface PutSensitivityAssetClassRequest {
    change: SensitivityAssetClassChange;
    intent: ChangeIntent;
}

export interface PutSensitivityAssetClassResponse {
    result: Result;
    sensitivity_asset_class: SensitivityAssetClass | null;
}

export interface PutManySensitivityAssetClassesRequest {
    changes: SensitivityAssetClassChange[];
    intent: ChangeIntent;
}

export interface PutManySensitivityAssetClassesResponse {
    result: Result;
    asset_classes: SensitivityAssetClass[];
}

export interface DeleteSensitivityAssetClassRequest {
    removal: SensitivityAssetClassRemoval;
    intent: ChangeIntent;
}

export interface DeleteSensitivityAssetClassResponse {
    result: Result;
}

export interface DeleteManySensitivityAssetClassesRequest {
    removals: SensitivityAssetClassRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySensitivityAssetClassesResponse {
    result: Result;
}

export interface ListSensitivityAssetClassVersionsRequest {
    key: SensitivityAssetClassKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SensitivityAssetClassVersionsFilter | null;
}

export interface ListSensitivityAssetClassVersionsResponse {
    result: Result;
    versions: SensitivityAssetClass[];
    total: number;
}

export interface GetSensitivityAssetClassVersionRequest {
    key: SensitivityAssetClassVersionKey;
}

export interface GetSensitivityAssetClassVersionResponse {
    result: Result;
    version: SensitivityAssetClass | null;
}

export const subjects = {
    list_sensitivity_asset_classes_request: 'analytics.v1.sensitivity_asset_classes.list',
    get_sensitivity_asset_class_request: 'analytics.v1.sensitivity_asset_classes.get',
    get_many_sensitivity_asset_classes_request: 'analytics.v1.sensitivity_asset_classes.get_many',
    put_sensitivity_asset_class_request: 'analytics.v1.sensitivity_asset_classes.put',
    put_many_sensitivity_asset_classes_request: 'analytics.v1.sensitivity_asset_classes.put_many',
    delete_sensitivity_asset_class_request: 'analytics.v1.sensitivity_asset_classes.delete',
    delete_many_sensitivity_asset_classes_request:
        'analytics.v1.sensitivity_asset_classes.delete_many',
    list_sensitivity_asset_class_versions_request:
        'analytics.v1.sensitivity_asset_classes_versions.list',
    get_sensitivity_asset_class_version_request:
        'analytics.v1.sensitivity_asset_classes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_sensitivity_asset_classes_request: true,
    get_sensitivity_asset_class_request: true,
    get_many_sensitivity_asset_classes_request: true,
    put_sensitivity_asset_class_request: true,
    put_many_sensitivity_asset_classes_request: true,
    delete_sensitivity_asset_class_request: true,
    delete_many_sensitivity_asset_classes_request: true,
    list_sensitivity_asset_class_versions_request: true,
    get_sensitivity_asset_class_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'analytics.v1.sensitivity_asset_classes_events.created',
    updated: 'analytics.v1.sensitivity_asset_classes_events.updated',
    deleted: 'analytics.v1.sensitivity_asset_classes_events.deleted',
} as const;
