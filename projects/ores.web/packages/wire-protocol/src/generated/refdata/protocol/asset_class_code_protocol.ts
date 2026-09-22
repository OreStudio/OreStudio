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
import type { AssetClassCode } from '../domain/asset_class_code.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AssetClassCodeKey {
    code: string;
}

export interface AssetClassCodeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface AssetClassCodeChange {
    write: AssetClassCodeWrite;
    precondition: Precondition;
}

export interface AssetClassCodeRemoval {
    key: AssetClassCodeKey;
    precondition: Precondition;
}

export interface AssetClassCodeLookup {
    key: AssetClassCodeKey;
    asset_class_code: AssetClassCode | null;
}

export interface AssetClassCodeEvent {
    event_id: string;
    key: AssetClassCodeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AssetClassCodeVersionKey {
    asset_class_code: AssetClassCodeKey;
    version: number;
}

export interface AssetClassCodeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAssetClassCodesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListAssetClassCodesResponse {
    result: Result;
    asset_classes: AssetClassCode[];
    total: number;
}

export interface GetAssetClassCodeRequest {
    key: AssetClassCodeKey;
}

export interface GetAssetClassCodeResponse {
    result: Result;
    asset_class_code: AssetClassCode | null;
}

export interface GetManyAssetClassCodesRequest {
    keys: AssetClassCodeKey[];
}

export interface GetManyAssetClassCodesResponse {
    result: Result;
    entries: AssetClassCodeLookup[];
}

export interface PutAssetClassCodeRequest {
    change: AssetClassCodeChange;
    intent: ChangeIntent;
}

export interface PutAssetClassCodeResponse {
    result: Result;
    asset_class_code: AssetClassCode;
}

export interface PutManyAssetClassCodesRequest {
    changes: AssetClassCodeChange[];
    intent: ChangeIntent;
}

export interface PutManyAssetClassCodesResponse {
    result: Result;
    asset_classes: AssetClassCode[];
}

export interface DeleteAssetClassCodeRequest {
    removal: AssetClassCodeRemoval;
    intent: ChangeIntent;
}

export interface DeleteAssetClassCodeResponse {
    result: Result;
}

export interface DeleteManyAssetClassCodesRequest {
    removals: AssetClassCodeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAssetClassCodesResponse {
    result: Result;
}

export interface ListAssetClassCodeVersionsRequest {
    key: AssetClassCodeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AssetClassCodeVersionsFilter | null;
}

export interface ListAssetClassCodeVersionsResponse {
    result: Result;
    versions: AssetClassCode[];
    total: number;
}

export interface GetAssetClassCodeVersionRequest {
    key: AssetClassCodeVersionKey;
}

export interface GetAssetClassCodeVersionResponse {
    result: Result;
    version: AssetClassCode;
}

export const subjects = {
    list_asset_class_codes_request: "refdata.v1.asset_class_codes.list",
    get_asset_class_code_request: "refdata.v1.asset_class_codes.get",
    get_many_asset_class_codes_request: "refdata.v1.asset_class_codes.get_many",
    put_asset_class_code_request: "refdata.v1.asset_class_codes.put",
    put_many_asset_class_codes_request: "refdata.v1.asset_class_codes.put_many",
    delete_asset_class_code_request: "refdata.v1.asset_class_codes.delete",
    delete_many_asset_class_codes_request: "refdata.v1.asset_class_codes.delete_many",
    list_asset_class_code_versions_request: "refdata.v1.asset_class_codes_versions.list",
    get_asset_class_code_version_request: "refdata.v1.asset_class_codes_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_asset_class_codes_request: true,
    get_asset_class_code_request: true,
    get_many_asset_class_codes_request: true,
    put_asset_class_code_request: true,
    put_many_asset_class_codes_request: true,
    delete_asset_class_code_request: true,
    delete_many_asset_class_codes_request: true,
    list_asset_class_code_versions_request: true,
    get_asset_class_code_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.asset_class_codes_events.created",
    updated: "refdata.v1.asset_class_codes_events.updated",
    deleted: "refdata.v1.asset_class_codes_events.deleted",
} as const;
