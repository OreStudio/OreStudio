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
import type { DatasetBundle } from '../domain/dataset_bundle.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DatasetBundleKey {
    code: string;
}

export interface DatasetBundleWrite {
    id: string;
    code: string;
    name: string;
    description: string;
}

export interface DatasetBundleChange {
    write: DatasetBundleWrite;
    precondition: Precondition;
}

export interface DatasetBundleRemoval {
    key: DatasetBundleKey;
    precondition: Precondition;
}

export interface DatasetBundleLookup {
    key: DatasetBundleKey;
    dataset_bundle: DatasetBundle | null;
}

export interface DatasetBundleEvent {
    event_id: string;
    key: DatasetBundleKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DatasetBundleVersionKey {
    dataset_bundle: DatasetBundleKey;
    version: number;
}

export interface DatasetBundleVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDatasetBundlesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListDatasetBundlesResponse {
    result: Result;
    bundles: DatasetBundle[];
    total: number;
}

export interface GetDatasetBundleRequest {
    key: DatasetBundleKey;
}

export interface GetDatasetBundleResponse {
    result: Result;
    dataset_bundle: DatasetBundle | null;
}

export interface GetManyDatasetBundlesRequest {
    keys: DatasetBundleKey[];
}

export interface GetManyDatasetBundlesResponse {
    result: Result;
    entries: DatasetBundleLookup[];
}

export interface PutDatasetBundleRequest {
    change: DatasetBundleChange;
    intent: ChangeIntent;
}

export interface PutDatasetBundleResponse {
    result: Result;
    dataset_bundle: DatasetBundle;
}

export interface PutManyDatasetBundlesRequest {
    changes: DatasetBundleChange[];
    intent: ChangeIntent;
}

export interface PutManyDatasetBundlesResponse {
    result: Result;
    bundles: DatasetBundle[];
}

export interface DeleteDatasetBundleRequest {
    removal: DatasetBundleRemoval;
    intent: ChangeIntent;
}

export interface DeleteDatasetBundleResponse {
    result: Result;
}

export interface DeleteManyDatasetBundlesRequest {
    removals: DatasetBundleRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDatasetBundlesResponse {
    result: Result;
}

export interface ListDatasetBundleVersionsRequest {
    key: DatasetBundleKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DatasetBundleVersionsFilter | null;
}

export interface ListDatasetBundleVersionsResponse {
    result: Result;
    versions: DatasetBundle[];
    total: number;
}

export interface GetDatasetBundleVersionRequest {
    key: DatasetBundleVersionKey;
}

export interface GetDatasetBundleVersionResponse {
    result: Result;
    version: DatasetBundle;
}

export const subjects = {
    list_dataset_bundles_request: "dq.v1.dataset_bundles.list",
    get_dataset_bundle_request: "dq.v1.dataset_bundles.get",
    get_many_dataset_bundles_request: "dq.v1.dataset_bundles.get_many",
    put_dataset_bundle_request: "dq.v1.dataset_bundles.put",
    put_many_dataset_bundles_request: "dq.v1.dataset_bundles.put_many",
    delete_dataset_bundle_request: "dq.v1.dataset_bundles.delete",
    delete_many_dataset_bundles_request: "dq.v1.dataset_bundles.delete_many",
    list_dataset_bundle_versions_request: "dq.v1.dataset_bundles_versions.list",
    get_dataset_bundle_version_request: "dq.v1.dataset_bundles_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_dataset_bundles_request: true,
    get_dataset_bundle_request: true,
    get_many_dataset_bundles_request: true,
    put_dataset_bundle_request: true,
    put_many_dataset_bundles_request: true,
    delete_dataset_bundle_request: true,
    delete_many_dataset_bundles_request: true,
    list_dataset_bundle_versions_request: true,
    get_dataset_bundle_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.dataset_bundles_events.created",
    updated: "dq.v1.dataset_bundles_events.updated",
    deleted: "dq.v1.dataset_bundles_events.deleted",
} as const;
