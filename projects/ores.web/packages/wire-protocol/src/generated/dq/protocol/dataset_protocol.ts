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
import type { Dataset } from '../domain/dataset.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DatasetKey {
    code: string;
}

export interface DatasetWrite {
    id: string;
    code: string;
    catalog_name: string | null;
    subject_area_name: string;
    domain_name: string;
    coding_scheme_code: string | null;
    origin_code: string;
    nature_code: string;
    treatment_code: string;
    methodology_id: string | null;
    name: string;
    description: string;
    source_system_id: string;
    business_context: string;
    upstream_derivation_id: string | null;
    lineage_depth: number;
    as_of_date: string;
    ingestion_timestamp: string;
    license_info: string | null;
    artefact_type: string;
}

export interface DatasetChange {
    write: DatasetWrite;
    precondition: Precondition;
}

export interface DatasetRemoval {
    key: DatasetKey;
    precondition: Precondition;
}

export interface DatasetLookup {
    key: DatasetKey;
    dataset: Dataset | null;
}

export interface DatasetsFilter {
    id_one_of: string[] | null;
}

export interface DatasetEvent {
    event_id: string;
    key: DatasetKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DatasetVersionKey {
    dataset: DatasetKey;
    version: number;
}

export interface DatasetVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDatasetsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DatasetsFilter | null;
}

export interface ListDatasetsResponse {
    result: Result;
    datasets: Dataset[];
    total: number;
}

export interface GetDatasetRequest {
    key: DatasetKey;
}

export interface GetDatasetResponse {
    result: Result;
    dataset: Dataset | null;
}

export interface GetManyDatasetsRequest {
    keys: DatasetKey[];
}

export interface GetManyDatasetsResponse {
    result: Result;
    entries: DatasetLookup[];
}

export interface PutDatasetRequest {
    change: DatasetChange;
    intent: ChangeIntent;
}

export interface PutDatasetResponse {
    result: Result;
    dataset: Dataset | null;
}

export interface PutManyDatasetsRequest {
    changes: DatasetChange[];
    intent: ChangeIntent;
}

export interface PutManyDatasetsResponse {
    result: Result;
    datasets: Dataset[];
}

export interface DeleteDatasetRequest {
    removal: DatasetRemoval;
    intent: ChangeIntent;
}

export interface DeleteDatasetResponse {
    result: Result;
}

export interface DeleteManyDatasetsRequest {
    removals: DatasetRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDatasetsResponse {
    result: Result;
}

export interface ListDatasetVersionsRequest {
    key: DatasetKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DatasetVersionsFilter | null;
}

export interface ListDatasetVersionsResponse {
    result: Result;
    versions: Dataset[];
    total: number;
}

export interface GetDatasetVersionRequest {
    key: DatasetVersionKey;
}

export interface GetDatasetVersionResponse {
    result: Result;
    version: Dataset | null;
}

export const subjects = {
    list_datasets_request: 'dq.v1.datasets.list',
    get_dataset_request: 'dq.v1.datasets.get',
    get_many_datasets_request: 'dq.v1.datasets.get_many',
    put_dataset_request: 'dq.v1.datasets.put',
    put_many_datasets_request: 'dq.v1.datasets.put_many',
    delete_dataset_request: 'dq.v1.datasets.delete',
    delete_many_datasets_request: 'dq.v1.datasets.delete_many',
    list_dataset_versions_request: 'dq.v1.datasets_versions.list',
    get_dataset_version_request: 'dq.v1.datasets_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_datasets_request: true,
    get_dataset_request: true,
    get_many_datasets_request: true,
    put_dataset_request: true,
    put_many_datasets_request: true,
    delete_dataset_request: true,
    delete_many_datasets_request: true,
    list_dataset_versions_request: true,
    get_dataset_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.datasets_events.created',
    updated: 'dq.v1.datasets_events.updated',
    deleted: 'dq.v1.datasets_events.deleted',
} as const;
