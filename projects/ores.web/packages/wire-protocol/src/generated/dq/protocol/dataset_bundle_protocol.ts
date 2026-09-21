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

export interface GetDatasetBundlesRequest {
    offset: number;
    limit: number;
}

export interface GetDatasetBundlesResponse {
    bundles: DatasetBundle[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveDatasetBundleRequest {
    data: DatasetBundle;
}

export interface SaveDatasetBundleResponse {
    success: boolean;
    message: string;
}

export interface DeleteDatasetBundleRequest {
    ids: string[];
}

export interface DeleteDatasetBundleResponse {
    success: boolean;
    message: string;
}

export interface GetDatasetBundleHistoryRequest {
    id: string;
}

export interface GetDatasetBundleHistoryResponse {
    history: DatasetBundle[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_dataset_bundles_request: "dq.v1.dataset_bundles.list",
    save_dataset_bundle_request: "dq.v1.dataset_bundles.save",
    delete_dataset_bundle_request: "dq.v1.dataset_bundles.delete",
    get_dataset_bundle_history_request: "dq.v1.dataset_bundles.history",
} as const;
