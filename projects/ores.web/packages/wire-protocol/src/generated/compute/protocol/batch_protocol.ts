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
import type { Batch } from '../domain/batch.js';

export interface GetBatchesRequest {
    offset: number;
    limit: number;
}

export interface GetBatchesResponse {
    batches: Batch[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveBatchRequest {
    data: Batch;
}

export interface SaveBatchResponse {
    success: boolean;
    message: string;
}

export interface DeleteBatchRequest {
    ids: string[];
}

export interface DeleteBatchResponse {
    success: boolean;
    message: string;
}

export interface GetBatchHistoryRequest {
    id: string;
}

export interface GetBatchHistoryResponse {
    history: Batch[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_batches_request: "compute.v1.batches.list",
    save_batch_request: "compute.v1.batches.save",
    delete_batch_request: "compute.v1.batches.delete",
    get_batch_history_request: "compute.v1.batches.history",
} as const;
