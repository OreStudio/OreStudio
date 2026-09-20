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
import type { Result } from '../domain/result.js';

export interface GetResultsRequest {
    offset: number;
    limit: number;
}

export interface GetResultsResponse {
    results: Result[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveResultRequest {
    data: Result;
}

export interface SaveResultResponse {
    success: boolean;
    message: string;
}

export interface DeleteResultRequest {
    ids: string[];
}

export interface DeleteResultResponse {
    success: boolean;
    message: string;
}

export interface GetResultHistoryRequest {
    id: string;
}

export interface GetResultHistoryResponse {
    history: Result[];
    success: boolean;
    message: string;
}

export interface GetResultsByWorkunitIdRequest {
    workunit_id: string;
    offset: number;
    limit: number;
}

export interface GetResultsByWorkunitIdResponse {
    results: Result[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_results_request: "compute.v1.results.list",
    save_result_request: "compute.v1.results.save",
    delete_result_request: "compute.v1.results.delete",
    get_result_history_request: "compute.v1.results.history",
    get_results_by_workunit_id_request: "compute.v1.results.list_by_workunit_id",
} as const;
