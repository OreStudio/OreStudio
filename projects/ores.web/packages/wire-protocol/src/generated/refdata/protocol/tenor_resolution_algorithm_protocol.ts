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
import type { TenorResolutionAlgorithm } from '../domain/tenor_resolution_algorithm.js';

export interface GetTenorResolutionAlgorithmsRequest {
    offset: number;
    limit: number;
}

export interface GetTenorResolutionAlgorithmsResponse {
    algorithms: TenorResolutionAlgorithm[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveTenorResolutionAlgorithmRequest {
    data: TenorResolutionAlgorithm;
}

export interface SaveTenorResolutionAlgorithmResponse {
    success: boolean;
    message: string;
}

export interface DeleteTenorResolutionAlgorithmRequest {
    codes: string[];
}

export interface DeleteTenorResolutionAlgorithmResponse {
    success: boolean;
    message: string;
}

export interface GetTenorResolutionAlgorithmHistoryRequest {
    code: string;
}

export interface GetTenorResolutionAlgorithmHistoryResponse {
    history: TenorResolutionAlgorithm[];
    success: boolean;
    message: string;
}

export const subjects = {
    get_tenor_resolution_algorithms_request: "refdata.v1.tenor_resolution_algorithms.list",
    save_tenor_resolution_algorithm_request: "refdata.v1.tenor_resolution_algorithms.save",
    delete_tenor_resolution_algorithm_request: "refdata.v1.tenor_resolution_algorithms.delete",
    get_tenor_resolution_algorithm_history_request: "refdata.v1.tenor_resolution_algorithms.history",
} as const;
