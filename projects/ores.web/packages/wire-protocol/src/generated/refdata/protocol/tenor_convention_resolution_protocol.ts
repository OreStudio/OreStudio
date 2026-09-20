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
import type { TenorConventionResolution } from '../domain/tenor_convention_resolution.js';

export interface GetTenorConventionResolutionsRequest {
    offset: number;
    limit: number;
}

export interface GetTenorConventionResolutionsResponse {
    tenor_convention_resolutions: TenorConventionResolution[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface GetTenorConventionResolutionsByConventionRequest {
    convention_code: string;
    offset: number;
    limit: number;
}

export interface GetTenorConventionResolutionsByConventionResponse {
    tenor_convention_resolutions: TenorConventionResolutionView[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface CountTenorConventionResolutionsByConventionRequest {
    convention_code: string;
}

export interface CountTenorConventionResolutionsByConventionResponse {
    total_available_count: number;
}

export interface CountTenorConventionResolutionsByTenorRequest {
    tenor_code: string;
}

export interface CountTenorConventionResolutionsByTenorResponse {
    total_available_count: number;
}

export interface TenorConventionResolutionView {
    tenor_convention_resolution: TenorConventionResolution;
}

export const subjects = {
    get_tenor_convention_resolutions_request: "refdata.v1.tenor_convention_resolutions.list",
    get_tenor_convention_resolutions_by_convention_request: "refdata.v1.tenor_convention_resolutions.list_by_convention_code",
    count_tenor_convention_resolutions_by_convention_request: "refdata.v1.tenor_convention_resolutions.count_by_convention_code",
    count_tenor_convention_resolutions_by_tenor_request: "refdata.v1.tenor_convention_resolutions.count_by_tenor_code",
} as const;
