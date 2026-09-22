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
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface TenorConventionResolutionKey {
    convention_code: string;
    tenor_code: string;
}

export interface TenorConventionResolutionLookup {
    key: TenorConventionResolutionKey;
    tenor_convention_resolution: TenorConventionResolution | null;
}

export interface TenorConventionResolutionsFilter {
    convention_code: string | null;
}

export interface TenorConventionResolutionEvent {
    event_id: string;
    key: TenorConventionResolutionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListTenorConventionResolutionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TenorConventionResolutionsFilter | null;
}

export interface ListTenorConventionResolutionsResponse {
    result: Result;
    tenor_convention_resolutions: TenorConventionResolution[];
    total: number;
}

export interface GetTenorConventionResolutionRequest {
    key: TenorConventionResolutionKey;
}

export interface GetTenorConventionResolutionResponse {
    result: Result;
    tenor_convention_resolution: TenorConventionResolution | null;
}

export interface GetManyTenorConventionResolutionsRequest {
    keys: TenorConventionResolutionKey[];
}

export interface GetManyTenorConventionResolutionsResponse {
    result: Result;
    entries: TenorConventionResolutionLookup[];
}

export interface ListByConventionCodeTenorConventionResolutionsRequest {
    convention_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: TenorConventionResolutionsFilter | null;
}

export interface ListByConventionCodeTenorConventionResolutionsResponse {
    result: Result;
    tenor_convention_resolutions: TenorConventionResolution[];
    total: number;
}

export const subjects = {
    list_tenor_convention_resolutions_request: "refdata.v1.tenor_convention_resolutions.list",
    get_tenor_convention_resolution_request: "refdata.v1.tenor_convention_resolutions.get",
    get_many_tenor_convention_resolutions_request: "refdata.v1.tenor_convention_resolutions.get_many",
    list_by_convention_code_tenor_convention_resolutions_request: "refdata.v1.tenor_convention_resolutions.list_by_convention_code",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenor_convention_resolutions_request: true,
    get_tenor_convention_resolution_request: true,
    get_many_tenor_convention_resolutions_request: true,
    list_by_convention_code_tenor_convention_resolutions_request: true,
} as const;
