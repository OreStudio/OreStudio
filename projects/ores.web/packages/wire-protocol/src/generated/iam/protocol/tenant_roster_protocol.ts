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
import type { Tenant } from '../domain/tenant.js';

/**
 * @brief One page of the tenants that match a search and two filters.
 *
 * The system tenant is never in the answer. The page is in code order, and the
 * answer carries how many tenants match in all.
 */
export interface SearchTenantsRequest {
    /**
     * @brief Text matched, without regard to case, anywhere in the code, the name
     * or the hostname. Empty matches every tenant.
     */
    search: string;
    /**
     * @brief A tenant type code to keep. Empty keeps every type.
     */
    type_filter: string;
    /**
     * @brief A tenant status code to keep. Empty keeps every status.
     */
    status_filter: string;
    /**
     * @brief How many matching tenants to skip.
     */
    offset: number;
    /**
     * @brief The most tenants to return. The handler caps it at 1000.
     */
    limit: number;
}

export interface SearchTenantsResponse {
    success: boolean;
    /**
     * @brief Why the search failed, or empty.
     */
    message: string;
    /**
     * @brief The page of matching tenants, in code order.
     */
    tenants: Tenant[];
    /**
     * @brief How many tenants match in all, across every page.
     */
    total: number;
}

export const subjects = {
    search_tenants_request: 'iam.v1.tenants.search',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    search_tenants_request: true,
} as const;
