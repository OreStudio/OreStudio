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
import type { Party } from '../domain/party.js';

/**
 * @brief One page of the current parties of one named tenant.
 *
 * Refused to a caller outside the system tenant. The system tenant itself is
 * not a tenant this read answers for.
 */
export interface ListTenantPartiesRequest {
    /**
     * @brief The id of the tenant whose parties are read.
     */
    tenant_id: string;
    /**
     * @brief How many parties to skip.
     */
    offset: number;
    /**
     * @brief The most parties to return. The handler caps it at 1000.
     */
    limit: number;
}

export interface ListTenantPartiesResponse {
    success: boolean;
    /**
     * @brief Why the read failed, or empty.
     */
    message: string;
    /**
     * @brief The page of the tenant's current parties.
     */
    parties: Party[];
    /**
     * @brief How many current parties the tenant holds in all.
     */
    total: number;
}

export const subjects = {
    list_tenant_parties_request: 'refdata.v1.parties.list-of-tenant',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_tenant_parties_request: true,
} as const;
