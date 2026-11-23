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
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief What one route of the party demands of its provider.
 */
export interface DemandedRoute {
    /**
     * @brief The provider the route is fed by.
     */
    provider_code: string;
    /**
     * @brief The name of the route's source, which is the id of its adapter.
     */
    adapter_id: string;
    /**
     * @brief Whether the party may receive the provider's data.
     */
    entitled: boolean;
    /**
     * @brief Why the route is not entitled, such as not_entitled or terms_not_accepted.
     * Empty when it is.
     */
    refusal: string;
    /**
     * @brief The items the route demands, as oresmd URIs, sorted. Empty when the
     * route is not entitled.
     */
    items: string[];
}

/**
 * @brief Reads what one party of the caller's tenant demands of each of its routes.
 */
export interface GetPartyDemandRequest {
    /**
     * @brief The party whose routes are read.
     */
    party_id: string;
}

/**
 * @brief The demand of each route of the party.
 */
export interface GetPartyDemandResponse {
    /**
     * @brief Whether the catalogue was read.
     */
    result: Result;
    /**
     * @brief One entry for each route of the party whose source the catalogue holds. A
     * party with no route has none.
     */
    routes: DemandedRoute[];
}

export const subjects = {
    get_party_demand_request: 'marketdata.v1.ops.get_party_demand',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_party_demand_request: true,
} as const;
