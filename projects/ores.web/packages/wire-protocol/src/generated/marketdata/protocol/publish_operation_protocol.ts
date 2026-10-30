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
/**
 * @brief The workflow step that publishes the DQ-cleared market data observations.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_marketdata_publish_market_data_observations_from_dq_fn.
 */
export interface PublishMarketDataObservationsFromDqRequest {}

/**
 * @brief The workflow step that publishes the adapter catalogue from DQ.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_marketdata_publish_adapters_from_dq_fn.
 */
export interface PublishAdaptersFromDqRequest {}

/**
 * @brief The workflow step that publishes the supplier catalogue from DQ.
 *
 * A trigger rather than a request: the DQ publisher sends it and reads no
 * reply. The SQL function the handler calls is
 * ores_marketdata_publish_suppliers_from_dq_fn.
 */
export interface PublishSuppliersFromDqRequest {}

export const subjects = {
    publish_market_data_observations_from_dq_request:
        'marketdata.v1.ops.publish_market_data_observations_from_dq',
    publish_adapters_from_dq_request: 'marketdata.v1.ops.publish_adapters_from_dq',
    publish_suppliers_from_dq_request: 'marketdata.v1.ops.publish_suppliers_from_dq',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    publish_market_data_observations_from_dq_request: true,
    publish_adapters_from_dq_request: true,
    publish_suppliers_from_dq_request: true,
} as const;
