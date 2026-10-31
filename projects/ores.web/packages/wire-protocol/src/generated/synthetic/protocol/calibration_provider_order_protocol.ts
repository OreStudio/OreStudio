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
import type { CalibrationProviderOrder } from '../domain/calibration_provider_order.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CalibrationProviderOrderKey {
    id: string;
}

export interface CalibrationProviderOrderWrite {
    id: string;
    policy_id: string;
    provider_code: string;
    rank: number;
}

export interface CalibrationProviderOrderChange {
    write: CalibrationProviderOrderWrite;
    precondition: Precondition;
}

export interface CalibrationProviderOrderRemoval {
    key: CalibrationProviderOrderKey;
    precondition: Precondition;
}

export interface CalibrationProviderOrderLookup {
    key: CalibrationProviderOrderKey;
    calibration_provider_order: CalibrationProviderOrder | null;
}

export interface CalibrationProviderOrdersFilter {
    policy_id: string | null;
    provider_code: string | null;
    id_one_of: string[] | null;
    policy_id_one_of: string[] | null;
    provider_code_one_of: string[] | null;
}

export interface CalibrationProviderOrderEvent {
    event_id: string;
    key: CalibrationProviderOrderKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CalibrationProviderOrderVersionKey {
    calibration_provider_order: CalibrationProviderOrderKey;
    version: number;
}

export interface CalibrationProviderOrderVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCalibrationProviderOrdersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CalibrationProviderOrdersFilter | null;
    as_of: string | null;
}

export interface ListCalibrationProviderOrdersResponse {
    result: Result;
    provider_orders: CalibrationProviderOrder[];
    total: number;
}

export interface GetCalibrationProviderOrderRequest {
    key: CalibrationProviderOrderKey;
}

export interface GetCalibrationProviderOrderResponse {
    result: Result;
    calibration_provider_order: CalibrationProviderOrder | null;
}

export interface GetManyCalibrationProviderOrdersRequest {
    keys: CalibrationProviderOrderKey[];
}

export interface GetManyCalibrationProviderOrdersResponse {
    result: Result;
    entries: CalibrationProviderOrderLookup[];
}

export interface PutCalibrationProviderOrderRequest {
    change: CalibrationProviderOrderChange;
    intent: ChangeIntent;
}

export interface PutCalibrationProviderOrderResponse {
    result: Result;
    calibration_provider_order: CalibrationProviderOrder | null;
}

export interface PutManyCalibrationProviderOrdersRequest {
    changes: CalibrationProviderOrderChange[];
    intent: ChangeIntent;
}

export interface PutManyCalibrationProviderOrdersResponse {
    result: Result;
    provider_orders: CalibrationProviderOrder[];
}

export interface DeleteCalibrationProviderOrderRequest {
    removal: CalibrationProviderOrderRemoval;
    intent: ChangeIntent;
}

export interface DeleteCalibrationProviderOrderResponse {
    result: Result;
}

export interface DeleteManyCalibrationProviderOrdersRequest {
    removals: CalibrationProviderOrderRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCalibrationProviderOrdersResponse {
    result: Result;
}

export interface ListCalibrationProviderOrderVersionsRequest {
    key: CalibrationProviderOrderKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CalibrationProviderOrderVersionsFilter | null;
}

export interface ListCalibrationProviderOrderVersionsResponse {
    result: Result;
    versions: CalibrationProviderOrder[];
    total: number;
}

export interface GetCalibrationProviderOrderVersionRequest {
    key: CalibrationProviderOrderVersionKey;
}

export interface GetCalibrationProviderOrderVersionResponse {
    result: Result;
    version: CalibrationProviderOrder | null;
}

export const subjects = {
    list_calibration_provider_orders_request: 'synthetic.v1.calibration_provider_orders.list',
    get_calibration_provider_order_request: 'synthetic.v1.calibration_provider_orders.get',
    get_many_calibration_provider_orders_request:
        'synthetic.v1.calibration_provider_orders.get_many',
    put_calibration_provider_order_request: 'synthetic.v1.calibration_provider_orders.put',
    put_many_calibration_provider_orders_request:
        'synthetic.v1.calibration_provider_orders.put_many',
    delete_calibration_provider_order_request: 'synthetic.v1.calibration_provider_orders.delete',
    delete_many_calibration_provider_orders_request:
        'synthetic.v1.calibration_provider_orders.delete_many',
    list_calibration_provider_order_versions_request:
        'synthetic.v1.calibration_provider_orders_versions.list',
    get_calibration_provider_order_version_request:
        'synthetic.v1.calibration_provider_orders_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_calibration_provider_orders_request: true,
    get_calibration_provider_order_request: true,
    get_many_calibration_provider_orders_request: true,
    put_calibration_provider_order_request: true,
    put_many_calibration_provider_orders_request: true,
    delete_calibration_provider_order_request: true,
    delete_many_calibration_provider_orders_request: true,
    list_calibration_provider_order_versions_request: true,
    get_calibration_provider_order_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.calibration_provider_orders_events.created',
    updated: 'synthetic.v1.calibration_provider_orders_events.updated',
    deleted: 'synthetic.v1.calibration_provider_orders_events.deleted',
} as const;
