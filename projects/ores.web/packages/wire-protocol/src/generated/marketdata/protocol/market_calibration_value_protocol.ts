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
import type { MarketCalibrationValue } from '../domain/market_calibration_value.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketCalibrationValueKey {
    id: string;
}

export interface MarketCalibrationValueWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    result_id: string;
    source: string;
    line: number;
    as_of: string;
    report_definition_id: string;
    market_object_type: string;
    market_object_id: string;
    result_name: string;
    result_key_1: string;
    result_key_2: string;
    result_key_3: string;
    result_type: string;
    value: string;
    figure: number | null;
}

export interface MarketCalibrationValueChange {
    write: MarketCalibrationValueWrite;
    precondition: Precondition;
}

export interface MarketCalibrationValueRemoval {
    key: MarketCalibrationValueKey;
    precondition: Precondition;
}

export interface MarketCalibrationValueLookup {
    key: MarketCalibrationValueKey;
    market_calibration_value: MarketCalibrationValue | null;
}

export interface MarketCalibrationValuesFilter {
    report_instance_id: string | null;
    market_object_type: string | null;
    market_object_id: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    market_object_type_one_of: string[] | null;
    market_object_id_one_of: string[] | null;
}

export interface MarketCalibrationValueEvent {
    event_id: string;
    key: MarketCalibrationValueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListMarketCalibrationValuesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketCalibrationValuesFilter | null;
    as_of: string | null;
}

export interface ListMarketCalibrationValuesResponse {
    result: Result;
    values: MarketCalibrationValue[];
    total: number;
}

export interface GetMarketCalibrationValueRequest {
    key: MarketCalibrationValueKey;
}

export interface GetMarketCalibrationValueResponse {
    result: Result;
    market_calibration_value: MarketCalibrationValue | null;
}

export interface GetManyMarketCalibrationValuesRequest {
    keys: MarketCalibrationValueKey[];
}

export interface GetManyMarketCalibrationValuesResponse {
    result: Result;
    entries: MarketCalibrationValueLookup[];
}

export interface PutMarketCalibrationValueRequest {
    change: MarketCalibrationValueChange;
    intent: ChangeIntent;
}

export interface PutMarketCalibrationValueResponse {
    result: Result;
    market_calibration_value: MarketCalibrationValue | null;
}

export interface PutManyMarketCalibrationValuesRequest {
    changes: MarketCalibrationValueChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketCalibrationValuesResponse {
    result: Result;
    values: MarketCalibrationValue[];
}

export interface DeleteMarketCalibrationValueRequest {
    removal: MarketCalibrationValueRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketCalibrationValueResponse {
    result: Result;
}

export interface DeleteManyMarketCalibrationValuesRequest {
    removals: MarketCalibrationValueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketCalibrationValuesResponse {
    result: Result;
}

export const subjects = {
    list_market_calibration_values_request: 'marketdata.v1.market_calibration_values.list',
    get_market_calibration_value_request: 'marketdata.v1.market_calibration_values.get',
    get_many_market_calibration_values_request: 'marketdata.v1.market_calibration_values.get_many',
    put_market_calibration_value_request: 'marketdata.v1.market_calibration_values.put',
    put_many_market_calibration_values_request: 'marketdata.v1.market_calibration_values.put_many',
    delete_market_calibration_value_request: 'marketdata.v1.market_calibration_values.delete',
    delete_many_market_calibration_values_request:
        'marketdata.v1.market_calibration_values.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_calibration_values_request: true,
    get_market_calibration_value_request: true,
    get_many_market_calibration_values_request: true,
    put_market_calibration_value_request: true,
    put_many_market_calibration_values_request: true,
    delete_market_calibration_value_request: true,
    delete_many_market_calibration_values_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.market_calibration_values_events.created',
    updated: 'marketdata.v1.market_calibration_values_events.updated',
    deleted: 'marketdata.v1.market_calibration_values_events.deleted',
} as const;
