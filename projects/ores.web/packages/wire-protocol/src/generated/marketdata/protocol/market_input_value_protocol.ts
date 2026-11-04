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
import type { MarketInputValue } from '../domain/market_input_value.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketInputValueKey {
    id: string;
}

export interface MarketInputValueWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    result_id: string;
    source: string;
    line: number;
    as_of: string;
    report_definition_id: string;
    family: string;
    datum_id: string;
    value: number;
    payment_date: string | null;
}

export interface MarketInputValueChange {
    write: MarketInputValueWrite;
    precondition: Precondition;
}

export interface MarketInputValueRemoval {
    key: MarketInputValueKey;
    precondition: Precondition;
}

export interface MarketInputValueLookup {
    key: MarketInputValueKey;
    market_input_value: MarketInputValue | null;
}

export interface MarketInputValuesFilter {
    report_instance_id: string | null;
    family: string | null;
    datum_id: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    family_one_of: string[] | null;
    datum_id_one_of: string[] | null;
}

export interface MarketInputValueEvent {
    event_id: string;
    key: MarketInputValueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListMarketInputValuesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketInputValuesFilter | null;
    as_of: string | null;
}

export interface ListMarketInputValuesResponse {
    result: Result;
    values: MarketInputValue[];
    total: number;
}

export interface GetMarketInputValueRequest {
    key: MarketInputValueKey;
}

export interface GetMarketInputValueResponse {
    result: Result;
    market_input_value: MarketInputValue | null;
}

export interface GetManyMarketInputValuesRequest {
    keys: MarketInputValueKey[];
}

export interface GetManyMarketInputValuesResponse {
    result: Result;
    entries: MarketInputValueLookup[];
}

export interface PutMarketInputValueRequest {
    change: MarketInputValueChange;
    intent: ChangeIntent;
}

export interface PutMarketInputValueResponse {
    result: Result;
    market_input_value: MarketInputValue | null;
}

export interface PutManyMarketInputValuesRequest {
    changes: MarketInputValueChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketInputValuesResponse {
    result: Result;
    values: MarketInputValue[];
}

export interface DeleteMarketInputValueRequest {
    removal: MarketInputValueRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketInputValueResponse {
    result: Result;
}

export interface DeleteManyMarketInputValuesRequest {
    removals: MarketInputValueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketInputValuesResponse {
    result: Result;
}

export const subjects = {
    list_market_input_values_request: 'marketdata.v1.market_input_values.list',
    get_market_input_value_request: 'marketdata.v1.market_input_values.get',
    get_many_market_input_values_request: 'marketdata.v1.market_input_values.get_many',
    put_market_input_value_request: 'marketdata.v1.market_input_values.put',
    put_many_market_input_values_request: 'marketdata.v1.market_input_values.put_many',
    delete_market_input_value_request: 'marketdata.v1.market_input_values.delete',
    delete_many_market_input_values_request: 'marketdata.v1.market_input_values.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_input_values_request: true,
    get_market_input_value_request: true,
    get_many_market_input_values_request: true,
    put_market_input_value_request: true,
    put_many_market_input_values_request: true,
    delete_market_input_value_request: true,
    delete_many_market_input_values_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.market_input_values.created',
    updated: 'marketdata.v1.events.market_input_values.updated',
    deleted: 'marketdata.v1.events.market_input_values.deleted',
} as const;
