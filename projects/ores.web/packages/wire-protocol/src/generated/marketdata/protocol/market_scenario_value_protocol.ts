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
import type { MarketScenarioValue } from '../domain/market_scenario_value.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketScenarioValueKey {
    id: string;
}

export interface MarketScenarioValueWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    result_id: string;
    source: string;
    line: number;
    as_of: string;
    factor: string;
    report_definition_id: string;
    family: string;
    scenario: string;
    value: number;
}

export interface MarketScenarioValueChange {
    write: MarketScenarioValueWrite;
    precondition: Precondition;
}

export interface MarketScenarioValueRemoval {
    key: MarketScenarioValueKey;
    precondition: Precondition;
}

export interface MarketScenarioValueLookup {
    key: MarketScenarioValueKey;
    market_scenario_value: MarketScenarioValue | null;
}

export interface MarketScenarioValuesFilter {
    report_instance_id: string | null;
    factor: string | null;
    family: string | null;
    scenario: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    factor_one_of: string[] | null;
    family_one_of: string[] | null;
    scenario_one_of: string[] | null;
}

export interface MarketScenarioValueEvent {
    event_id: string;
    key: MarketScenarioValueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListMarketScenarioValuesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketScenarioValuesFilter | null;
    as_of: string | null;
}

export interface ListMarketScenarioValuesResponse {
    result: Result;
    values: MarketScenarioValue[];
    total: number;
}

export interface GetMarketScenarioValueRequest {
    key: MarketScenarioValueKey;
}

export interface GetMarketScenarioValueResponse {
    result: Result;
    market_scenario_value: MarketScenarioValue | null;
}

export interface GetManyMarketScenarioValuesRequest {
    keys: MarketScenarioValueKey[];
}

export interface GetManyMarketScenarioValuesResponse {
    result: Result;
    entries: MarketScenarioValueLookup[];
}

export interface PutMarketScenarioValueRequest {
    change: MarketScenarioValueChange;
    intent: ChangeIntent;
}

export interface PutMarketScenarioValueResponse {
    result: Result;
    market_scenario_value: MarketScenarioValue | null;
}

export interface PutManyMarketScenarioValuesRequest {
    changes: MarketScenarioValueChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketScenarioValuesResponse {
    result: Result;
    values: MarketScenarioValue[];
}

export interface DeleteMarketScenarioValueRequest {
    removal: MarketScenarioValueRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketScenarioValueResponse {
    result: Result;
}

export interface DeleteManyMarketScenarioValuesRequest {
    removals: MarketScenarioValueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketScenarioValuesResponse {
    result: Result;
}

export const subjects = {
    list_market_scenario_values_request: 'marketdata.v1.market_scenario_values.list',
    get_market_scenario_value_request: 'marketdata.v1.market_scenario_values.get',
    get_many_market_scenario_values_request: 'marketdata.v1.market_scenario_values.get_many',
    put_market_scenario_value_request: 'marketdata.v1.market_scenario_values.put',
    put_many_market_scenario_values_request: 'marketdata.v1.market_scenario_values.put_many',
    delete_market_scenario_value_request: 'marketdata.v1.market_scenario_values.delete',
    delete_many_market_scenario_values_request: 'marketdata.v1.market_scenario_values.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_scenario_values_request: true,
    get_market_scenario_value_request: true,
    get_many_market_scenario_values_request: true,
    put_market_scenario_value_request: true,
    put_many_market_scenario_values_request: true,
    delete_market_scenario_value_request: true,
    delete_many_market_scenario_values_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.events.market_scenario_values.created',
    updated: 'marketdata.v1.events.market_scenario_values.updated',
    deleted: 'marketdata.v1.events.market_scenario_values.deleted',
} as const;
