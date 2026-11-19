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
import type { InstrumentValue } from '../domain/instrument_value.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface InstrumentValueKey {
    trade_id: string;
    value_role: string;
    sequence_number: number;
}

export interface InstrumentValueWrite {
    trade_id: string;
    value_role: string;
    sequence_number: number;
    trade_activity_id: string;
    value: string;
}

export interface InstrumentValueChange {
    write: InstrumentValueWrite;
    precondition: Precondition;
}

export interface InstrumentValueRemoval {
    key: InstrumentValueKey;
    precondition: Precondition;
}

export interface InstrumentValueLookup {
    key: InstrumentValueKey;
    instrument_value: InstrumentValue | null;
}

export interface InstrumentValuesFilter {
    trade_id: string | null;
    trade_id_one_of: string[] | null;
}

export interface InstrumentValueEvent {
    event_id: string;
    key: InstrumentValueKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentValueVersionKey {
    instrument_value: InstrumentValueKey;
    version: number;
}

export interface InstrumentValueVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentValuesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentValuesFilter | null;
    as_of: string | null;
}

export interface ListInstrumentValuesResponse {
    result: Result;
    instrument_values: InstrumentValue[];
    total: number;
}

export interface GetInstrumentValueRequest {
    key: InstrumentValueKey;
}

export interface GetInstrumentValueResponse {
    result: Result;
    instrument_value: InstrumentValue | null;
}

export interface GetManyInstrumentValuesRequest {
    keys: InstrumentValueKey[];
}

export interface GetManyInstrumentValuesResponse {
    result: Result;
    entries: InstrumentValueLookup[];
}

export interface PutInstrumentValueRequest {
    change: InstrumentValueChange;
    intent: ChangeIntent;
}

export interface PutInstrumentValueResponse {
    result: Result;
    instrument_value: InstrumentValue | null;
}

export interface PutManyInstrumentValuesRequest {
    changes: InstrumentValueChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentValuesResponse {
    result: Result;
    instrument_values: InstrumentValue[];
}

export interface DeleteInstrumentValueRequest {
    removal: InstrumentValueRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentValueResponse {
    result: Result;
}

export interface DeleteManyInstrumentValuesRequest {
    removals: InstrumentValueRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentValuesResponse {
    result: Result;
}

export interface ListByTradeIdInstrumentValuesRequest {
    trade_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentValuesFilter | null;
}

export interface ListByTradeIdInstrumentValuesResponse {
    result: Result;
    instrument_values: InstrumentValue[];
    total: number;
}

export interface ListInstrumentValueVersionsRequest {
    key: InstrumentValueKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentValueVersionsFilter | null;
}

export interface ListInstrumentValueVersionsResponse {
    result: Result;
    versions: InstrumentValue[];
    total: number;
}

export interface GetInstrumentValueVersionRequest {
    key: InstrumentValueVersionKey;
}

export interface GetInstrumentValueVersionResponse {
    result: Result;
    version: InstrumentValue | null;
}

export const subjects = {
    list_instrument_values_request: 'trading.v1.instrument_values.list',
    get_instrument_value_request: 'trading.v1.instrument_values.get',
    get_many_instrument_values_request: 'trading.v1.instrument_values.get_many',
    put_instrument_value_request: 'trading.v1.instrument_values.put',
    put_many_instrument_values_request: 'trading.v1.instrument_values.put_many',
    delete_instrument_value_request: 'trading.v1.instrument_values.delete',
    delete_many_instrument_values_request: 'trading.v1.instrument_values.delete_many',
    list_by_trade_id_instrument_values_request: 'trading.v1.instrument_values.list_by_trade_id',
    list_instrument_value_versions_request: 'trading.v1.instrument_values_versions.list',
    get_instrument_value_version_request: 'trading.v1.instrument_values_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_values_request: true,
    get_instrument_value_request: true,
    get_many_instrument_values_request: true,
    put_instrument_value_request: true,
    put_many_instrument_values_request: true,
    delete_instrument_value_request: true,
    delete_many_instrument_values_request: true,
    list_by_trade_id_instrument_values_request: true,
    list_instrument_value_versions_request: true,
    get_instrument_value_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.instrument_values.created',
    updated: 'trading.v1.events.instrument_values.updated',
    deleted: 'trading.v1.events.instrument_values.deleted',
} as const;
