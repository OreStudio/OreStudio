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
import type { TradeAdditionalField } from '../domain/trade_additional_field.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeAdditionalFieldKey {
    trade_id: string;
    sequence_number: number;
}

export interface TradeAdditionalFieldWrite {
    trade_id: string;
    sequence_number: number;
    trade_activity_id: string;
    name: string;
    value: string;
}

export interface TradeAdditionalFieldChange {
    write: TradeAdditionalFieldWrite;
    precondition: Precondition;
}

export interface TradeAdditionalFieldRemoval {
    key: TradeAdditionalFieldKey;
    precondition: Precondition;
}

export interface TradeAdditionalFieldLookup {
    key: TradeAdditionalFieldKey;
    trade_additional_field: TradeAdditionalField | null;
}

export interface TradeAdditionalFieldEvent {
    event_id: string;
    key: TradeAdditionalFieldKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeAdditionalFieldVersionKey {
    trade_additional_field: TradeAdditionalFieldKey;
    version: number;
}

export interface TradeAdditionalFieldVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeAdditionalFieldsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListTradeAdditionalFieldsResponse {
    result: Result;
    trade_additional_fields: TradeAdditionalField[];
    total: number;
}

export interface GetTradeAdditionalFieldRequest {
    key: TradeAdditionalFieldKey;
}

export interface GetTradeAdditionalFieldResponse {
    result: Result;
    trade_additional_field: TradeAdditionalField | null;
}

export interface GetManyTradeAdditionalFieldsRequest {
    keys: TradeAdditionalFieldKey[];
}

export interface GetManyTradeAdditionalFieldsResponse {
    result: Result;
    entries: TradeAdditionalFieldLookup[];
}

export interface PutTradeAdditionalFieldRequest {
    change: TradeAdditionalFieldChange;
    intent: ChangeIntent;
}

export interface PutTradeAdditionalFieldResponse {
    result: Result;
    trade_additional_field: TradeAdditionalField | null;
}

export interface PutManyTradeAdditionalFieldsRequest {
    changes: TradeAdditionalFieldChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeAdditionalFieldsResponse {
    result: Result;
    trade_additional_fields: TradeAdditionalField[];
}

export interface DeleteTradeAdditionalFieldRequest {
    removal: TradeAdditionalFieldRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeAdditionalFieldResponse {
    result: Result;
}

export interface DeleteManyTradeAdditionalFieldsRequest {
    removals: TradeAdditionalFieldRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeAdditionalFieldsResponse {
    result: Result;
}

export interface ListTradeAdditionalFieldVersionsRequest {
    key: TradeAdditionalFieldKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeAdditionalFieldVersionsFilter | null;
}

export interface ListTradeAdditionalFieldVersionsResponse {
    result: Result;
    versions: TradeAdditionalField[];
    total: number;
}

export interface GetTradeAdditionalFieldVersionRequest {
    key: TradeAdditionalFieldVersionKey;
}

export interface GetTradeAdditionalFieldVersionResponse {
    result: Result;
    version: TradeAdditionalField | null;
}

export const subjects = {
    list_trade_additional_fields_request: 'trading.v1.trade_additional_fields.list',
    get_trade_additional_field_request: 'trading.v1.trade_additional_fields.get',
    get_many_trade_additional_fields_request: 'trading.v1.trade_additional_fields.get_many',
    put_trade_additional_field_request: 'trading.v1.trade_additional_fields.put',
    put_many_trade_additional_fields_request: 'trading.v1.trade_additional_fields.put_many',
    delete_trade_additional_field_request: 'trading.v1.trade_additional_fields.delete',
    delete_many_trade_additional_fields_request: 'trading.v1.trade_additional_fields.delete_many',
    list_trade_additional_field_versions_request:
        'trading.v1.trade_additional_fields_versions.list',
    get_trade_additional_field_version_request: 'trading.v1.trade_additional_fields_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_additional_fields_request: true,
    get_trade_additional_field_request: true,
    get_many_trade_additional_fields_request: true,
    put_trade_additional_field_request: true,
    put_many_trade_additional_fields_request: true,
    delete_trade_additional_field_request: true,
    delete_many_trade_additional_fields_request: true,
    list_trade_additional_field_versions_request: true,
    get_trade_additional_field_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_additional_fields_events.created',
    updated: 'trading.v1.trade_additional_fields_events.updated',
    deleted: 'trading.v1.trade_additional_fields_events.deleted',
} as const;
