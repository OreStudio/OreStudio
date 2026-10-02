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
import type { TradeEnvelopeAdditionalField } from '../domain/trade_envelope_additional_field.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeEnvelopeAdditionalFieldKey {
    trade_id: string;
    sequence_number: number;
}

export interface TradeEnvelopeAdditionalFieldWrite {
    trade_id: string;
    sequence_number: number;
    name: string;
    value: string;
}

export interface TradeEnvelopeAdditionalFieldChange {
    write: TradeEnvelopeAdditionalFieldWrite;
    precondition: Precondition;
}

export interface TradeEnvelopeAdditionalFieldRemoval {
    key: TradeEnvelopeAdditionalFieldKey;
    precondition: Precondition;
}

export interface TradeEnvelopeAdditionalFieldLookup {
    key: TradeEnvelopeAdditionalFieldKey;
    trade_envelope_additional_field: TradeEnvelopeAdditionalField | null;
}

export interface TradeEnvelopeAdditionalFieldEvent {
    event_id: string;
    key: TradeEnvelopeAdditionalFieldKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeEnvelopeAdditionalFieldVersionKey {
    trade_envelope_additional_field: TradeEnvelopeAdditionalFieldKey;
    version: number;
}

export interface TradeEnvelopeAdditionalFieldVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeEnvelopeAdditionalFieldsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListTradeEnvelopeAdditionalFieldsResponse {
    result: Result;
    trade_envelope_additional_fields: TradeEnvelopeAdditionalField[];
    total: number;
}

export interface GetTradeEnvelopeAdditionalFieldRequest {
    key: TradeEnvelopeAdditionalFieldKey;
}

export interface GetTradeEnvelopeAdditionalFieldResponse {
    result: Result;
    trade_envelope_additional_field: TradeEnvelopeAdditionalField | null;
}

export interface GetManyTradeEnvelopeAdditionalFieldsRequest {
    keys: TradeEnvelopeAdditionalFieldKey[];
}

export interface GetManyTradeEnvelopeAdditionalFieldsResponse {
    result: Result;
    entries: TradeEnvelopeAdditionalFieldLookup[];
}

export interface PutTradeEnvelopeAdditionalFieldRequest {
    change: TradeEnvelopeAdditionalFieldChange;
    intent: ChangeIntent;
}

export interface PutTradeEnvelopeAdditionalFieldResponse {
    result: Result;
    trade_envelope_additional_field: TradeEnvelopeAdditionalField | null;
}

export interface PutManyTradeEnvelopeAdditionalFieldsRequest {
    changes: TradeEnvelopeAdditionalFieldChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeEnvelopeAdditionalFieldsResponse {
    result: Result;
    trade_envelope_additional_fields: TradeEnvelopeAdditionalField[];
}

export interface DeleteTradeEnvelopeAdditionalFieldRequest {
    removal: TradeEnvelopeAdditionalFieldRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeEnvelopeAdditionalFieldResponse {
    result: Result;
}

export interface DeleteManyTradeEnvelopeAdditionalFieldsRequest {
    removals: TradeEnvelopeAdditionalFieldRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeEnvelopeAdditionalFieldsResponse {
    result: Result;
}

export interface ListTradeEnvelopeAdditionalFieldVersionsRequest {
    key: TradeEnvelopeAdditionalFieldKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeEnvelopeAdditionalFieldVersionsFilter | null;
}

export interface ListTradeEnvelopeAdditionalFieldVersionsResponse {
    result: Result;
    versions: TradeEnvelopeAdditionalField[];
    total: number;
}

export interface GetTradeEnvelopeAdditionalFieldVersionRequest {
    key: TradeEnvelopeAdditionalFieldVersionKey;
}

export interface GetTradeEnvelopeAdditionalFieldVersionResponse {
    result: Result;
    version: TradeEnvelopeAdditionalField | null;
}

export const subjects = {
    list_trade_envelope_additional_fields_request:
        'trading.v1.trade_envelope_additional_fields.list',
    get_trade_envelope_additional_field_request: 'trading.v1.trade_envelope_additional_fields.get',
    get_many_trade_envelope_additional_fields_request:
        'trading.v1.trade_envelope_additional_fields.get_many',
    put_trade_envelope_additional_field_request: 'trading.v1.trade_envelope_additional_fields.put',
    put_many_trade_envelope_additional_fields_request:
        'trading.v1.trade_envelope_additional_fields.put_many',
    delete_trade_envelope_additional_field_request:
        'trading.v1.trade_envelope_additional_fields.delete',
    delete_many_trade_envelope_additional_fields_request:
        'trading.v1.trade_envelope_additional_fields.delete_many',
    list_trade_envelope_additional_field_versions_request:
        'trading.v1.trade_envelope_additional_fields_versions.list',
    get_trade_envelope_additional_field_version_request:
        'trading.v1.trade_envelope_additional_fields_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_envelope_additional_fields_request: true,
    get_trade_envelope_additional_field_request: true,
    get_many_trade_envelope_additional_fields_request: true,
    put_trade_envelope_additional_field_request: true,
    put_many_trade_envelope_additional_fields_request: true,
    delete_trade_envelope_additional_field_request: true,
    delete_many_trade_envelope_additional_fields_request: true,
    list_trade_envelope_additional_field_versions_request: true,
    get_trade_envelope_additional_field_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_envelope_additional_fields_events.created',
    updated: 'trading.v1.trade_envelope_additional_fields_events.updated',
    deleted: 'trading.v1.trade_envelope_additional_fields_events.deleted',
} as const;
