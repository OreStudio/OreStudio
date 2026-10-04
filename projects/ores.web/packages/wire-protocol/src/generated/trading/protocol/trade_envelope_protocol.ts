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
import type { TradeEnvelope } from '../domain/trade_envelope.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeEnvelopeKey {
    trade_id: string;
}

export interface TradeEnvelopeWrite {
    trade_id: string;
    counter_party: string | null;
    netting_set_id: string | null;
    has_portfolio_ids: boolean;
    has_additional_fields: boolean;
}

export interface TradeEnvelopeChange {
    write: TradeEnvelopeWrite;
    precondition: Precondition;
}

export interface TradeEnvelopeRemoval {
    key: TradeEnvelopeKey;
    precondition: Precondition;
}

export interface TradeEnvelopeLookup {
    key: TradeEnvelopeKey;
    trade_envelope: TradeEnvelope | null;
}

export interface TradeEnvelopesFilter {
    trade_id_one_of: string[] | null;
}

export interface TradeEnvelopeEvent {
    event_id: string;
    key: TradeEnvelopeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeEnvelopeVersionKey {
    trade_envelope: TradeEnvelopeKey;
    version: number;
}

export interface TradeEnvelopeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeEnvelopesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeEnvelopesFilter | null;
}

export interface ListTradeEnvelopesResponse {
    result: Result;
    trade_envelopes: TradeEnvelope[];
    total: number;
}

export interface GetTradeEnvelopeRequest {
    key: TradeEnvelopeKey;
}

export interface GetTradeEnvelopeResponse {
    result: Result;
    trade_envelope: TradeEnvelope | null;
}

export interface GetManyTradeEnvelopesRequest {
    keys: TradeEnvelopeKey[];
}

export interface GetManyTradeEnvelopesResponse {
    result: Result;
    entries: TradeEnvelopeLookup[];
}

export interface PutTradeEnvelopeRequest {
    change: TradeEnvelopeChange;
    intent: ChangeIntent;
}

export interface PutTradeEnvelopeResponse {
    result: Result;
    trade_envelope: TradeEnvelope | null;
}

export interface PutManyTradeEnvelopesRequest {
    changes: TradeEnvelopeChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeEnvelopesResponse {
    result: Result;
    trade_envelopes: TradeEnvelope[];
}

export interface DeleteTradeEnvelopeRequest {
    removal: TradeEnvelopeRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeEnvelopeResponse {
    result: Result;
}

export interface DeleteManyTradeEnvelopesRequest {
    removals: TradeEnvelopeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeEnvelopesResponse {
    result: Result;
}

export interface ListTradeEnvelopeVersionsRequest {
    key: TradeEnvelopeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeEnvelopeVersionsFilter | null;
}

export interface ListTradeEnvelopeVersionsResponse {
    result: Result;
    versions: TradeEnvelope[];
    total: number;
}

export interface GetTradeEnvelopeVersionRequest {
    key: TradeEnvelopeVersionKey;
}

export interface GetTradeEnvelopeVersionResponse {
    result: Result;
    version: TradeEnvelope | null;
}

export const subjects = {
    list_trade_envelopes_request: 'trading.v1.trade_envelopes.list',
    get_trade_envelope_request: 'trading.v1.trade_envelopes.get',
    get_many_trade_envelopes_request: 'trading.v1.trade_envelopes.get_many',
    put_trade_envelope_request: 'trading.v1.trade_envelopes.put',
    put_many_trade_envelopes_request: 'trading.v1.trade_envelopes.put_many',
    delete_trade_envelope_request: 'trading.v1.trade_envelopes.delete',
    delete_many_trade_envelopes_request: 'trading.v1.trade_envelopes.delete_many',
    list_trade_envelope_versions_request: 'trading.v1.trade_envelopes_versions.list',
    get_trade_envelope_version_request: 'trading.v1.trade_envelopes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_envelopes_request: true,
    get_trade_envelope_request: true,
    get_many_trade_envelopes_request: true,
    put_trade_envelope_request: true,
    put_many_trade_envelopes_request: true,
    delete_trade_envelope_request: true,
    delete_many_trade_envelopes_request: true,
    list_trade_envelope_versions_request: true,
    get_trade_envelope_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_envelopes_events.created',
    updated: 'trading.v1.trade_envelopes_events.updated',
    deleted: 'trading.v1.trade_envelopes_events.deleted',
} as const;
