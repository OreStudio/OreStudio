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
import type { MarketStatePayload } from '../domain/market_state_payload.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface MarketStatePayloadKey {
    id: string;
}

export interface MarketStatePayloadWrite {
    id: string;
    party_id: string;
    report_instance_id: string;
    family: string;
    file_name: string;
    as_of: string;
    report_definition_id: string;
    row_count: number;
    column_count: number;
    column_names: string;
    key_values: string;
    keys_truncated: boolean;
    payload: string;
    payload_bytes: number;
    content_sha256: string;
    source: string;
}

export interface MarketStatePayloadChange {
    write: MarketStatePayloadWrite;
    precondition: Precondition;
}

export interface MarketStatePayloadRemoval {
    key: MarketStatePayloadKey;
    precondition: Precondition;
}

export interface MarketStatePayloadLookup {
    key: MarketStatePayloadKey;
    market_state_payload: MarketStatePayload | null;
}

export interface MarketStatePayloadsFilter {
    report_instance_id: string | null;
    family: string | null;
    id_one_of: string[] | null;
    report_instance_id_one_of: string[] | null;
    family_one_of: string[] | null;
}

export interface MarketStatePayloadEvent {
    event_id: string;
    key: MarketStatePayloadKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListMarketStatePayloadsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: MarketStatePayloadsFilter | null;
    as_of: string | null;
}

export interface ListMarketStatePayloadsResponse {
    result: Result;
    payloads: MarketStatePayload[];
    total: number;
}

export interface GetMarketStatePayloadRequest {
    key: MarketStatePayloadKey;
}

export interface GetMarketStatePayloadResponse {
    result: Result;
    market_state_payload: MarketStatePayload | null;
}

export interface GetManyMarketStatePayloadsRequest {
    keys: MarketStatePayloadKey[];
}

export interface GetManyMarketStatePayloadsResponse {
    result: Result;
    entries: MarketStatePayloadLookup[];
}

export interface PutMarketStatePayloadRequest {
    change: MarketStatePayloadChange;
    intent: ChangeIntent;
}

export interface PutMarketStatePayloadResponse {
    result: Result;
    market_state_payload: MarketStatePayload | null;
}

export interface PutManyMarketStatePayloadsRequest {
    changes: MarketStatePayloadChange[];
    intent: ChangeIntent;
}

export interface PutManyMarketStatePayloadsResponse {
    result: Result;
    payloads: MarketStatePayload[];
}

export interface DeleteMarketStatePayloadRequest {
    removal: MarketStatePayloadRemoval;
    intent: ChangeIntent;
}

export interface DeleteMarketStatePayloadResponse {
    result: Result;
}

export interface DeleteManyMarketStatePayloadsRequest {
    removals: MarketStatePayloadRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyMarketStatePayloadsResponse {
    result: Result;
}

export const subjects = {
    list_market_state_payloads_request: 'marketdata.v1.market_state_payloads.list',
    get_market_state_payload_request: 'marketdata.v1.market_state_payloads.get',
    get_many_market_state_payloads_request: 'marketdata.v1.market_state_payloads.get_many',
    put_market_state_payload_request: 'marketdata.v1.market_state_payloads.put',
    put_many_market_state_payloads_request: 'marketdata.v1.market_state_payloads.put_many',
    delete_market_state_payload_request: 'marketdata.v1.market_state_payloads.delete',
    delete_many_market_state_payloads_request: 'marketdata.v1.market_state_payloads.delete_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_market_state_payloads_request: true,
    get_market_state_payload_request: true,
    get_many_market_state_payloads_request: true,
    put_market_state_payload_request: true,
    put_many_market_state_payloads_request: true,
    delete_market_state_payload_request: true,
    delete_many_market_state_payloads_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'marketdata.v1.market_state_payloads_events.created',
    updated: 'marketdata.v1.market_state_payloads_events.updated',
    deleted: 'marketdata.v1.market_state_payloads_events.deleted',
} as const;
