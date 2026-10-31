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
import type { TradeSummary } from '../domain/trade_summary.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeSummaryKey {
    id: string;
}

export interface TradeSummaryLookup {
    key: TradeSummaryKey;
    trade_summary: TradeSummary | null;
}

export interface TradeSummariesFilter {
    row_kind: string | null;
    party_id: string | null;
    counterparty_id: string | null | null;
    book_id: string | null | null;
    instrument_type: string | null;
    status: string | null | null;
    authorisation_status: string | null;
    trade_date: string | null | null;
    structure_id: string | null | null;
    is_leg: boolean | null;
    id_one_of: string[] | null;
    row_kind_one_of: string[] | null;
    party_id_one_of: string[] | null;
    counterparty_id_one_of: string[] | null;
    book_id_one_of: string[] | null;
    instrument_type_one_of: string[] | null;
    status_one_of: string[] | null;
    authorisation_status_one_of: string[] | null;
    trade_date_one_of: string[] | null;
    structure_id_one_of: string[] | null;
    is_leg_one_of: boolean[] | null;
    trade_date_from: string | null;
    trade_date_to: string | null;
    search: string | null;
}

export interface TradeSummaryEvent {
    event_id: string;
    key: TradeSummaryKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListTradeSummariesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeSummariesFilter | null;
}

export interface ListTradeSummariesResponse {
    result: Result;
    trade_summaries: TradeSummary[];
    total: number;
}

export interface GetTradeSummaryRequest {
    key: TradeSummaryKey;
}

export interface GetTradeSummaryResponse {
    result: Result;
    trade_summary: TradeSummary | null;
}

export interface GetManyTradeSummariesRequest {
    keys: TradeSummaryKey[];
}

export interface GetManyTradeSummariesResponse {
    result: Result;
    entries: TradeSummaryLookup[];
}

export const subjects = {
    list_trade_summaries_request: 'trading.v1.trade_summaries.list',
    get_trade_summary_request: 'trading.v1.trade_summaries.get',
    get_many_trade_summaries_request: 'trading.v1.trade_summaries.get_many',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_summaries_request: true,
    get_trade_summary_request: true,
    get_many_trade_summaries_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.trade_summaries_events.created',
    updated: 'trading.v1.trade_summaries_events.updated',
    deleted: 'trading.v1.trade_summaries_events.deleted',
} as const;
