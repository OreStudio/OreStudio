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
import type { CurveQuote } from '../domain/curve_quote.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveQuoteKey {
    id: string;
}

export interface CurveQuoteWrite {
    id: string;
    curve_definition_id: string;
    curve_segment_id: string;
    item_kind: string;
    quote_text: string;
    position: number;
}

export interface CurveQuoteChange {
    write: CurveQuoteWrite;
    precondition: Precondition;
}

export interface CurveQuoteRemoval {
    key: CurveQuoteKey;
    precondition: Precondition;
}

export interface CurveQuoteLookup {
    key: CurveQuoteKey;
    curve_quote: CurveQuote | null;
}

export interface CurveQuoteEvent {
    event_id: string;
    key: CurveQuoteKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveQuoteVersionKey {
    curve_quote: CurveQuoteKey;
    version: number;
}

export interface CurveQuoteVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveQuotesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveQuotesResponse {
    result: Result;
    quotes: CurveQuote[];
    total: number;
}

export interface GetCurveQuoteRequest {
    key: CurveQuoteKey;
}

export interface GetCurveQuoteResponse {
    result: Result;
    curve_quote: CurveQuote | null;
}

export interface GetManyCurveQuotesRequest {
    keys: CurveQuoteKey[];
}

export interface GetManyCurveQuotesResponse {
    result: Result;
    entries: CurveQuoteLookup[];
}

export interface PutCurveQuoteRequest {
    change: CurveQuoteChange;
    intent: ChangeIntent;
}

export interface PutCurveQuoteResponse {
    result: Result;
    curve_quote: CurveQuote | null;
}

export interface PutManyCurveQuotesRequest {
    changes: CurveQuoteChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveQuotesResponse {
    result: Result;
    quotes: CurveQuote[];
}

export interface DeleteCurveQuoteRequest {
    removal: CurveQuoteRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveQuoteResponse {
    result: Result;
}

export interface DeleteManyCurveQuotesRequest {
    removals: CurveQuoteRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveQuotesResponse {
    result: Result;
}

export interface ListCurveQuoteVersionsRequest {
    key: CurveQuoteKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveQuoteVersionsFilter | null;
}

export interface ListCurveQuoteVersionsResponse {
    result: Result;
    versions: CurveQuote[];
    total: number;
}

export interface GetCurveQuoteVersionRequest {
    key: CurveQuoteVersionKey;
}

export interface GetCurveQuoteVersionResponse {
    result: Result;
    version: CurveQuote | null;
}

export const subjects = {
    list_curve_quotes_request: 'refdata.v1.curve_quotes.list',
    get_curve_quote_request: 'refdata.v1.curve_quotes.get',
    get_many_curve_quotes_request: 'refdata.v1.curve_quotes.get_many',
    put_curve_quote_request: 'refdata.v1.curve_quotes.put',
    put_many_curve_quotes_request: 'refdata.v1.curve_quotes.put_many',
    delete_curve_quote_request: 'refdata.v1.curve_quotes.delete',
    delete_many_curve_quotes_request: 'refdata.v1.curve_quotes.delete_many',
    list_curve_quote_versions_request: 'refdata.v1.curve_quotes_versions.list',
    get_curve_quote_version_request: 'refdata.v1.curve_quotes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_quotes_request: true,
    get_curve_quote_request: true,
    get_many_curve_quotes_request: true,
    put_curve_quote_request: true,
    put_many_curve_quotes_request: true,
    delete_curve_quote_request: true,
    delete_many_curve_quotes_request: true,
    list_curve_quote_versions_request: true,
    get_curve_quote_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_quotes_events.created',
    updated: 'refdata.v1.curve_quotes_events.updated',
    deleted: 'refdata.v1.curve_quotes_events.deleted',
} as const;
