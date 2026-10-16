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
import type { TradeGenerationBook } from '../domain/trade_generation_book.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface TradeGenerationBookKey {
    id: string;
}

export interface TradeGenerationBookWrite {
    id: string;
    config_id: string;
    book_id: string;
}

export interface TradeGenerationBookChange {
    write: TradeGenerationBookWrite;
    precondition: Precondition;
}

export interface TradeGenerationBookRemoval {
    key: TradeGenerationBookKey;
    precondition: Precondition;
}

export interface TradeGenerationBookLookup {
    key: TradeGenerationBookKey;
    trade_generation_book: TradeGenerationBook | null;
}

export interface TradeGenerationBooksFilter {
    config_id: string | null;
    id_one_of: string[] | null;
    config_id_one_of: string[] | null;
}

export interface TradeGenerationBookEvent {
    event_id: string;
    key: TradeGenerationBookKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface TradeGenerationBookVersionKey {
    trade_generation_book: TradeGenerationBookKey;
    version: number;
}

export interface TradeGenerationBookVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListTradeGenerationBooksRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: TradeGenerationBooksFilter | null;
    as_of: string | null;
}

export interface ListTradeGenerationBooksResponse {
    result: Result;
    trade_generation_books: TradeGenerationBook[];
    total: number;
}

export interface GetTradeGenerationBookRequest {
    key: TradeGenerationBookKey;
}

export interface GetTradeGenerationBookResponse {
    result: Result;
    trade_generation_book: TradeGenerationBook | null;
}

export interface GetManyTradeGenerationBooksRequest {
    keys: TradeGenerationBookKey[];
}

export interface GetManyTradeGenerationBooksResponse {
    result: Result;
    entries: TradeGenerationBookLookup[];
}

export interface PutTradeGenerationBookRequest {
    change: TradeGenerationBookChange;
    intent: ChangeIntent;
}

export interface PutTradeGenerationBookResponse {
    result: Result;
    trade_generation_book: TradeGenerationBook | null;
}

export interface PutManyTradeGenerationBooksRequest {
    changes: TradeGenerationBookChange[];
    intent: ChangeIntent;
}

export interface PutManyTradeGenerationBooksResponse {
    result: Result;
    trade_generation_books: TradeGenerationBook[];
}

export interface DeleteTradeGenerationBookRequest {
    removal: TradeGenerationBookRemoval;
    intent: ChangeIntent;
}

export interface DeleteTradeGenerationBookResponse {
    result: Result;
}

export interface DeleteManyTradeGenerationBooksRequest {
    removals: TradeGenerationBookRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyTradeGenerationBooksResponse {
    result: Result;
}

export interface ListTradeGenerationBookVersionsRequest {
    key: TradeGenerationBookKey;
    offset: number;
    limit: number;
    order: Order;
    filter: TradeGenerationBookVersionsFilter | null;
}

export interface ListTradeGenerationBookVersionsResponse {
    result: Result;
    versions: TradeGenerationBook[];
    total: number;
}

export interface GetTradeGenerationBookVersionRequest {
    key: TradeGenerationBookVersionKey;
}

export interface GetTradeGenerationBookVersionResponse {
    result: Result;
    version: TradeGenerationBook | null;
}

export const subjects = {
    list_trade_generation_books_request: 'synthetic.v1.trade_generation_books.list',
    get_trade_generation_book_request: 'synthetic.v1.trade_generation_books.get',
    get_many_trade_generation_books_request: 'synthetic.v1.trade_generation_books.get_many',
    put_trade_generation_book_request: 'synthetic.v1.trade_generation_books.put',
    put_many_trade_generation_books_request: 'synthetic.v1.trade_generation_books.put_many',
    delete_trade_generation_book_request: 'synthetic.v1.trade_generation_books.delete',
    delete_many_trade_generation_books_request: 'synthetic.v1.trade_generation_books.delete_many',
    list_trade_generation_book_versions_request:
        'synthetic.v1.trade_generation_books_versions.list',
    get_trade_generation_book_version_request: 'synthetic.v1.trade_generation_books_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_trade_generation_books_request: true,
    get_trade_generation_book_request: true,
    get_many_trade_generation_books_request: true,
    put_trade_generation_book_request: true,
    put_many_trade_generation_books_request: true,
    delete_trade_generation_book_request: true,
    delete_many_trade_generation_books_request: true,
    list_trade_generation_book_versions_request: true,
    get_trade_generation_book_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.trade_generation_books_events.created',
    updated: 'synthetic.v1.trade_generation_books_events.updated',
    deleted: 'synthetic.v1.trade_generation_books_events.deleted',
} as const;
