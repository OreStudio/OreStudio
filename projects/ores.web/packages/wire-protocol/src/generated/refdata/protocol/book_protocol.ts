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
import type { Book } from '../domain/book.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface BookKey {
    name: string;
}

export interface BookWrite {
    id: string;
    name: string;
    description: string;
    parent_portfolio_id: string;
    owner_unit_id: string | null;
    functional_currency: string;
    gl_account_ref: string;
    cost_center: string;
    book_status: string;
    regulatory_book_type: string;
    is_sweepable: boolean;
    rates_centre_code: string;
}

export interface BookChange {
    write: BookWrite;
    precondition: Precondition;
}

export interface BookRemoval {
    key: BookKey;
    precondition: Precondition;
}

export interface BookLookup {
    key: BookKey;
    book: Book | null;
}

export interface BooksFilter {
    parent_portfolio_id: string | null;
}

export interface BookEvent {
    event_id: string;
    key: BookKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BookVersionKey {
    book: BookKey;
    version: number;
}

export interface BookVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBooksRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: BooksFilter | null;
}

export interface ListBooksResponse {
    result: Result;
    books: Book[];
    total: number;
}

export interface GetBookRequest {
    key: BookKey;
}

export interface GetBookResponse {
    result: Result;
    book: Book | null;
}

export interface GetManyBooksRequest {
    keys: BookKey[];
}

export interface GetManyBooksResponse {
    result: Result;
    entries: BookLookup[];
}

export interface PutBookRequest {
    change: BookChange;
    intent: ChangeIntent;
}

export interface PutBookResponse {
    result: Result;
    book: Book;
}

export interface PutManyBooksRequest {
    changes: BookChange[];
    intent: ChangeIntent;
}

export interface PutManyBooksResponse {
    result: Result;
    books: Book[];
}

export interface DeleteBookRequest {
    removal: BookRemoval;
    intent: ChangeIntent;
}

export interface DeleteBookResponse {
    result: Result;
}

export interface DeleteManyBooksRequest {
    removals: BookRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBooksResponse {
    result: Result;
}

export interface ListByParentPortfolioIdBooksRequest {
    parent_portfolio_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: BooksFilter | null;
}

export interface ListByParentPortfolioIdBooksResponse {
    result: Result;
    books: Book[];
    total: number;
}

export interface ListBookVersionsRequest {
    key: BookKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BookVersionsFilter | null;
}

export interface ListBookVersionsResponse {
    result: Result;
    versions: Book[];
    total: number;
}

export interface GetBookVersionRequest {
    key: BookVersionKey;
}

export interface GetBookVersionResponse {
    result: Result;
    version: Book;
}

export const subjects = {
    list_books_request: "refdata.v1.books.list",
    get_book_request: "refdata.v1.books.get",
    get_many_books_request: "refdata.v1.books.get_many",
    put_book_request: "refdata.v1.books.put",
    put_many_books_request: "refdata.v1.books.put_many",
    delete_book_request: "refdata.v1.books.delete",
    delete_many_books_request: "refdata.v1.books.delete_many",
    list_by_parent_portfolio_id_books_request: "refdata.v1.books.list_by_parent_portfolio_id",
    list_book_versions_request: "refdata.v1.books_versions.list",
    get_book_version_request: "refdata.v1.books_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_books_request: true,
    get_book_request: true,
    get_many_books_request: true,
    put_book_request: true,
    put_many_books_request: true,
    delete_book_request: true,
    delete_many_books_request: true,
    list_by_parent_portfolio_id_books_request: true,
    list_book_versions_request: true,
    get_book_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.books_events.created",
    updated: "refdata.v1.books_events.updated",
    deleted: "refdata.v1.books_events.deleted",
} as const;
