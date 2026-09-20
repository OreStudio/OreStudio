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

export interface GetBooksRequest {
    offset: number;
    limit: number;
}

export interface GetBooksResponse {
    books: Book[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export interface SaveBookRequest {
    data: Book;
}

export interface SaveBookResponse {
    success: boolean;
    message: string;
}

export interface DeleteBookRequest {
    ids: string[];
}

export interface DeleteBookResponse {
    success: boolean;
    message: string;
}

export interface GetBookHistoryRequest {
    id: string;
}

export interface GetBookHistoryResponse {
    history: Book[];
    success: boolean;
    message: string;
}

export interface GetBooksByParentPortfolioIdRequest {
    parent_portfolio_id: string;
    offset: number;
    limit: number;
}

export interface GetBooksByParentPortfolioIdResponse {
    books: Book[];
    total_available_count: number;
    success: boolean;
    message: string;
}

export const subjects = {
    get_books_request: "refdata.v1.books.list",
    save_book_request: "refdata.v1.books.save",
    delete_book_request: "refdata.v1.books.delete",
    get_book_history_request: "refdata.v1.books.history",
    get_books_by_parent_portfolio_id_request: "refdata.v1.books.list_by_parent_portfolio_id",
} as const;
