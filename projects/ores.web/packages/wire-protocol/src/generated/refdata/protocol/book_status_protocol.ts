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
import type { BookStatus } from '../domain/book_status.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface BookStatusKey {
    code: string;
}

export interface BookStatusWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface BookStatusChange {
    write: BookStatusWrite;
    precondition: Precondition;
}

export interface BookStatusRemoval {
    key: BookStatusKey;
    precondition: Precondition;
}

export interface BookStatusLookup {
    key: BookStatusKey;
    book_status: BookStatus | null;
}

export interface BookStatusEvent {
    event_id: string;
    key: BookStatusKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface BookStatusVersionKey {
    book_status: BookStatusKey;
    version: number;
}

export interface BookStatusVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListBookStatusesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListBookStatusesResponse {
    result: Result;
    statuses: BookStatus[];
    total: number;
}

export interface GetBookStatusRequest {
    key: BookStatusKey;
}

export interface GetBookStatusResponse {
    result: Result;
    book_status: BookStatus | null;
}

export interface GetManyBookStatusesRequest {
    keys: BookStatusKey[];
}

export interface GetManyBookStatusesResponse {
    result: Result;
    entries: BookStatusLookup[];
}

export interface PutBookStatusRequest {
    change: BookStatusChange;
    intent: ChangeIntent;
}

export interface PutBookStatusResponse {
    result: Result;
    book_status: BookStatus;
}

export interface PutManyBookStatusesRequest {
    changes: BookStatusChange[];
    intent: ChangeIntent;
}

export interface PutManyBookStatusesResponse {
    result: Result;
    statuses: BookStatus[];
}

export interface DeleteBookStatusRequest {
    removal: BookStatusRemoval;
    intent: ChangeIntent;
}

export interface DeleteBookStatusResponse {
    result: Result;
}

export interface DeleteManyBookStatusesRequest {
    removals: BookStatusRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyBookStatusesResponse {
    result: Result;
}

export interface ListBookStatusVersionsRequest {
    key: BookStatusKey;
    offset: number;
    limit: number;
    order: Order;
    filter: BookStatusVersionsFilter | null;
}

export interface ListBookStatusVersionsResponse {
    result: Result;
    versions: BookStatus[];
    total: number;
}

export interface GetBookStatusVersionRequest {
    key: BookStatusVersionKey;
}

export interface GetBookStatusVersionResponse {
    result: Result;
    version: BookStatus;
}

export const subjects = {
    list_book_statuses_request: "refdata.v1.book_statuses.list",
    get_book_status_request: "refdata.v1.book_statuses.get",
    get_many_book_statuses_request: "refdata.v1.book_statuses.get_many",
    put_book_status_request: "refdata.v1.book_statuses.put",
    put_many_book_statuses_request: "refdata.v1.book_statuses.put_many",
    delete_book_status_request: "refdata.v1.book_statuses.delete",
    delete_many_book_statuses_request: "refdata.v1.book_statuses.delete_many",
    list_book_status_versions_request: "refdata.v1.book_statuses_versions.list",
    get_book_status_version_request: "refdata.v1.book_statuses_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_book_statuses_request: true,
    get_book_status_request: true,
    get_many_book_statuses_request: true,
    put_book_status_request: true,
    put_many_book_statuses_request: true,
    delete_book_status_request: true,
    delete_many_book_statuses_request: true,
    list_book_status_versions_request: true,
    get_book_status_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.book_statuses_events.created",
    updated: "refdata.v1.book_statuses_events.updated",
    deleted: "refdata.v1.book_statuses_events.deleted",
} as const;
