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
import type { CreditRating } from '../domain/credit_rating.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CreditRatingKey {
    code: string;
}

export interface CreditRatingWrite {
    code: string;
    name: string;
    display_order: number;
}

export interface CreditRatingChange {
    write: CreditRatingWrite;
    precondition: Precondition;
}

export interface CreditRatingRemoval {
    key: CreditRatingKey;
    precondition: Precondition;
}

export interface CreditRatingLookup {
    key: CreditRatingKey;
    credit_rating: CreditRating | null;
}

export interface CreditRatingEvent {
    event_id: string;
    key: CreditRatingKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CreditRatingVersionKey {
    credit_rating: CreditRatingKey;
    version: number;
}

export interface CreditRatingVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCreditRatingsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCreditRatingsResponse {
    result: Result;
    ratings: CreditRating[];
    total: number;
}

export interface GetCreditRatingRequest {
    key: CreditRatingKey;
}

export interface GetCreditRatingResponse {
    result: Result;
    credit_rating: CreditRating | null;
}

export interface GetManyCreditRatingsRequest {
    keys: CreditRatingKey[];
}

export interface GetManyCreditRatingsResponse {
    result: Result;
    entries: CreditRatingLookup[];
}

export interface PutCreditRatingRequest {
    change: CreditRatingChange;
    intent: ChangeIntent;
}

export interface PutCreditRatingResponse {
    result: Result;
    credit_rating: CreditRating;
}

export interface PutManyCreditRatingsRequest {
    changes: CreditRatingChange[];
    intent: ChangeIntent;
}

export interface PutManyCreditRatingsResponse {
    result: Result;
    ratings: CreditRating[];
}

export interface DeleteCreditRatingRequest {
    removal: CreditRatingRemoval;
    intent: ChangeIntent;
}

export interface DeleteCreditRatingResponse {
    result: Result;
}

export interface DeleteManyCreditRatingsRequest {
    removals: CreditRatingRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCreditRatingsResponse {
    result: Result;
}

export interface ListCreditRatingVersionsRequest {
    key: CreditRatingKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CreditRatingVersionsFilter | null;
}

export interface ListCreditRatingVersionsResponse {
    result: Result;
    versions: CreditRating[];
    total: number;
}

export interface GetCreditRatingVersionRequest {
    key: CreditRatingVersionKey;
}

export interface GetCreditRatingVersionResponse {
    result: Result;
    version: CreditRating;
}

export const subjects = {
    list_credit_ratings_request: "analytics.v1.credit_ratings.list",
    get_credit_rating_request: "analytics.v1.credit_ratings.get",
    get_many_credit_ratings_request: "analytics.v1.credit_ratings.get_many",
    put_credit_rating_request: "analytics.v1.credit_ratings.put",
    put_many_credit_ratings_request: "analytics.v1.credit_ratings.put_many",
    delete_credit_rating_request: "analytics.v1.credit_ratings.delete",
    delete_many_credit_ratings_request: "analytics.v1.credit_ratings.delete_many",
    list_credit_rating_versions_request: "analytics.v1.credit_ratings_versions.list",
    get_credit_rating_version_request: "analytics.v1.credit_ratings_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_credit_ratings_request: true,
    get_credit_rating_request: true,
    get_many_credit_ratings_request: true,
    put_credit_rating_request: true,
    put_many_credit_ratings_request: true,
    delete_credit_rating_request: true,
    delete_many_credit_ratings_request: true,
    list_credit_rating_versions_request: true,
    get_credit_rating_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "analytics.v1.credit_ratings_events.created",
    updated: "analytics.v1.credit_ratings_events.updated",
    deleted: "analytics.v1.credit_ratings_events.deleted",
} as const;
