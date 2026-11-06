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
import type { DefaultCurveMarketTerms } from '../domain/default_curve_market_terms.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DefaultCurveMarketTermsKey {
    id: string;
}

export interface DefaultCurveMarketTermsWrite {
    id: string;
    default_curve_configuration_id: string;
    discount_curve: string | null;
    conventions: string | null;
    has_quotes: boolean;
}

export interface DefaultCurveMarketTermsChange {
    write: DefaultCurveMarketTermsWrite;
    precondition: Precondition;
}

export interface DefaultCurveMarketTermsRemoval {
    key: DefaultCurveMarketTermsKey;
    precondition: Precondition;
}

export interface DefaultCurveMarketTermsLookup {
    key: DefaultCurveMarketTermsKey;
    default_curve_market_terms: DefaultCurveMarketTerms | null;
}

export interface DefaultCurveMarketTermsFilter {
    id_one_of: string[] | null;
}

export interface DefaultCurveMarketTermsEvent {
    event_id: string;
    key: DefaultCurveMarketTermsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DefaultCurveMarketTermsVersionKey {
    default_curve_market_terms: DefaultCurveMarketTermsKey;
    version: number;
}

export interface DefaultCurveMarketTermsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDefaultCurveMarketTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveMarketTermsFilter | null;
    as_of: string | null;
}

export interface ListDefaultCurveMarketTermsResponse {
    result: Result;
    market_terms: DefaultCurveMarketTerms[];
    total: number;
}

export interface GetDefaultCurveMarketTermsRequest {
    key: DefaultCurveMarketTermsKey;
}

export interface GetDefaultCurveMarketTermsResponse {
    result: Result;
    default_curve_market_terms: DefaultCurveMarketTerms | null;
}

export interface GetManyDefaultCurveMarketTermsRequest {
    keys: DefaultCurveMarketTermsKey[];
}

export interface GetManyDefaultCurveMarketTermsResponse {
    result: Result;
    entries: DefaultCurveMarketTermsLookup[];
}

export interface PutDefaultCurveMarketTermsRequest {
    change: DefaultCurveMarketTermsChange;
    intent: ChangeIntent;
}

export interface PutDefaultCurveMarketTermsResponse {
    result: Result;
    default_curve_market_terms: DefaultCurveMarketTerms | null;
}

export interface PutManyDefaultCurveMarketTermsRequest {
    changes: DefaultCurveMarketTermsChange[];
    intent: ChangeIntent;
}

export interface PutManyDefaultCurveMarketTermsResponse {
    result: Result;
    market_terms: DefaultCurveMarketTerms[];
}

export interface DeleteDefaultCurveMarketTermsRequest {
    removal: DefaultCurveMarketTermsRemoval;
    intent: ChangeIntent;
}

export interface DeleteDefaultCurveMarketTermsResponse {
    result: Result;
}

export interface DeleteManyDefaultCurveMarketTermsRequest {
    removals: DefaultCurveMarketTermsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDefaultCurveMarketTermsResponse {
    result: Result;
}

export interface ListDefaultCurveMarketTermsVersionsRequest {
    key: DefaultCurveMarketTermsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveMarketTermsVersionsFilter | null;
}

export interface ListDefaultCurveMarketTermsVersionsResponse {
    result: Result;
    versions: DefaultCurveMarketTerms[];
    total: number;
}

export interface GetDefaultCurveMarketTermsVersionRequest {
    key: DefaultCurveMarketTermsVersionKey;
}

export interface GetDefaultCurveMarketTermsVersionResponse {
    result: Result;
    version: DefaultCurveMarketTerms | null;
}

export const subjects = {
    list_default_curve_market_terms_request: 'refdata.v1.default_curve_market_terms.list',
    get_default_curve_market_terms_request: 'refdata.v1.default_curve_market_terms.get',
    get_many_default_curve_market_terms_request: 'refdata.v1.default_curve_market_terms.get_many',
    put_default_curve_market_terms_request: 'refdata.v1.default_curve_market_terms.put',
    put_many_default_curve_market_terms_request: 'refdata.v1.default_curve_market_terms.put_many',
    delete_default_curve_market_terms_request: 'refdata.v1.default_curve_market_terms.delete',
    delete_many_default_curve_market_terms_request:
        'refdata.v1.default_curve_market_terms.delete_many',
    list_default_curve_market_terms_versions_request:
        'refdata.v1.default_curve_market_terms_versions.list',
    get_default_curve_market_terms_version_request:
        'refdata.v1.default_curve_market_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_default_curve_market_terms_request: true,
    get_default_curve_market_terms_request: true,
    get_many_default_curve_market_terms_request: true,
    put_default_curve_market_terms_request: true,
    put_many_default_curve_market_terms_request: true,
    delete_default_curve_market_terms_request: true,
    delete_many_default_curve_market_terms_request: true,
    list_default_curve_market_terms_versions_request: true,
    get_default_curve_market_terms_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.default_curve_market_terms.created',
    updated: 'refdata.v1.events.default_curve_market_terms.updated',
    deleted: 'refdata.v1.events.default_curve_market_terms.deleted',
} as const;
