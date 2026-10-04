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
import type { CdsVolatilityTerm } from '../domain/cds_volatility_term.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CdsVolatilityTermKey {
    id: string;
}

export interface CdsVolatilityTermWrite {
    id: string;
    curve_definition_id: string;
    label: string;
    curve: string;
    maturity: string | null;
    position: number;
}

export interface CdsVolatilityTermChange {
    write: CdsVolatilityTermWrite;
    precondition: Precondition;
}

export interface CdsVolatilityTermRemoval {
    key: CdsVolatilityTermKey;
    precondition: Precondition;
}

export interface CdsVolatilityTermLookup {
    key: CdsVolatilityTermKey;
    cds_volatility_term: CdsVolatilityTerm | null;
}

export interface CdsVolatilityTermsFilter {
    id_one_of: string[] | null;
}

export interface CdsVolatilityTermEvent {
    event_id: string;
    key: CdsVolatilityTermKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CdsVolatilityTermVersionKey {
    cds_volatility_term: CdsVolatilityTermKey;
    version: number;
}

export interface CdsVolatilityTermVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCdsVolatilityTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CdsVolatilityTermsFilter | null;
}

export interface ListCdsVolatilityTermsResponse {
    result: Result;
    terms: CdsVolatilityTerm[];
    total: number;
}

export interface GetCdsVolatilityTermRequest {
    key: CdsVolatilityTermKey;
}

export interface GetCdsVolatilityTermResponse {
    result: Result;
    cds_volatility_term: CdsVolatilityTerm | null;
}

export interface GetManyCdsVolatilityTermsRequest {
    keys: CdsVolatilityTermKey[];
}

export interface GetManyCdsVolatilityTermsResponse {
    result: Result;
    entries: CdsVolatilityTermLookup[];
}

export interface PutCdsVolatilityTermRequest {
    change: CdsVolatilityTermChange;
    intent: ChangeIntent;
}

export interface PutCdsVolatilityTermResponse {
    result: Result;
    cds_volatility_term: CdsVolatilityTerm | null;
}

export interface PutManyCdsVolatilityTermsRequest {
    changes: CdsVolatilityTermChange[];
    intent: ChangeIntent;
}

export interface PutManyCdsVolatilityTermsResponse {
    result: Result;
    terms: CdsVolatilityTerm[];
}

export interface DeleteCdsVolatilityTermRequest {
    removal: CdsVolatilityTermRemoval;
    intent: ChangeIntent;
}

export interface DeleteCdsVolatilityTermResponse {
    result: Result;
}

export interface DeleteManyCdsVolatilityTermsRequest {
    removals: CdsVolatilityTermRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCdsVolatilityTermsResponse {
    result: Result;
}

export interface ListCdsVolatilityTermVersionsRequest {
    key: CdsVolatilityTermKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CdsVolatilityTermVersionsFilter | null;
}

export interface ListCdsVolatilityTermVersionsResponse {
    result: Result;
    versions: CdsVolatilityTerm[];
    total: number;
}

export interface GetCdsVolatilityTermVersionRequest {
    key: CdsVolatilityTermVersionKey;
}

export interface GetCdsVolatilityTermVersionResponse {
    result: Result;
    version: CdsVolatilityTerm | null;
}

export const subjects = {
    list_cds_volatility_terms_request: 'refdata.v1.cds_volatility_terms.list',
    get_cds_volatility_term_request: 'refdata.v1.cds_volatility_terms.get',
    get_many_cds_volatility_terms_request: 'refdata.v1.cds_volatility_terms.get_many',
    put_cds_volatility_term_request: 'refdata.v1.cds_volatility_terms.put',
    put_many_cds_volatility_terms_request: 'refdata.v1.cds_volatility_terms.put_many',
    delete_cds_volatility_term_request: 'refdata.v1.cds_volatility_terms.delete',
    delete_many_cds_volatility_terms_request: 'refdata.v1.cds_volatility_terms.delete_many',
    list_cds_volatility_term_versions_request: 'refdata.v1.cds_volatility_terms_versions.list',
    get_cds_volatility_term_version_request: 'refdata.v1.cds_volatility_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cds_volatility_terms_request: true,
    get_cds_volatility_term_request: true,
    get_many_cds_volatility_terms_request: true,
    put_cds_volatility_term_request: true,
    put_many_cds_volatility_terms_request: true,
    delete_cds_volatility_term_request: true,
    delete_many_cds_volatility_terms_request: true,
    list_cds_volatility_term_versions_request: true,
    get_cds_volatility_term_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cds_volatility_terms_events.created',
    updated: 'refdata.v1.cds_volatility_terms_events.updated',
    deleted: 'refdata.v1.cds_volatility_terms_events.deleted',
} as const;
