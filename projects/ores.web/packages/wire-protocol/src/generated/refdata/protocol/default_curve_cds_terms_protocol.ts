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
import type { DefaultCurveCdsTerms } from '../domain/default_curve_cds_terms.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DefaultCurveCdsTermsKey {
    id: string;
}

export interface DefaultCurveCdsTermsWrite {
    id: string;
    default_curve_configuration_id: string;
    start_date: string | null;
    running_spread: string | null;
    index_term: string | null;
    imply_default_from_market: string | null;
    price_is_upfront: string | null;
}

export interface DefaultCurveCdsTermsChange {
    write: DefaultCurveCdsTermsWrite;
    precondition: Precondition;
}

export interface DefaultCurveCdsTermsRemoval {
    key: DefaultCurveCdsTermsKey;
    precondition: Precondition;
}

export interface DefaultCurveCdsTermsLookup {
    key: DefaultCurveCdsTermsKey;
    default_curve_cds_terms: DefaultCurveCdsTerms | null;
}

export interface DefaultCurveCdsTermsFilter {
    id_one_of: string[] | null;
}

export interface DefaultCurveCdsTermsEvent {
    event_id: string;
    key: DefaultCurveCdsTermsKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DefaultCurveCdsTermsVersionKey {
    default_curve_cds_terms: DefaultCurveCdsTermsKey;
    version: number;
}

export interface DefaultCurveCdsTermsVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDefaultCurveCdsTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveCdsTermsFilter | null;
    as_of: string | null;
}

export interface ListDefaultCurveCdsTermsResponse {
    result: Result;
    cds_terms: DefaultCurveCdsTerms[];
    total: number;
}

export interface GetDefaultCurveCdsTermsRequest {
    key: DefaultCurveCdsTermsKey;
}

export interface GetDefaultCurveCdsTermsResponse {
    result: Result;
    default_curve_cds_terms: DefaultCurveCdsTerms | null;
}

export interface GetManyDefaultCurveCdsTermsRequest {
    keys: DefaultCurveCdsTermsKey[];
}

export interface GetManyDefaultCurveCdsTermsResponse {
    result: Result;
    entries: DefaultCurveCdsTermsLookup[];
}

export interface PutDefaultCurveCdsTermsRequest {
    change: DefaultCurveCdsTermsChange;
    intent: ChangeIntent;
}

export interface PutDefaultCurveCdsTermsResponse {
    result: Result;
    default_curve_cds_terms: DefaultCurveCdsTerms | null;
}

export interface PutManyDefaultCurveCdsTermsRequest {
    changes: DefaultCurveCdsTermsChange[];
    intent: ChangeIntent;
}

export interface PutManyDefaultCurveCdsTermsResponse {
    result: Result;
    cds_terms: DefaultCurveCdsTerms[];
}

export interface DeleteDefaultCurveCdsTermsRequest {
    removal: DefaultCurveCdsTermsRemoval;
    intent: ChangeIntent;
}

export interface DeleteDefaultCurveCdsTermsResponse {
    result: Result;
}

export interface DeleteManyDefaultCurveCdsTermsRequest {
    removals: DefaultCurveCdsTermsRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDefaultCurveCdsTermsResponse {
    result: Result;
}

export interface ListDefaultCurveCdsTermsVersionsRequest {
    key: DefaultCurveCdsTermsKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DefaultCurveCdsTermsVersionsFilter | null;
}

export interface ListDefaultCurveCdsTermsVersionsResponse {
    result: Result;
    versions: DefaultCurveCdsTerms[];
    total: number;
}

export interface GetDefaultCurveCdsTermsVersionRequest {
    key: DefaultCurveCdsTermsVersionKey;
}

export interface GetDefaultCurveCdsTermsVersionResponse {
    result: Result;
    version: DefaultCurveCdsTerms | null;
}

export const subjects = {
    list_default_curve_cds_terms_request: 'refdata.v1.default_curve_cds_terms.list',
    get_default_curve_cds_terms_request: 'refdata.v1.default_curve_cds_terms.get',
    get_many_default_curve_cds_terms_request: 'refdata.v1.default_curve_cds_terms.get_many',
    put_default_curve_cds_terms_request: 'refdata.v1.default_curve_cds_terms.put',
    put_many_default_curve_cds_terms_request: 'refdata.v1.default_curve_cds_terms.put_many',
    delete_default_curve_cds_terms_request: 'refdata.v1.default_curve_cds_terms.delete',
    delete_many_default_curve_cds_terms_request: 'refdata.v1.default_curve_cds_terms.delete_many',
    list_default_curve_cds_terms_versions_request:
        'refdata.v1.default_curve_cds_terms_versions.list',
    get_default_curve_cds_terms_version_request: 'refdata.v1.default_curve_cds_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_default_curve_cds_terms_request: true,
    get_default_curve_cds_terms_request: true,
    get_many_default_curve_cds_terms_request: true,
    put_default_curve_cds_terms_request: true,
    put_many_default_curve_cds_terms_request: true,
    delete_default_curve_cds_terms_request: true,
    delete_many_default_curve_cds_terms_request: true,
    list_default_curve_cds_terms_versions_request: true,
    get_default_curve_cds_terms_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.default_curve_cds_terms_events.created',
    updated: 'refdata.v1.default_curve_cds_terms_events.updated',
    deleted: 'refdata.v1.default_curve_cds_terms_events.deleted',
} as const;
