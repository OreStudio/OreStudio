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
import type { ConventionKindTerm } from '../domain/convention_kind_term.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ConventionKindTermKey {
    code: string;
}

export interface ConventionKindTermWrite {
    code: string;
    kind_code: string;
    entity_name: string;
    column_name: string;
    schema_name: string;
    xpath: string;
    position: number;
}

export interface ConventionKindTermChange {
    write: ConventionKindTermWrite;
    precondition: Precondition;
}

export interface ConventionKindTermRemoval {
    key: ConventionKindTermKey;
    precondition: Precondition;
}

export interface ConventionKindTermLookup {
    key: ConventionKindTermKey;
    convention_kind_term: ConventionKindTerm | null;
}

export interface ConventionKindTermsFilter {
    code_one_of: string[] | null;
}

export interface ConventionKindTermEvent {
    event_id: string;
    key: ConventionKindTermKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ConventionKindTermVersionKey {
    convention_kind_term: ConventionKindTermKey;
    version: number;
}

export interface ConventionKindTermVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListConventionKindTermsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionKindTermsFilter | null;
    as_of: string | null;
}

export interface ListConventionKindTermsResponse {
    result: Result;
    terms: ConventionKindTerm[];
    total: number;
}

export interface GetConventionKindTermRequest {
    key: ConventionKindTermKey;
}

export interface GetConventionKindTermResponse {
    result: Result;
    convention_kind_term: ConventionKindTerm | null;
}

export interface GetManyConventionKindTermsRequest {
    keys: ConventionKindTermKey[];
}

export interface GetManyConventionKindTermsResponse {
    result: Result;
    entries: ConventionKindTermLookup[];
}

export interface PutConventionKindTermRequest {
    change: ConventionKindTermChange;
    intent: ChangeIntent;
}

export interface PutConventionKindTermResponse {
    result: Result;
    convention_kind_term: ConventionKindTerm | null;
}

export interface PutManyConventionKindTermsRequest {
    changes: ConventionKindTermChange[];
    intent: ChangeIntent;
}

export interface PutManyConventionKindTermsResponse {
    result: Result;
    terms: ConventionKindTerm[];
}

export interface DeleteConventionKindTermRequest {
    removal: ConventionKindTermRemoval;
    intent: ChangeIntent;
}

export interface DeleteConventionKindTermResponse {
    result: Result;
}

export interface DeleteManyConventionKindTermsRequest {
    removals: ConventionKindTermRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyConventionKindTermsResponse {
    result: Result;
}

export interface ListConventionKindTermVersionsRequest {
    key: ConventionKindTermKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ConventionKindTermVersionsFilter | null;
}

export interface ListConventionKindTermVersionsResponse {
    result: Result;
    versions: ConventionKindTerm[];
    total: number;
}

export interface GetConventionKindTermVersionRequest {
    key: ConventionKindTermVersionKey;
}

export interface GetConventionKindTermVersionResponse {
    result: Result;
    version: ConventionKindTerm | null;
}

export const subjects = {
    list_convention_kind_terms_request: 'refdata.v1.convention_kind_terms.list',
    get_convention_kind_term_request: 'refdata.v1.convention_kind_terms.get',
    get_many_convention_kind_terms_request: 'refdata.v1.convention_kind_terms.get_many',
    put_convention_kind_term_request: 'refdata.v1.convention_kind_terms.put',
    put_many_convention_kind_terms_request: 'refdata.v1.convention_kind_terms.put_many',
    delete_convention_kind_term_request: 'refdata.v1.convention_kind_terms.delete',
    delete_many_convention_kind_terms_request: 'refdata.v1.convention_kind_terms.delete_many',
    list_convention_kind_term_versions_request: 'refdata.v1.convention_kind_terms_versions.list',
    get_convention_kind_term_version_request: 'refdata.v1.convention_kind_terms_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_convention_kind_terms_request: true,
    get_convention_kind_term_request: true,
    get_many_convention_kind_terms_request: true,
    put_convention_kind_term_request: true,
    put_many_convention_kind_terms_request: true,
    delete_convention_kind_term_request: true,
    delete_many_convention_kind_terms_request: true,
    list_convention_kind_term_versions_request: true,
    get_convention_kind_term_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.convention_kind_terms_events.created',
    updated: 'refdata.v1.convention_kind_terms_events.updated',
    deleted: 'refdata.v1.convention_kind_terms_events.deleted',
} as const;
