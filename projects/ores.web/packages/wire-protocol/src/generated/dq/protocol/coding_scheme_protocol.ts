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
import type { CodingScheme } from '../domain/coding_scheme.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CodingSchemeKey {
    code: string;
}

export interface CodingSchemeWrite {
    code: string;
    name: string;
    authority_type: string;
    subject_area_name: string;
    domain_name: string;
    uri: string;
    description: string;
}

export interface CodingSchemeChange {
    write: CodingSchemeWrite;
    precondition: Precondition;
}

export interface CodingSchemeRemoval {
    key: CodingSchemeKey;
    precondition: Precondition;
}

export interface CodingSchemeLookup {
    key: CodingSchemeKey;
    coding_scheme: CodingScheme | null;
}

export interface CodingSchemeEvent {
    event_id: string;
    key: CodingSchemeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CodingSchemeVersionKey {
    coding_scheme: CodingSchemeKey;
    version: number;
}

export interface CodingSchemeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCodingSchemesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCodingSchemesResponse {
    result: Result;
    schemes: CodingScheme[];
    total: number;
}

export interface GetCodingSchemeRequest {
    key: CodingSchemeKey;
}

export interface GetCodingSchemeResponse {
    result: Result;
    coding_scheme: CodingScheme | null;
}

export interface GetManyCodingSchemesRequest {
    keys: CodingSchemeKey[];
}

export interface GetManyCodingSchemesResponse {
    result: Result;
    entries: CodingSchemeLookup[];
}

export interface PutCodingSchemeRequest {
    change: CodingSchemeChange;
    intent: ChangeIntent;
}

export interface PutCodingSchemeResponse {
    result: Result;
    coding_scheme: CodingScheme | null;
}

export interface PutManyCodingSchemesRequest {
    changes: CodingSchemeChange[];
    intent: ChangeIntent;
}

export interface PutManyCodingSchemesResponse {
    result: Result;
    schemes: CodingScheme[];
}

export interface DeleteCodingSchemeRequest {
    removal: CodingSchemeRemoval;
    intent: ChangeIntent;
}

export interface DeleteCodingSchemeResponse {
    result: Result;
}

export interface DeleteManyCodingSchemesRequest {
    removals: CodingSchemeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCodingSchemesResponse {
    result: Result;
}

export interface ListCodingSchemeVersionsRequest {
    key: CodingSchemeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CodingSchemeVersionsFilter | null;
}

export interface ListCodingSchemeVersionsResponse {
    result: Result;
    versions: CodingScheme[];
    total: number;
}

export interface GetCodingSchemeVersionRequest {
    key: CodingSchemeVersionKey;
}

export interface GetCodingSchemeVersionResponse {
    result: Result;
    version: CodingScheme | null;
}

export const subjects = {
    list_coding_schemes_request: 'dq.v1.coding_schemes.list',
    get_coding_scheme_request: 'dq.v1.coding_schemes.get',
    get_many_coding_schemes_request: 'dq.v1.coding_schemes.get_many',
    put_coding_scheme_request: 'dq.v1.coding_schemes.put',
    put_many_coding_schemes_request: 'dq.v1.coding_schemes.put_many',
    delete_coding_scheme_request: 'dq.v1.coding_schemes.delete',
    delete_many_coding_schemes_request: 'dq.v1.coding_schemes.delete_many',
    list_coding_scheme_versions_request: 'dq.v1.coding_schemes_versions.list',
    get_coding_scheme_version_request: 'dq.v1.coding_schemes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_coding_schemes_request: true,
    get_coding_scheme_request: true,
    get_many_coding_schemes_request: true,
    put_coding_scheme_request: true,
    put_many_coding_schemes_request: true,
    delete_coding_scheme_request: true,
    delete_many_coding_schemes_request: true,
    list_coding_scheme_versions_request: true,
    get_coding_scheme_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'dq.v1.coding_schemes_events.created',
    updated: 'dq.v1.coding_schemes_events.updated',
    deleted: 'dq.v1.coding_schemes_events.deleted',
} as const;
