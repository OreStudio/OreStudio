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
import type { NettingSetIdentifier } from '../domain/netting_set_identifier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface NettingSetIdentifierKey {
    id_value: string;
}

export interface NettingSetIdentifierWrite {
    id: string;
    netting_set_id: string;
    id_scheme: string;
    id_value: string;
    description: string;
}

export interface NettingSetIdentifierChange {
    write: NettingSetIdentifierWrite;
    precondition: Precondition;
}

export interface NettingSetIdentifierRemoval {
    key: NettingSetIdentifierKey;
    precondition: Precondition;
}

export interface NettingSetIdentifierLookup {
    key: NettingSetIdentifierKey;
    netting_set_identifier: NettingSetIdentifier | null;
}

export interface NettingSetIdentifiersFilter {
    netting_set_id: string | null;
    id_one_of: string[] | null;
    netting_set_id_one_of: string[] | null;
}

export interface NettingSetIdentifierEvent {
    event_id: string;
    key: NettingSetIdentifierKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface NettingSetIdentifierVersionKey {
    netting_set_identifier: NettingSetIdentifierKey;
    version: number;
}

export interface NettingSetIdentifierVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListNettingSetIdentifiersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: NettingSetIdentifiersFilter | null;
    as_of: string | null;
}

export interface ListNettingSetIdentifiersResponse {
    result: Result;
    netting_set_identifiers: NettingSetIdentifier[];
    total: number;
}

export interface GetNettingSetIdentifierRequest {
    key: NettingSetIdentifierKey;
}

export interface GetNettingSetIdentifierResponse {
    result: Result;
    netting_set_identifier: NettingSetIdentifier | null;
}

export interface GetManyNettingSetIdentifiersRequest {
    keys: NettingSetIdentifierKey[];
}

export interface GetManyNettingSetIdentifiersResponse {
    result: Result;
    entries: NettingSetIdentifierLookup[];
}

export interface PutNettingSetIdentifierRequest {
    change: NettingSetIdentifierChange;
    intent: ChangeIntent;
}

export interface PutNettingSetIdentifierResponse {
    result: Result;
    netting_set_identifier: NettingSetIdentifier | null;
}

export interface PutManyNettingSetIdentifiersRequest {
    changes: NettingSetIdentifierChange[];
    intent: ChangeIntent;
}

export interface PutManyNettingSetIdentifiersResponse {
    result: Result;
    netting_set_identifiers: NettingSetIdentifier[];
}

export interface DeleteNettingSetIdentifierRequest {
    removal: NettingSetIdentifierRemoval;
    intent: ChangeIntent;
}

export interface DeleteNettingSetIdentifierResponse {
    result: Result;
}

export interface DeleteManyNettingSetIdentifiersRequest {
    removals: NettingSetIdentifierRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyNettingSetIdentifiersResponse {
    result: Result;
}

export interface ListByNettingSetIdNettingSetIdentifiersRequest {
    netting_set_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: NettingSetIdentifiersFilter | null;
}

export interface ListByNettingSetIdNettingSetIdentifiersResponse {
    result: Result;
    netting_set_identifiers: NettingSetIdentifier[];
    total: number;
}

export interface ListNettingSetIdentifierVersionsRequest {
    key: NettingSetIdentifierKey;
    offset: number;
    limit: number;
    order: Order;
    filter: NettingSetIdentifierVersionsFilter | null;
}

export interface ListNettingSetIdentifierVersionsResponse {
    result: Result;
    versions: NettingSetIdentifier[];
    total: number;
}

export interface GetNettingSetIdentifierVersionRequest {
    key: NettingSetIdentifierVersionKey;
}

export interface GetNettingSetIdentifierVersionResponse {
    result: Result;
    version: NettingSetIdentifier | null;
}

export const subjects = {
    list_netting_set_identifiers_request: 'refdata.v1.netting_set_identifiers.list',
    get_netting_set_identifier_request: 'refdata.v1.netting_set_identifiers.get',
    get_many_netting_set_identifiers_request: 'refdata.v1.netting_set_identifiers.get_many',
    put_netting_set_identifier_request: 'refdata.v1.netting_set_identifiers.put',
    put_many_netting_set_identifiers_request: 'refdata.v1.netting_set_identifiers.put_many',
    delete_netting_set_identifier_request: 'refdata.v1.netting_set_identifiers.delete',
    delete_many_netting_set_identifiers_request: 'refdata.v1.netting_set_identifiers.delete_many',
    list_by_netting_set_id_netting_set_identifiers_request:
        'refdata.v1.netting_set_identifiers.list_by_netting_set_id',
    list_netting_set_identifier_versions_request:
        'refdata.v1.netting_set_identifiers_versions.list',
    get_netting_set_identifier_version_request: 'refdata.v1.netting_set_identifiers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_netting_set_identifiers_request: true,
    get_netting_set_identifier_request: true,
    get_many_netting_set_identifiers_request: true,
    put_netting_set_identifier_request: true,
    put_many_netting_set_identifiers_request: true,
    delete_netting_set_identifier_request: true,
    delete_many_netting_set_identifiers_request: true,
    list_by_netting_set_id_netting_set_identifiers_request: true,
    list_netting_set_identifier_versions_request: true,
    get_netting_set_identifier_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.netting_set_identifiers_events.created',
    updated: 'refdata.v1.netting_set_identifiers_events.updated',
    deleted: 'refdata.v1.netting_set_identifiers_events.deleted',
} as const;
