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
import type { StructureIdentifier } from '../domain/structure_identifier.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface StructureIdentifierKey {
    structure_id: string;
    id_type: string;
}

export interface StructureIdentifierWrite {
    structure_id: string;
    id_type: string;
    id_value: string;
    issuing_party_id: string | null;
}

export interface StructureIdentifierChange {
    write: StructureIdentifierWrite;
    precondition: Precondition;
}

export interface StructureIdentifierRemoval {
    key: StructureIdentifierKey;
    precondition: Precondition;
}

export interface StructureIdentifierLookup {
    key: StructureIdentifierKey;
    structure_identifier: StructureIdentifier | null;
}

export interface StructureIdentifiersFilter {
    structure_id: string | null;
    structure_id_one_of: string[] | null;
}

export interface StructureIdentifierEvent {
    event_id: string;
    key: StructureIdentifierKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StructureIdentifierVersionKey {
    structure_identifier: StructureIdentifierKey;
    version: number;
}

export interface StructureIdentifierVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStructureIdentifiersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StructureIdentifiersFilter | null;
    as_of: string | null;
}

export interface ListStructureIdentifiersResponse {
    result: Result;
    identifiers: StructureIdentifier[];
    total: number;
}

export interface GetStructureIdentifierRequest {
    key: StructureIdentifierKey;
}

export interface GetStructureIdentifierResponse {
    result: Result;
    structure_identifier: StructureIdentifier | null;
}

export interface GetManyStructureIdentifiersRequest {
    keys: StructureIdentifierKey[];
}

export interface GetManyStructureIdentifiersResponse {
    result: Result;
    entries: StructureIdentifierLookup[];
}

export interface PutStructureIdentifierRequest {
    change: StructureIdentifierChange;
    intent: ChangeIntent;
}

export interface PutStructureIdentifierResponse {
    result: Result;
    structure_identifier: StructureIdentifier | null;
}

export interface PutManyStructureIdentifiersRequest {
    changes: StructureIdentifierChange[];
    intent: ChangeIntent;
}

export interface PutManyStructureIdentifiersResponse {
    result: Result;
    identifiers: StructureIdentifier[];
}

export interface DeleteStructureIdentifierRequest {
    removal: StructureIdentifierRemoval;
    intent: ChangeIntent;
}

export interface DeleteStructureIdentifierResponse {
    result: Result;
}

export interface DeleteManyStructureIdentifiersRequest {
    removals: StructureIdentifierRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStructureIdentifiersResponse {
    result: Result;
}

export interface ListByStructureIdStructureIdentifiersRequest {
    structure_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: StructureIdentifiersFilter | null;
}

export interface ListByStructureIdStructureIdentifiersResponse {
    result: Result;
    identifiers: StructureIdentifier[];
    total: number;
}

export interface ListStructureIdentifierVersionsRequest {
    key: StructureIdentifierKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StructureIdentifierVersionsFilter | null;
}

export interface ListStructureIdentifierVersionsResponse {
    result: Result;
    versions: StructureIdentifier[];
    total: number;
}

export interface GetStructureIdentifierVersionRequest {
    key: StructureIdentifierVersionKey;
}

export interface GetStructureIdentifierVersionResponse {
    result: Result;
    version: StructureIdentifier | null;
}

export const subjects = {
    list_structure_identifiers_request: 'trading.v1.structure_identifiers.list',
    get_structure_identifier_request: 'trading.v1.structure_identifiers.get',
    get_many_structure_identifiers_request: 'trading.v1.structure_identifiers.get_many',
    put_structure_identifier_request: 'trading.v1.structure_identifiers.put',
    put_many_structure_identifiers_request: 'trading.v1.structure_identifiers.put_many',
    delete_structure_identifier_request: 'trading.v1.structure_identifiers.delete',
    delete_many_structure_identifiers_request: 'trading.v1.structure_identifiers.delete_many',
    list_by_structure_id_structure_identifiers_request:
        'trading.v1.structure_identifiers.list_by_structure_id',
    list_structure_identifier_versions_request: 'trading.v1.structure_identifiers_versions.list',
    get_structure_identifier_version_request: 'trading.v1.structure_identifiers_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_structure_identifiers_request: true,
    get_structure_identifier_request: true,
    get_many_structure_identifiers_request: true,
    put_structure_identifier_request: true,
    put_many_structure_identifiers_request: true,
    delete_structure_identifier_request: true,
    delete_many_structure_identifiers_request: true,
    list_by_structure_id_structure_identifiers_request: true,
    list_structure_identifier_versions_request: true,
    get_structure_identifier_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.structure_identifiers_events.created',
    updated: 'trading.v1.structure_identifiers_events.updated',
    deleted: 'trading.v1.structure_identifiers_events.deleted',
} as const;
