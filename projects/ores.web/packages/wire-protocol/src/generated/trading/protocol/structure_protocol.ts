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
import type { Structure } from '../domain/structure.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface StructureKey {
    id: string;
}

export interface StructureWrite {
    id: string;
    counterparty_id: string;
    kind: string;
    template_code: string;
    parent_structure_id: string;
}

export interface StructureChange {
    write: StructureWrite;
    precondition: Precondition;
}

export interface StructureRemoval {
    key: StructureKey;
    precondition: Precondition;
}

export interface StructureLookup {
    key: StructureKey;
    structure: Structure | null;
}

export interface StructuresFilter {
    id_one_of: string[] | null;
}

export interface StructureEvent {
    event_id: string;
    key: StructureKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface StructureVersionKey {
    structure: StructureKey;
    version: number;
}

export interface StructureVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListStructuresRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: StructuresFilter | null;
    as_of: string | null;
}

export interface ListStructuresResponse {
    result: Result;
    structures: Structure[];
    total: number;
}

export interface GetStructureRequest {
    key: StructureKey;
}

export interface GetStructureResponse {
    result: Result;
    structure: Structure | null;
}

export interface GetManyStructuresRequest {
    keys: StructureKey[];
}

export interface GetManyStructuresResponse {
    result: Result;
    entries: StructureLookup[];
}

export interface PutStructureRequest {
    change: StructureChange;
    intent: ChangeIntent;
}

export interface PutStructureResponse {
    result: Result;
    structure: Structure | null;
}

export interface PutManyStructuresRequest {
    changes: StructureChange[];
    intent: ChangeIntent;
}

export interface PutManyStructuresResponse {
    result: Result;
    structures: Structure[];
}

export interface DeleteStructureRequest {
    removal: StructureRemoval;
    intent: ChangeIntent;
}

export interface DeleteStructureResponse {
    result: Result;
}

export interface DeleteManyStructuresRequest {
    removals: StructureRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyStructuresResponse {
    result: Result;
}

export interface ListStructureVersionsRequest {
    key: StructureKey;
    offset: number;
    limit: number;
    order: Order;
    filter: StructureVersionsFilter | null;
}

export interface ListStructureVersionsResponse {
    result: Result;
    versions: Structure[];
    total: number;
}

export interface GetStructureVersionRequest {
    key: StructureVersionKey;
}

export interface GetStructureVersionResponse {
    result: Result;
    version: Structure | null;
}

export const subjects = {
    list_structures_request: 'trading.v1.structures.list',
    get_structure_request: 'trading.v1.structures.get',
    get_many_structures_request: 'trading.v1.structures.get_many',
    put_structure_request: 'trading.v1.structures.put',
    put_many_structures_request: 'trading.v1.structures.put_many',
    delete_structure_request: 'trading.v1.structures.delete',
    delete_many_structures_request: 'trading.v1.structures.delete_many',
    list_structure_versions_request: 'trading.v1.structures_versions.list',
    get_structure_version_request: 'trading.v1.structures_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_structures_request: true,
    get_structure_request: true,
    get_many_structures_request: true,
    put_structure_request: true,
    put_many_structures_request: true,
    delete_structure_request: true,
    delete_many_structures_request: true,
    list_structure_versions_request: true,
    get_structure_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.structures_events.created',
    updated: 'trading.v1.structures_events.updated',
    deleted: 'trading.v1.structures_events.deleted',
} as const;
