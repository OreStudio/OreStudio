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
import type { ArtefactType } from '../domain/artefact_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ArtefactTypeKey {
    code: string;
}

export interface ArtefactTypeWrite {
    code: string;
    name: string;
    description: string | null;
    artefact_table: string | null;
    target_table: string | null;
    target_subject: string | null;
    display_order: number;
}

export interface ArtefactTypeChange {
    write: ArtefactTypeWrite;
    precondition: Precondition;
}

export interface ArtefactTypeRemoval {
    key: ArtefactTypeKey;
    precondition: Precondition;
}

export interface ArtefactTypeLookup {
    key: ArtefactTypeKey;
    artefact_type: ArtefactType | null;
}

export interface ArtefactTypeEvent {
    event_id: string;
    key: ArtefactTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ArtefactTypeVersionKey {
    artefact_type: ArtefactTypeKey;
    version: number;
}

export interface ArtefactTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListArtefactTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListArtefactTypesResponse {
    result: Result;
    types: ArtefactType[];
    total: number;
}

export interface GetArtefactTypeRequest {
    key: ArtefactTypeKey;
}

export interface GetArtefactTypeResponse {
    result: Result;
    artefact_type: ArtefactType | null;
}

export interface GetManyArtefactTypesRequest {
    keys: ArtefactTypeKey[];
}

export interface GetManyArtefactTypesResponse {
    result: Result;
    entries: ArtefactTypeLookup[];
}

export interface PutArtefactTypeRequest {
    change: ArtefactTypeChange;
    intent: ChangeIntent;
}

export interface PutArtefactTypeResponse {
    result: Result;
    artefact_type: ArtefactType;
}

export interface PutManyArtefactTypesRequest {
    changes: ArtefactTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyArtefactTypesResponse {
    result: Result;
    types: ArtefactType[];
}

export interface DeleteArtefactTypeRequest {
    removal: ArtefactTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteArtefactTypeResponse {
    result: Result;
}

export interface DeleteManyArtefactTypesRequest {
    removals: ArtefactTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyArtefactTypesResponse {
    result: Result;
}

export interface ListArtefactTypeVersionsRequest {
    key: ArtefactTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ArtefactTypeVersionsFilter | null;
}

export interface ListArtefactTypeVersionsResponse {
    result: Result;
    versions: ArtefactType[];
    total: number;
}

export interface GetArtefactTypeVersionRequest {
    key: ArtefactTypeVersionKey;
}

export interface GetArtefactTypeVersionResponse {
    result: Result;
    version: ArtefactType;
}

export const subjects = {
    list_artefact_types_request: "dq.v1.artefact_types.list",
    get_artefact_type_request: "dq.v1.artefact_types.get",
    get_many_artefact_types_request: "dq.v1.artefact_types.get_many",
    put_artefact_type_request: "dq.v1.artefact_types.put",
    put_many_artefact_types_request: "dq.v1.artefact_types.put_many",
    delete_artefact_type_request: "dq.v1.artefact_types.delete",
    delete_many_artefact_types_request: "dq.v1.artefact_types.delete_many",
    list_artefact_type_versions_request: "dq.v1.artefact_types_versions.list",
    get_artefact_type_version_request: "dq.v1.artefact_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_artefact_types_request: true,
    get_artefact_type_request: true,
    get_many_artefact_types_request: true,
    put_artefact_type_request: true,
    put_many_artefact_types_request: true,
    delete_artefact_type_request: true,
    delete_many_artefact_types_request: true,
    list_artefact_type_versions_request: true,
    get_artefact_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.artefact_types_events.created",
    updated: "dq.v1.artefact_types_events.updated",
    deleted: "dq.v1.artefact_types_events.deleted",
} as const;
