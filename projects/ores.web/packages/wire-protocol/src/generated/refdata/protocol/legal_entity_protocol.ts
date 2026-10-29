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
import type { LegalEntity } from '../domain/legal_entity.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LegalEntityKey {
    lei: string;
}

export interface LegalEntityWrite {
    lei: string;
    legal_name: string;
    jurisdiction: string;
    entity_status: string;
    entity_category: string;
    display_order: number;
}

export interface LegalEntityChange {
    write: LegalEntityWrite;
    precondition: Precondition;
}

export interface LegalEntityRemoval {
    key: LegalEntityKey;
    precondition: Precondition;
}

export interface LegalEntityLookup {
    key: LegalEntityKey;
    legal_entity: LegalEntity | null;
}

export interface LegalEntitiesFilter {
    lei_one_of: string[] | null;
}

export interface LegalEntityEvent {
    event_id: string;
    key: LegalEntityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface LegalEntityVersionKey {
    legal_entity: LegalEntityKey;
    version: number;
}

export interface LegalEntityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListLegalEntitiesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: LegalEntitiesFilter | null;
    as_of: string | null;
}

export interface ListLegalEntitiesResponse {
    result: Result;
    legal_entities: LegalEntity[];
    total: number;
}

export interface GetLegalEntityRequest {
    key: LegalEntityKey;
}

export interface GetLegalEntityResponse {
    result: Result;
    legal_entity: LegalEntity | null;
}

export interface GetManyLegalEntitiesRequest {
    keys: LegalEntityKey[];
}

export interface GetManyLegalEntitiesResponse {
    result: Result;
    entries: LegalEntityLookup[];
}

export interface PutLegalEntityRequest {
    change: LegalEntityChange;
    intent: ChangeIntent;
}

export interface PutLegalEntityResponse {
    result: Result;
    legal_entity: LegalEntity | null;
}

export interface PutManyLegalEntitiesRequest {
    changes: LegalEntityChange[];
    intent: ChangeIntent;
}

export interface PutManyLegalEntitiesResponse {
    result: Result;
    legal_entities: LegalEntity[];
}

export interface DeleteLegalEntityRequest {
    removal: LegalEntityRemoval;
    intent: ChangeIntent;
}

export interface DeleteLegalEntityResponse {
    result: Result;
}

export interface DeleteManyLegalEntitiesRequest {
    removals: LegalEntityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLegalEntitiesResponse {
    result: Result;
}

export interface ListLegalEntityVersionsRequest {
    key: LegalEntityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: LegalEntityVersionsFilter | null;
}

export interface ListLegalEntityVersionsResponse {
    result: Result;
    versions: LegalEntity[];
    total: number;
}

export interface GetLegalEntityVersionRequest {
    key: LegalEntityVersionKey;
}

export interface GetLegalEntityVersionResponse {
    result: Result;
    version: LegalEntity | null;
}

export const subjects = {
    list_legal_entities_request: 'refdata.v1.legal_entities.list',
    get_legal_entity_request: 'refdata.v1.legal_entities.get',
    get_many_legal_entities_request: 'refdata.v1.legal_entities.get_many',
    put_legal_entity_request: 'refdata.v1.legal_entities.put',
    put_many_legal_entities_request: 'refdata.v1.legal_entities.put_many',
    delete_legal_entity_request: 'refdata.v1.legal_entities.delete',
    delete_many_legal_entities_request: 'refdata.v1.legal_entities.delete_many',
    list_legal_entity_versions_request: 'refdata.v1.legal_entities_versions.list',
    get_legal_entity_version_request: 'refdata.v1.legal_entities_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_legal_entities_request: true,
    get_legal_entity_request: true,
    get_many_legal_entities_request: true,
    put_legal_entity_request: true,
    put_many_legal_entities_request: true,
    delete_legal_entity_request: true,
    delete_many_legal_entities_request: true,
    list_legal_entity_versions_request: true,
    get_legal_entity_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.legal_entities_events.created',
    updated: 'refdata.v1.legal_entities_events.updated',
    deleted: 'refdata.v1.legal_entities_events.deleted',
} as const;
