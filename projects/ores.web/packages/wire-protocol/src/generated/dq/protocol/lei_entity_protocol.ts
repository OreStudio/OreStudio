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
import type { LeiEntity } from '../domain/lei_entity.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LeiEntityKey {
    lei: string;
}

export interface LeiEntityWrite {
    lei: string;
    entity_legal_name: string;
    entity_entity_category: string;
    entity_entity_sub_category: string | null;
    entity_entity_status: string;
    entity_legal_form_entity_legal_form_code: string | null;
    entity_legal_form_other_legal_form: string | null;
    entity_legal_jurisdiction: string | null;
    entity_legal_address_first_address_line: string | null;
    entity_legal_address_city: string | null;
    entity_legal_address_region: string | null;
    entity_legal_address_country: string;
    entity_legal_address_postal_code: string | null;
    entity_headquarters_address_first_address_line: string | null;
    entity_headquarters_address_city: string | null;
    entity_headquarters_address_region: string | null;
    entity_headquarters_address_country: string | null;
    entity_headquarters_address_postal_code: string | null;
    entity_entity_creation_date: string | null;
    registration_initial_registration_date: string | null;
    registration_last_update_date: string | null;
    registration_next_renewal_date: string | null;
    registration_registration_status: string | null;
    entity_transliterated_name_1: string | null;
    entity_transliterated_name_1_type: string | null;
}

export interface LeiEntityChange {
    write: LeiEntityWrite;
    precondition: Precondition;
}

export interface LeiEntityRemoval {
    key: LeiEntityKey;
    precondition: Precondition;
}

export interface LeiEntityLookup {
    key: LeiEntityKey;
    lei_entity: LeiEntity | null;
}

export interface LeiEntityEvent {
    event_id: string;
    key: LeiEntityKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface LeiEntityVersionKey {
    lei_entity: LeiEntityKey;
    version: number;
}

export interface LeiEntityVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListLeiEntitiesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListLeiEntitiesResponse {
    result: Result;
    entities: LeiEntity[];
    total: number;
}

export interface GetLeiEntityRequest {
    key: LeiEntityKey;
}

export interface GetLeiEntityResponse {
    result: Result;
    lei_entity: LeiEntity | null;
}

export interface GetManyLeiEntitiesRequest {
    keys: LeiEntityKey[];
}

export interface GetManyLeiEntitiesResponse {
    result: Result;
    entries: LeiEntityLookup[];
}

export interface PutLeiEntityRequest {
    change: LeiEntityChange;
    intent: ChangeIntent;
}

export interface PutLeiEntityResponse {
    result: Result;
    lei_entity: LeiEntity;
}

export interface PutManyLeiEntitiesRequest {
    changes: LeiEntityChange[];
    intent: ChangeIntent;
}

export interface PutManyLeiEntitiesResponse {
    result: Result;
    entities: LeiEntity[];
}

export interface DeleteLeiEntityRequest {
    removal: LeiEntityRemoval;
    intent: ChangeIntent;
}

export interface DeleteLeiEntityResponse {
    result: Result;
}

export interface DeleteManyLeiEntitiesRequest {
    removals: LeiEntityRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLeiEntitiesResponse {
    result: Result;
}

export interface ListLeiEntityVersionsRequest {
    key: LeiEntityKey;
    offset: number;
    limit: number;
    order: Order;
    filter: LeiEntityVersionsFilter | null;
}

export interface ListLeiEntityVersionsResponse {
    result: Result;
    versions: LeiEntity[];
    total: number;
}

export interface GetLeiEntityVersionRequest {
    key: LeiEntityVersionKey;
}

export interface GetLeiEntityVersionResponse {
    result: Result;
    version: LeiEntity;
}

export const subjects = {
    list_lei_entities_request: "dq.v1.lei_entities.list",
    get_lei_entity_request: "dq.v1.lei_entities.get",
    get_many_lei_entities_request: "dq.v1.lei_entities.get_many",
    put_lei_entity_request: "dq.v1.lei_entities.put",
    put_many_lei_entities_request: "dq.v1.lei_entities.put_many",
    delete_lei_entity_request: "dq.v1.lei_entities.delete",
    delete_many_lei_entities_request: "dq.v1.lei_entities.delete_many",
    list_lei_entity_versions_request: "dq.v1.lei_entities_versions.list",
    get_lei_entity_version_request: "dq.v1.lei_entities_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_lei_entities_request: true,
    get_lei_entity_request: true,
    get_many_lei_entities_request: true,
    put_lei_entity_request: true,
    put_many_lei_entities_request: true,
    delete_lei_entity_request: true,
    delete_many_lei_entities_request: true,
    list_lei_entity_versions_request: true,
    get_lei_entity_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.lei_entities_events.created",
    updated: "dq.v1.lei_entities_events.updated",
    deleted: "dq.v1.lei_entities_events.deleted",
} as const;
