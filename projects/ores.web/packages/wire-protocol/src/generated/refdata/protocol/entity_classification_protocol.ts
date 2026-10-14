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
import type { EntityClassification } from '../domain/entity_classification.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface EntityClassificationKey {
    code: string;
    coding_scheme_code: string;
}

export interface EntityClassificationWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface EntityClassificationChange {
    write: EntityClassificationWrite;
    precondition: Precondition;
}

export interface EntityClassificationRemoval {
    key: EntityClassificationKey;
    precondition: Precondition;
}

export interface EntityClassificationLookup {
    key: EntityClassificationKey;
    entity_classification: EntityClassification | null;
}

export interface EntityClassificationEvent {
    event_id: string;
    key: EntityClassificationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface EntityClassificationVersionKey {
    entity_classification: EntityClassificationKey;
    version: number;
}

export interface EntityClassificationVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListEntityClassificationsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListEntityClassificationsResponse {
    result: Result;
    classifications: EntityClassification[];
    total: number;
}

export interface GetEntityClassificationRequest {
    key: EntityClassificationKey;
}

export interface GetEntityClassificationResponse {
    result: Result;
    entity_classification: EntityClassification | null;
}

export interface GetManyEntityClassificationsRequest {
    keys: EntityClassificationKey[];
}

export interface GetManyEntityClassificationsResponse {
    result: Result;
    entries: EntityClassificationLookup[];
}

export interface PutEntityClassificationRequest {
    change: EntityClassificationChange;
    intent: ChangeIntent;
}

export interface PutEntityClassificationResponse {
    result: Result;
    entity_classification: EntityClassification | null;
}

export interface PutManyEntityClassificationsRequest {
    changes: EntityClassificationChange[];
    intent: ChangeIntent;
}

export interface PutManyEntityClassificationsResponse {
    result: Result;
    classifications: EntityClassification[];
}

export interface DeleteEntityClassificationRequest {
    removal: EntityClassificationRemoval;
    intent: ChangeIntent;
}

export interface DeleteEntityClassificationResponse {
    result: Result;
}

export interface DeleteManyEntityClassificationsRequest {
    removals: EntityClassificationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyEntityClassificationsResponse {
    result: Result;
}

export interface ListEntityClassificationVersionsRequest {
    key: EntityClassificationKey;
    offset: number;
    limit: number;
    order: Order;
    filter: EntityClassificationVersionsFilter | null;
}

export interface ListEntityClassificationVersionsResponse {
    result: Result;
    versions: EntityClassification[];
    total: number;
}

export interface GetEntityClassificationVersionRequest {
    key: EntityClassificationVersionKey;
}

export interface GetEntityClassificationVersionResponse {
    result: Result;
    version: EntityClassification | null;
}

export const subjects = {
    list_entity_classifications_request: 'refdata.v1.entity_classifications.list',
    get_entity_classification_request: 'refdata.v1.entity_classifications.get',
    get_many_entity_classifications_request: 'refdata.v1.entity_classifications.get_many',
    put_entity_classification_request: 'refdata.v1.entity_classifications.put',
    put_many_entity_classifications_request: 'refdata.v1.entity_classifications.put_many',
    delete_entity_classification_request: 'refdata.v1.entity_classifications.delete',
    delete_many_entity_classifications_request: 'refdata.v1.entity_classifications.delete_many',
    list_entity_classification_versions_request: 'refdata.v1.entity_classifications_versions.list',
    get_entity_classification_version_request: 'refdata.v1.entity_classifications_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_entity_classifications_request: true,
    get_entity_classification_request: true,
    get_many_entity_classifications_request: true,
    put_entity_classification_request: true,
    put_many_entity_classifications_request: true,
    delete_entity_classification_request: true,
    delete_many_entity_classifications_request: true,
    list_entity_classification_versions_request: true,
    get_entity_classification_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.entity_classifications_events.created',
    updated: 'refdata.v1.entity_classifications_events.updated',
    deleted: 'refdata.v1.entity_classifications_events.deleted',
} as const;
