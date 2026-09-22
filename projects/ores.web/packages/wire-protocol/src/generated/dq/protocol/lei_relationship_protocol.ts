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
import type { LeiRelationship } from '../domain/lei_relationship.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LeiRelationshipKey {
    relationship_start_node_node_id: string;
}

export interface LeiRelationshipWrite {
    relationship_start_node_node_id: string;
    relationship_start_node_node_id_type: string;
    relationship_end_node_node_id: string;
    relationship_end_node_node_id_type: string;
    relationship_relationship_type: string;
    relationship_relationship_status: string;
    relationship_period_1_start_date: string | null;
    relationship_period_1_end_date: string | null;
    registration_initial_registration_date: string | null;
    registration_last_update_date: string | null;
    registration_registration_status: string | null;
    registration_validation_sources: string | null;
}

export interface LeiRelationshipChange {
    write: LeiRelationshipWrite;
    precondition: Precondition;
}

export interface LeiRelationshipRemoval {
    key: LeiRelationshipKey;
    precondition: Precondition;
}

export interface LeiRelationshipLookup {
    key: LeiRelationshipKey;
    lei_relationship: LeiRelationship | null;
}

export interface LeiRelationshipEvent {
    event_id: string;
    key: LeiRelationshipKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface LeiRelationshipVersionKey {
    lei_relationship: LeiRelationshipKey;
    version: number;
}

export interface LeiRelationshipVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListLeiRelationshipsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListLeiRelationshipsResponse {
    result: Result;
    relationships: LeiRelationship[];
    total: number;
}

export interface GetLeiRelationshipRequest {
    key: LeiRelationshipKey;
}

export interface GetLeiRelationshipResponse {
    result: Result;
    lei_relationship: LeiRelationship | null;
}

export interface GetManyLeiRelationshipsRequest {
    keys: LeiRelationshipKey[];
}

export interface GetManyLeiRelationshipsResponse {
    result: Result;
    entries: LeiRelationshipLookup[];
}

export interface PutLeiRelationshipRequest {
    change: LeiRelationshipChange;
    intent: ChangeIntent;
}

export interface PutLeiRelationshipResponse {
    result: Result;
    lei_relationship: LeiRelationship;
}

export interface PutManyLeiRelationshipsRequest {
    changes: LeiRelationshipChange[];
    intent: ChangeIntent;
}

export interface PutManyLeiRelationshipsResponse {
    result: Result;
    relationships: LeiRelationship[];
}

export interface DeleteLeiRelationshipRequest {
    removal: LeiRelationshipRemoval;
    intent: ChangeIntent;
}

export interface DeleteLeiRelationshipResponse {
    result: Result;
}

export interface DeleteManyLeiRelationshipsRequest {
    removals: LeiRelationshipRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLeiRelationshipsResponse {
    result: Result;
}

export interface ListLeiRelationshipVersionsRequest {
    key: LeiRelationshipKey;
    offset: number;
    limit: number;
    order: Order;
    filter: LeiRelationshipVersionsFilter | null;
}

export interface ListLeiRelationshipVersionsResponse {
    result: Result;
    versions: LeiRelationship[];
    total: number;
}

export interface GetLeiRelationshipVersionRequest {
    key: LeiRelationshipVersionKey;
}

export interface GetLeiRelationshipVersionResponse {
    result: Result;
    version: LeiRelationship;
}

export const subjects = {
    list_lei_relationships_request: "dq.v1.lei_relationships.list",
    get_lei_relationship_request: "dq.v1.lei_relationships.get",
    get_many_lei_relationships_request: "dq.v1.lei_relationships.get_many",
    put_lei_relationship_request: "dq.v1.lei_relationships.put",
    put_many_lei_relationships_request: "dq.v1.lei_relationships.put_many",
    delete_lei_relationship_request: "dq.v1.lei_relationships.delete",
    delete_many_lei_relationships_request: "dq.v1.lei_relationships.delete_many",
    list_lei_relationship_versions_request: "dq.v1.lei_relationships_versions.list",
    get_lei_relationship_version_request: "dq.v1.lei_relationships_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_lei_relationships_request: true,
    get_lei_relationship_request: true,
    get_many_lei_relationships_request: true,
    put_lei_relationship_request: true,
    put_many_lei_relationships_request: true,
    delete_lei_relationship_request: true,
    delete_many_lei_relationships_request: true,
    list_lei_relationship_versions_request: true,
    get_lei_relationship_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.lei_relationships_events.created",
    updated: "dq.v1.lei_relationships_events.updated",
    deleted: "dq.v1.lei_relationships_events.deleted",
} as const;
