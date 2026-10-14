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
import type { PartyRelationship } from '../domain/party_relationship.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PartyRelationshipKey {
    code: string;
    coding_scheme_code: string;
}

export interface PartyRelationshipWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface PartyRelationshipChange {
    write: PartyRelationshipWrite;
    precondition: Precondition;
}

export interface PartyRelationshipRemoval {
    key: PartyRelationshipKey;
    precondition: Precondition;
}

export interface PartyRelationshipLookup {
    key: PartyRelationshipKey;
    party_relationship: PartyRelationship | null;
}

export interface PartyRelationshipEvent {
    event_id: string;
    key: PartyRelationshipKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface PartyRelationshipVersionKey {
    party_relationship: PartyRelationshipKey;
    version: number;
}

export interface PartyRelationshipVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListPartyRelationshipsRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListPartyRelationshipsResponse {
    result: Result;
    relationships: PartyRelationship[];
    total: number;
}

export interface GetPartyRelationshipRequest {
    key: PartyRelationshipKey;
}

export interface GetPartyRelationshipResponse {
    result: Result;
    party_relationship: PartyRelationship | null;
}

export interface GetManyPartyRelationshipsRequest {
    keys: PartyRelationshipKey[];
}

export interface GetManyPartyRelationshipsResponse {
    result: Result;
    entries: PartyRelationshipLookup[];
}

export interface PutPartyRelationshipRequest {
    change: PartyRelationshipChange;
    intent: ChangeIntent;
}

export interface PutPartyRelationshipResponse {
    result: Result;
    party_relationship: PartyRelationship | null;
}

export interface PutManyPartyRelationshipsRequest {
    changes: PartyRelationshipChange[];
    intent: ChangeIntent;
}

export interface PutManyPartyRelationshipsResponse {
    result: Result;
    relationships: PartyRelationship[];
}

export interface DeletePartyRelationshipRequest {
    removal: PartyRelationshipRemoval;
    intent: ChangeIntent;
}

export interface DeletePartyRelationshipResponse {
    result: Result;
}

export interface DeleteManyPartyRelationshipsRequest {
    removals: PartyRelationshipRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPartyRelationshipsResponse {
    result: Result;
}

export interface ListPartyRelationshipVersionsRequest {
    key: PartyRelationshipKey;
    offset: number;
    limit: number;
    order: Order;
    filter: PartyRelationshipVersionsFilter | null;
}

export interface ListPartyRelationshipVersionsResponse {
    result: Result;
    versions: PartyRelationship[];
    total: number;
}

export interface GetPartyRelationshipVersionRequest {
    key: PartyRelationshipVersionKey;
}

export interface GetPartyRelationshipVersionResponse {
    result: Result;
    version: PartyRelationship | null;
}

export const subjects = {
    list_party_relationships_request: 'refdata.v1.party_relationships.list',
    get_party_relationship_request: 'refdata.v1.party_relationships.get',
    get_many_party_relationships_request: 'refdata.v1.party_relationships.get_many',
    put_party_relationship_request: 'refdata.v1.party_relationships.put',
    put_many_party_relationships_request: 'refdata.v1.party_relationships.put_many',
    delete_party_relationship_request: 'refdata.v1.party_relationships.delete',
    delete_many_party_relationships_request: 'refdata.v1.party_relationships.delete_many',
    list_party_relationship_versions_request: 'refdata.v1.party_relationships_versions.list',
    get_party_relationship_version_request: 'refdata.v1.party_relationships_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_party_relationships_request: true,
    get_party_relationship_request: true,
    get_many_party_relationships_request: true,
    put_party_relationship_request: true,
    put_many_party_relationships_request: true,
    delete_party_relationship_request: true,
    delete_many_party_relationships_request: true,
    list_party_relationship_versions_request: true,
    get_party_relationship_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.party_relationships_events.created',
    updated: 'refdata.v1.party_relationships_events.updated',
    deleted: 'refdata.v1.party_relationships_events.deleted',
} as const;
