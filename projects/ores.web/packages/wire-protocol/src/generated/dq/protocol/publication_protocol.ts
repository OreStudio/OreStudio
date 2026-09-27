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
import type { Publication } from '../domain/publication.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface PublicationKey {
    id: string;
}

export interface PublicationWrite {
    id: string;
    dataset_id: string;
    dataset_code: string;
    mode: string;
    target_table: string;
    records_inserted: number;
    records_updated: number;
    records_skipped: number;
    records_deleted: number;
    published_by: string;
    published_at: string;
}

export interface PublicationChange {
    write: PublicationWrite;
    precondition: Precondition;
}

export interface PublicationRemoval {
    key: PublicationKey;
    precondition: Precondition;
}

export interface PublicationLookup {
    key: PublicationKey;
    publication: Publication | null;
}

export interface PublicationEvent {
    event_id: string;
    key: PublicationKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListPublicationsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListPublicationsResponse {
    result: Result;
    publications: Publication[];
    total: number;
}

export interface GetPublicationRequest {
    key: PublicationKey;
}

export interface GetPublicationResponse {
    result: Result;
    publication: Publication | null;
}

export interface GetManyPublicationsRequest {
    keys: PublicationKey[];
}

export interface GetManyPublicationsResponse {
    result: Result;
    entries: PublicationLookup[];
}

export interface PutPublicationRequest {
    change: PublicationChange;
    intent: ChangeIntent;
}

export interface PutPublicationResponse {
    result: Result;
    publication: Publication;
}

export interface PutManyPublicationsRequest {
    changes: PublicationChange[];
    intent: ChangeIntent;
}

export interface PutManyPublicationsResponse {
    result: Result;
    publications: Publication[];
}

export interface DeletePublicationRequest {
    removal: PublicationRemoval;
    intent: ChangeIntent;
}

export interface DeletePublicationResponse {
    result: Result;
}

export interface DeleteManyPublicationsRequest {
    removals: PublicationRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyPublicationsResponse {
    result: Result;
}

export const subjects = {
    list_publications_request: "dq.v1.publications.list",
    get_publication_request: "dq.v1.publications.get",
    get_many_publications_request: "dq.v1.publications.get_many",
    put_publication_request: "dq.v1.publications.put",
    put_many_publications_request: "dq.v1.publications.put_many",
    delete_publication_request: "dq.v1.publications.delete",
    delete_many_publications_request: "dq.v1.publications.delete_many",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_publications_request: true,
    get_publication_request: true,
    get_many_publications_request: true,
    put_publication_request: true,
    put_many_publications_request: true,
    delete_publication_request: true,
    delete_many_publications_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.publications_events.created",
    updated: "dq.v1.publications_events.updated",
    deleted: "dq.v1.publications_events.deleted",
} as const;
