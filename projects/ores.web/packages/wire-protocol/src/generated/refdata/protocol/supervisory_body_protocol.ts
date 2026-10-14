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
import type { SupervisoryBody } from '../domain/supervisory_body.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SupervisoryBodyKey {
    code: string;
    coding_scheme_code: string;
}

export interface SupervisoryBodyWrite {
    code: string;
    coding_scheme_code: string;
    source: string;
    description: string;
}

export interface SupervisoryBodyChange {
    write: SupervisoryBodyWrite;
    precondition: Precondition;
}

export interface SupervisoryBodyRemoval {
    key: SupervisoryBodyKey;
    precondition: Precondition;
}

export interface SupervisoryBodyLookup {
    key: SupervisoryBodyKey;
    supervisory_body: SupervisoryBody | null;
}

export interface SupervisoryBodyEvent {
    event_id: string;
    key: SupervisoryBodyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SupervisoryBodyVersionKey {
    supervisory_body: SupervisoryBodyKey;
    version: number;
}

export interface SupervisoryBodyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSupervisoryBodiesRequest {
    offset: number;
    limit: number;
    order: Order;
    as_of: string | null;
}

export interface ListSupervisoryBodiesResponse {
    result: Result;
    bodies: SupervisoryBody[];
    total: number;
}

export interface GetSupervisoryBodyRequest {
    key: SupervisoryBodyKey;
}

export interface GetSupervisoryBodyResponse {
    result: Result;
    supervisory_body: SupervisoryBody | null;
}

export interface GetManySupervisoryBodiesRequest {
    keys: SupervisoryBodyKey[];
}

export interface GetManySupervisoryBodiesResponse {
    result: Result;
    entries: SupervisoryBodyLookup[];
}

export interface PutSupervisoryBodyRequest {
    change: SupervisoryBodyChange;
    intent: ChangeIntent;
}

export interface PutSupervisoryBodyResponse {
    result: Result;
    supervisory_body: SupervisoryBody | null;
}

export interface PutManySupervisoryBodiesRequest {
    changes: SupervisoryBodyChange[];
    intent: ChangeIntent;
}

export interface PutManySupervisoryBodiesResponse {
    result: Result;
    bodies: SupervisoryBody[];
}

export interface DeleteSupervisoryBodyRequest {
    removal: SupervisoryBodyRemoval;
    intent: ChangeIntent;
}

export interface DeleteSupervisoryBodyResponse {
    result: Result;
}

export interface DeleteManySupervisoryBodiesRequest {
    removals: SupervisoryBodyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySupervisoryBodiesResponse {
    result: Result;
}

export interface ListSupervisoryBodyVersionsRequest {
    key: SupervisoryBodyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SupervisoryBodyVersionsFilter | null;
}

export interface ListSupervisoryBodyVersionsResponse {
    result: Result;
    versions: SupervisoryBody[];
    total: number;
}

export interface GetSupervisoryBodyVersionRequest {
    key: SupervisoryBodyVersionKey;
}

export interface GetSupervisoryBodyVersionResponse {
    result: Result;
    version: SupervisoryBody | null;
}

export const subjects = {
    list_supervisory_bodies_request: 'refdata.v1.supervisory_bodies.list',
    get_supervisory_body_request: 'refdata.v1.supervisory_bodies.get',
    get_many_supervisory_bodies_request: 'refdata.v1.supervisory_bodies.get_many',
    put_supervisory_body_request: 'refdata.v1.supervisory_bodies.put',
    put_many_supervisory_bodies_request: 'refdata.v1.supervisory_bodies.put_many',
    delete_supervisory_body_request: 'refdata.v1.supervisory_bodies.delete',
    delete_many_supervisory_bodies_request: 'refdata.v1.supervisory_bodies.delete_many',
    list_supervisory_body_versions_request: 'refdata.v1.supervisory_bodies_versions.list',
    get_supervisory_body_version_request: 'refdata.v1.supervisory_bodies_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_supervisory_bodies_request: true,
    get_supervisory_body_request: true,
    get_many_supervisory_bodies_request: true,
    put_supervisory_body_request: true,
    put_many_supervisory_bodies_request: true,
    delete_supervisory_body_request: true,
    delete_many_supervisory_bodies_request: true,
    list_supervisory_body_versions_request: true,
    get_supervisory_body_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.supervisory_bodies_events.created',
    updated: 'refdata.v1.supervisory_bodies_events.updated',
    deleted: 'refdata.v1.supervisory_bodies_events.deleted',
} as const;
