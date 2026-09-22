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
import type { SubjectArea } from '../domain/subject_area.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SubjectAreaKey {
    name: string;
    domain_name: string;
}

export interface SubjectAreaWrite {
    name: string;
    domain_name: string;
    description: string;
}

export interface SubjectAreaChange {
    write: SubjectAreaWrite;
    precondition: Precondition;
}

export interface SubjectAreaRemoval {
    key: SubjectAreaKey;
    precondition: Precondition;
}

export interface SubjectAreaLookup {
    key: SubjectAreaKey;
    subject_area: SubjectArea | null;
}

export interface SubjectAreaEvent {
    event_id: string;
    key: SubjectAreaKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SubjectAreaVersionKey {
    subject_area: SubjectAreaKey;
    version: number;
}

export interface SubjectAreaVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSubjectAreasRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSubjectAreasResponse {
    result: Result;
    areas: SubjectArea[];
    total: number;
}

export interface GetSubjectAreaRequest {
    key: SubjectAreaKey;
}

export interface GetSubjectAreaResponse {
    result: Result;
    subject_area: SubjectArea | null;
}

export interface GetManySubjectAreasRequest {
    keys: SubjectAreaKey[];
}

export interface GetManySubjectAreasResponse {
    result: Result;
    entries: SubjectAreaLookup[];
}

export interface PutSubjectAreaRequest {
    change: SubjectAreaChange;
    intent: ChangeIntent;
}

export interface PutSubjectAreaResponse {
    result: Result;
    subject_area: SubjectArea;
}

export interface PutManySubjectAreasRequest {
    changes: SubjectAreaChange[];
    intent: ChangeIntent;
}

export interface PutManySubjectAreasResponse {
    result: Result;
    areas: SubjectArea[];
}

export interface DeleteSubjectAreaRequest {
    removal: SubjectAreaRemoval;
    intent: ChangeIntent;
}

export interface DeleteSubjectAreaResponse {
    result: Result;
}

export interface DeleteManySubjectAreasRequest {
    removals: SubjectAreaRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySubjectAreasResponse {
    result: Result;
}

export interface ListSubjectAreaVersionsRequest {
    key: SubjectAreaKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SubjectAreaVersionsFilter | null;
}

export interface ListSubjectAreaVersionsResponse {
    result: Result;
    versions: SubjectArea[];
    total: number;
}

export interface GetSubjectAreaVersionRequest {
    key: SubjectAreaVersionKey;
}

export interface GetSubjectAreaVersionResponse {
    result: Result;
    version: SubjectArea;
}

export const subjects = {
    list_subject_areas_request: "dq.v1.subject_areas.list",
    get_subject_area_request: "dq.v1.subject_areas.get",
    get_many_subject_areas_request: "dq.v1.subject_areas.get_many",
    put_subject_area_request: "dq.v1.subject_areas.put",
    put_many_subject_areas_request: "dq.v1.subject_areas.put_many",
    delete_subject_area_request: "dq.v1.subject_areas.delete",
    delete_many_subject_areas_request: "dq.v1.subject_areas.delete_many",
    list_subject_area_versions_request: "dq.v1.subject_areas_versions.list",
    get_subject_area_version_request: "dq.v1.subject_areas_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_subject_areas_request: true,
    get_subject_area_request: true,
    get_many_subject_areas_request: true,
    put_subject_area_request: true,
    put_many_subject_areas_request: true,
    delete_subject_area_request: true,
    delete_many_subject_areas_request: true,
    list_subject_area_versions_request: true,
    get_subject_area_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.subject_areas_events.created",
    updated: "dq.v1.subject_areas_events.updated",
    deleted: "dq.v1.subject_areas_events.deleted",
} as const;
