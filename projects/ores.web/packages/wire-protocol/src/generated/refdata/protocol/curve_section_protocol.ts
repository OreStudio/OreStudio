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
import type { CurveSection } from '../domain/curve_section.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveSectionKey {
    code: string;
}

export interface CurveSectionWrite {
    code: string;
    entry_element: string;
    description: string;
}

export interface CurveSectionChange {
    write: CurveSectionWrite;
    precondition: Precondition;
}

export interface CurveSectionRemoval {
    key: CurveSectionKey;
    precondition: Precondition;
}

export interface CurveSectionLookup {
    key: CurveSectionKey;
    curve_section: CurveSection | null;
}

export interface CurveSectionEvent {
    event_id: string;
    key: CurveSectionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveSectionVersionKey {
    curve_section: CurveSectionKey;
    version: number;
}

export interface CurveSectionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveSectionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveSectionsResponse {
    result: Result;
    sections: CurveSection[];
    total: number;
}

export interface GetCurveSectionRequest {
    key: CurveSectionKey;
}

export interface GetCurveSectionResponse {
    result: Result;
    curve_section: CurveSection | null;
}

export interface GetManyCurveSectionsRequest {
    keys: CurveSectionKey[];
}

export interface GetManyCurveSectionsResponse {
    result: Result;
    entries: CurveSectionLookup[];
}

export interface PutCurveSectionRequest {
    change: CurveSectionChange;
    intent: ChangeIntent;
}

export interface PutCurveSectionResponse {
    result: Result;
    curve_section: CurveSection | null;
}

export interface PutManyCurveSectionsRequest {
    changes: CurveSectionChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveSectionsResponse {
    result: Result;
    sections: CurveSection[];
}

export interface DeleteCurveSectionRequest {
    removal: CurveSectionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveSectionResponse {
    result: Result;
}

export interface DeleteManyCurveSectionsRequest {
    removals: CurveSectionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveSectionsResponse {
    result: Result;
}

export interface ListCurveSectionVersionsRequest {
    key: CurveSectionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveSectionVersionsFilter | null;
}

export interface ListCurveSectionVersionsResponse {
    result: Result;
    versions: CurveSection[];
    total: number;
}

export interface GetCurveSectionVersionRequest {
    key: CurveSectionVersionKey;
}

export interface GetCurveSectionVersionResponse {
    result: Result;
    version: CurveSection | null;
}

export const subjects = {
    list_curve_sections_request: 'refdata.v1.curve_sections.list',
    get_curve_section_request: 'refdata.v1.curve_sections.get',
    get_many_curve_sections_request: 'refdata.v1.curve_sections.get_many',
    put_curve_section_request: 'refdata.v1.curve_sections.put',
    put_many_curve_sections_request: 'refdata.v1.curve_sections.put_many',
    delete_curve_section_request: 'refdata.v1.curve_sections.delete',
    delete_many_curve_sections_request: 'refdata.v1.curve_sections.delete_many',
    list_curve_section_versions_request: 'refdata.v1.curve_sections_versions.list',
    get_curve_section_version_request: 'refdata.v1.curve_sections_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_sections_request: true,
    get_curve_section_request: true,
    get_many_curve_sections_request: true,
    put_curve_section_request: true,
    put_many_curve_sections_request: true,
    delete_curve_section_request: true,
    delete_many_curve_sections_request: true,
    list_curve_section_versions_request: true,
    get_curve_section_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_sections_events.created',
    updated: 'refdata.v1.curve_sections_events.updated',
    deleted: 'refdata.v1.curve_sections_events.deleted',
} as const;
