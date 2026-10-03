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
import type { CurveConfigurationSection } from '../domain/curve_configuration_section.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveConfigurationSectionKey {
    id: string;
}

export interface CurveConfigurationSectionWrite {
    id: string;
    curve_configuration_id: string;
    section_code: string;
}

export interface CurveConfigurationSectionChange {
    write: CurveConfigurationSectionWrite;
    precondition: Precondition;
}

export interface CurveConfigurationSectionRemoval {
    key: CurveConfigurationSectionKey;
    precondition: Precondition;
}

export interface CurveConfigurationSectionLookup {
    key: CurveConfigurationSectionKey;
    curve_configuration_section: CurveConfigurationSection | null;
}

export interface CurveConfigurationSectionEvent {
    event_id: string;
    key: CurveConfigurationSectionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveConfigurationSectionVersionKey {
    curve_configuration_section: CurveConfigurationSectionKey;
    version: number;
}

export interface CurveConfigurationSectionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveConfigurationSectionsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCurveConfigurationSectionsResponse {
    result: Result;
    sections: CurveConfigurationSection[];
    total: number;
}

export interface GetCurveConfigurationSectionRequest {
    key: CurveConfigurationSectionKey;
}

export interface GetCurveConfigurationSectionResponse {
    result: Result;
    curve_configuration_section: CurveConfigurationSection | null;
}

export interface GetManyCurveConfigurationSectionsRequest {
    keys: CurveConfigurationSectionKey[];
}

export interface GetManyCurveConfigurationSectionsResponse {
    result: Result;
    entries: CurveConfigurationSectionLookup[];
}

export interface PutCurveConfigurationSectionRequest {
    change: CurveConfigurationSectionChange;
    intent: ChangeIntent;
}

export interface PutCurveConfigurationSectionResponse {
    result: Result;
    curve_configuration_section: CurveConfigurationSection | null;
}

export interface PutManyCurveConfigurationSectionsRequest {
    changes: CurveConfigurationSectionChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveConfigurationSectionsResponse {
    result: Result;
    sections: CurveConfigurationSection[];
}

export interface DeleteCurveConfigurationSectionRequest {
    removal: CurveConfigurationSectionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveConfigurationSectionResponse {
    result: Result;
}

export interface DeleteManyCurveConfigurationSectionsRequest {
    removals: CurveConfigurationSectionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveConfigurationSectionsResponse {
    result: Result;
}

export interface ListCurveConfigurationSectionVersionsRequest {
    key: CurveConfigurationSectionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveConfigurationSectionVersionsFilter | null;
}

export interface ListCurveConfigurationSectionVersionsResponse {
    result: Result;
    versions: CurveConfigurationSection[];
    total: number;
}

export interface GetCurveConfigurationSectionVersionRequest {
    key: CurveConfigurationSectionVersionKey;
}

export interface GetCurveConfigurationSectionVersionResponse {
    result: Result;
    version: CurveConfigurationSection | null;
}

export const subjects = {
    list_curve_configuration_sections_request: 'refdata.v1.curve_configuration_sections.list',
    get_curve_configuration_section_request: 'refdata.v1.curve_configuration_sections.get',
    get_many_curve_configuration_sections_request:
        'refdata.v1.curve_configuration_sections.get_many',
    put_curve_configuration_section_request: 'refdata.v1.curve_configuration_sections.put',
    put_many_curve_configuration_sections_request:
        'refdata.v1.curve_configuration_sections.put_many',
    delete_curve_configuration_section_request: 'refdata.v1.curve_configuration_sections.delete',
    delete_many_curve_configuration_sections_request:
        'refdata.v1.curve_configuration_sections.delete_many',
    list_curve_configuration_section_versions_request:
        'refdata.v1.curve_configuration_sections_versions.list',
    get_curve_configuration_section_version_request:
        'refdata.v1.curve_configuration_sections_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_configuration_sections_request: true,
    get_curve_configuration_section_request: true,
    get_many_curve_configuration_sections_request: true,
    put_curve_configuration_section_request: true,
    put_many_curve_configuration_sections_request: true,
    delete_curve_configuration_section_request: true,
    delete_many_curve_configuration_sections_request: true,
    list_curve_configuration_section_versions_request: true,
    get_curve_configuration_section_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_configuration_sections_events.created',
    updated: 'refdata.v1.curve_configuration_sections_events.updated',
    deleted: 'refdata.v1.curve_configuration_sections_events.deleted',
} as const;
