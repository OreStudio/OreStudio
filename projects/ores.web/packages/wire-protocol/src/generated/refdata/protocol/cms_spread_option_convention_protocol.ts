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
import type { CmsSpreadOptionConvention } from '../domain/cms_spread_option_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CmsSpreadOptionConventionKey {
    id: string;
}

export interface CmsSpreadOptionConventionWrite {
    id: string;
    forward_start: string;
    spot_days: string;
    swap_tenor: string;
    fixing_days: number;
    calendar: string;
    day_count_fraction: string;
    roll_convention: string;
    oresmd_uri: string | null;
}

export interface CmsSpreadOptionConventionChange {
    write: CmsSpreadOptionConventionWrite;
    precondition: Precondition;
}

export interface CmsSpreadOptionConventionRemoval {
    key: CmsSpreadOptionConventionKey;
    precondition: Precondition;
}

export interface CmsSpreadOptionConventionLookup {
    key: CmsSpreadOptionConventionKey;
    cms_spread_option_convention: CmsSpreadOptionConvention | null;
}

export interface CmsSpreadOptionConventionsFilter {
    id_one_of: string[] | null;
}

export interface CmsSpreadOptionConventionEvent {
    event_id: string;
    key: CmsSpreadOptionConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CmsSpreadOptionConventionVersionKey {
    cms_spread_option_convention: CmsSpreadOptionConventionKey;
    version: number;
}

export interface CmsSpreadOptionConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCmsSpreadOptionConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CmsSpreadOptionConventionsFilter | null;
    as_of: string | null;
}

export interface ListCmsSpreadOptionConventionsResponse {
    result: Result;
    cms_spread_option_conventions: CmsSpreadOptionConvention[];
    total: number;
}

export interface GetCmsSpreadOptionConventionRequest {
    key: CmsSpreadOptionConventionKey;
}

export interface GetCmsSpreadOptionConventionResponse {
    result: Result;
    cms_spread_option_convention: CmsSpreadOptionConvention | null;
}

export interface GetManyCmsSpreadOptionConventionsRequest {
    keys: CmsSpreadOptionConventionKey[];
}

export interface GetManyCmsSpreadOptionConventionsResponse {
    result: Result;
    entries: CmsSpreadOptionConventionLookup[];
}

export interface PutCmsSpreadOptionConventionRequest {
    change: CmsSpreadOptionConventionChange;
    intent: ChangeIntent;
}

export interface PutCmsSpreadOptionConventionResponse {
    result: Result;
    cms_spread_option_convention: CmsSpreadOptionConvention | null;
}

export interface PutManyCmsSpreadOptionConventionsRequest {
    changes: CmsSpreadOptionConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyCmsSpreadOptionConventionsResponse {
    result: Result;
    cms_spread_option_conventions: CmsSpreadOptionConvention[];
}

export interface DeleteCmsSpreadOptionConventionRequest {
    removal: CmsSpreadOptionConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteCmsSpreadOptionConventionResponse {
    result: Result;
}

export interface DeleteManyCmsSpreadOptionConventionsRequest {
    removals: CmsSpreadOptionConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCmsSpreadOptionConventionsResponse {
    result: Result;
}

export interface ListCmsSpreadOptionConventionVersionsRequest {
    key: CmsSpreadOptionConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CmsSpreadOptionConventionVersionsFilter | null;
}

export interface ListCmsSpreadOptionConventionVersionsResponse {
    result: Result;
    versions: CmsSpreadOptionConvention[];
    total: number;
}

export interface GetCmsSpreadOptionConventionVersionRequest {
    key: CmsSpreadOptionConventionVersionKey;
}

export interface GetCmsSpreadOptionConventionVersionResponse {
    result: Result;
    version: CmsSpreadOptionConvention | null;
}

export const subjects = {
    list_cms_spread_option_conventions_request: 'refdata.v1.cms_spread_option_conventions.list',
    get_cms_spread_option_convention_request: 'refdata.v1.cms_spread_option_conventions.get',
    get_many_cms_spread_option_conventions_request:
        'refdata.v1.cms_spread_option_conventions.get_many',
    put_cms_spread_option_convention_request: 'refdata.v1.cms_spread_option_conventions.put',
    put_many_cms_spread_option_conventions_request:
        'refdata.v1.cms_spread_option_conventions.put_many',
    delete_cms_spread_option_convention_request: 'refdata.v1.cms_spread_option_conventions.delete',
    delete_many_cms_spread_option_conventions_request:
        'refdata.v1.cms_spread_option_conventions.delete_many',
    list_cms_spread_option_convention_versions_request:
        'refdata.v1.cms_spread_option_conventions_versions.list',
    get_cms_spread_option_convention_version_request:
        'refdata.v1.cms_spread_option_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cms_spread_option_conventions_request: true,
    get_cms_spread_option_convention_request: true,
    get_many_cms_spread_option_conventions_request: true,
    put_cms_spread_option_convention_request: true,
    put_many_cms_spread_option_conventions_request: true,
    delete_cms_spread_option_convention_request: true,
    delete_many_cms_spread_option_conventions_request: true,
    list_cms_spread_option_convention_versions_request: true,
    get_cms_spread_option_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.cms_spread_option_conventions_events.created',
    updated: 'refdata.v1.cms_spread_option_conventions_events.updated',
    deleted: 'refdata.v1.cms_spread_option_conventions_events.deleted',
} as const;
