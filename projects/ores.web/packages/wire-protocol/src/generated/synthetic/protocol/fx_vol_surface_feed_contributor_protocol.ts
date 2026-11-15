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
import type { FxVolSurfaceFeedContributor } from '../domain/fx_vol_surface_feed_contributor.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolSurfaceFeedContributorKey {
    id: string;
}

export interface FxVolSurfaceFeedContributorWrite {
    id: string;
    feed_id: string;
    contributor_id: string;
}

export interface FxVolSurfaceFeedContributorChange {
    write: FxVolSurfaceFeedContributorWrite;
    precondition: Precondition;
}

export interface FxVolSurfaceFeedContributorRemoval {
    key: FxVolSurfaceFeedContributorKey;
    precondition: Precondition;
}

export interface FxVolSurfaceFeedContributorLookup {
    key: FxVolSurfaceFeedContributorKey;
    fx_vol_surface_feed_contributor: FxVolSurfaceFeedContributor | null;
}

export interface FxVolSurfaceFeedContributorsFilter {
    feed_id: string | null;
    contributor_id: string | null;
    id_one_of: string[] | null;
    feed_id_one_of: string[] | null;
    contributor_id_one_of: string[] | null;
}

export interface FxVolSurfaceFeedContributorEvent {
    event_id: string;
    key: FxVolSurfaceFeedContributorKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolSurfaceFeedContributorVersionKey {
    fx_vol_surface_feed_contributor: FxVolSurfaceFeedContributorKey;
    version: number;
}

export interface FxVolSurfaceFeedContributorVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolSurfaceFeedContributorsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceFeedContributorsFilter | null;
    as_of: string | null;
}

export interface ListFxVolSurfaceFeedContributorsResponse {
    result: Result;
    fx_vol_surface_feed_contributors: FxVolSurfaceFeedContributor[];
    total: number;
}

export interface GetFxVolSurfaceFeedContributorRequest {
    key: FxVolSurfaceFeedContributorKey;
}

export interface GetFxVolSurfaceFeedContributorResponse {
    result: Result;
    fx_vol_surface_feed_contributor: FxVolSurfaceFeedContributor | null;
}

export interface GetManyFxVolSurfaceFeedContributorsRequest {
    keys: FxVolSurfaceFeedContributorKey[];
}

export interface GetManyFxVolSurfaceFeedContributorsResponse {
    result: Result;
    entries: FxVolSurfaceFeedContributorLookup[];
}

export interface PutFxVolSurfaceFeedContributorRequest {
    change: FxVolSurfaceFeedContributorChange;
    intent: ChangeIntent;
}

export interface PutFxVolSurfaceFeedContributorResponse {
    result: Result;
    fx_vol_surface_feed_contributor: FxVolSurfaceFeedContributor | null;
}

export interface PutManyFxVolSurfaceFeedContributorsRequest {
    changes: FxVolSurfaceFeedContributorChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolSurfaceFeedContributorsResponse {
    result: Result;
    fx_vol_surface_feed_contributors: FxVolSurfaceFeedContributor[];
}

export interface DeleteFxVolSurfaceFeedContributorRequest {
    removal: FxVolSurfaceFeedContributorRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolSurfaceFeedContributorResponse {
    result: Result;
}

export interface DeleteManyFxVolSurfaceFeedContributorsRequest {
    removals: FxVolSurfaceFeedContributorRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolSurfaceFeedContributorsResponse {
    result: Result;
}

export interface ListFxVolSurfaceFeedContributorVersionsRequest {
    key: FxVolSurfaceFeedContributorKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceFeedContributorVersionsFilter | null;
}

export interface ListFxVolSurfaceFeedContributorVersionsResponse {
    result: Result;
    versions: FxVolSurfaceFeedContributor[];
    total: number;
}

export interface GetFxVolSurfaceFeedContributorVersionRequest {
    key: FxVolSurfaceFeedContributorVersionKey;
}

export interface GetFxVolSurfaceFeedContributorVersionResponse {
    result: Result;
    version: FxVolSurfaceFeedContributor | null;
}

export const subjects = {
    list_fx_vol_surface_feed_contributors_request:
        'synthetic.v1.fx_vol_surface_feed_contributors.list',
    get_fx_vol_surface_feed_contributor_request:
        'synthetic.v1.fx_vol_surface_feed_contributors.get',
    get_many_fx_vol_surface_feed_contributors_request:
        'synthetic.v1.fx_vol_surface_feed_contributors.get_many',
    put_fx_vol_surface_feed_contributor_request:
        'synthetic.v1.fx_vol_surface_feed_contributors.put',
    put_many_fx_vol_surface_feed_contributors_request:
        'synthetic.v1.fx_vol_surface_feed_contributors.put_many',
    delete_fx_vol_surface_feed_contributor_request:
        'synthetic.v1.fx_vol_surface_feed_contributors.delete',
    delete_many_fx_vol_surface_feed_contributors_request:
        'synthetic.v1.fx_vol_surface_feed_contributors.delete_many',
    list_fx_vol_surface_feed_contributor_versions_request:
        'synthetic.v1.fx_vol_surface_feed_contributors_versions.list',
    get_fx_vol_surface_feed_contributor_version_request:
        'synthetic.v1.fx_vol_surface_feed_contributors_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_surface_feed_contributors_request: true,
    get_fx_vol_surface_feed_contributor_request: true,
    get_many_fx_vol_surface_feed_contributors_request: true,
    put_fx_vol_surface_feed_contributor_request: true,
    put_many_fx_vol_surface_feed_contributors_request: true,
    delete_fx_vol_surface_feed_contributor_request: true,
    delete_many_fx_vol_surface_feed_contributors_request: true,
    list_fx_vol_surface_feed_contributor_versions_request: true,
    get_fx_vol_surface_feed_contributor_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.fx_vol_surface_feed_contributors.created',
    updated: 'synthetic.v1.events.fx_vol_surface_feed_contributors.updated',
    deleted: 'synthetic.v1.events.fx_vol_surface_feed_contributors.deleted',
} as const;
