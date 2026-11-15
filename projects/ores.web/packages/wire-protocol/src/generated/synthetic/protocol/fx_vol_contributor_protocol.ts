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
import type { FxVolContributor } from '../domain/fx_vol_contributor.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolContributorKey {
    id: string;
}

export interface FxVolContributorWrite {
    id: string;
    provider_code: string;
    noise: number;
    wing_widening: number;
    expiry_widening: number;
    quote_step: number;
    missing_probability: number;
    stale_probability: number;
    outlier_probability: number;
    outlier_size: number;
    bias: number;
}

export interface FxVolContributorChange {
    write: FxVolContributorWrite;
    precondition: Precondition;
}

export interface FxVolContributorRemoval {
    key: FxVolContributorKey;
    precondition: Precondition;
}

export interface FxVolContributorLookup {
    key: FxVolContributorKey;
    fx_vol_contributor: FxVolContributor | null;
}

export interface FxVolContributorsFilter {
    id_one_of: string[] | null;
}

export interface FxVolContributorEvent {
    event_id: string;
    key: FxVolContributorKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolContributorVersionKey {
    fx_vol_contributor: FxVolContributorKey;
    version: number;
}

export interface FxVolContributorVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolContributorsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolContributorsFilter | null;
    as_of: string | null;
}

export interface ListFxVolContributorsResponse {
    result: Result;
    fx_vol_contributors: FxVolContributor[];
    total: number;
}

export interface GetFxVolContributorRequest {
    key: FxVolContributorKey;
}

export interface GetFxVolContributorResponse {
    result: Result;
    fx_vol_contributor: FxVolContributor | null;
}

export interface GetManyFxVolContributorsRequest {
    keys: FxVolContributorKey[];
}

export interface GetManyFxVolContributorsResponse {
    result: Result;
    entries: FxVolContributorLookup[];
}

export interface PutFxVolContributorRequest {
    change: FxVolContributorChange;
    intent: ChangeIntent;
}

export interface PutFxVolContributorResponse {
    result: Result;
    fx_vol_contributor: FxVolContributor | null;
}

export interface PutManyFxVolContributorsRequest {
    changes: FxVolContributorChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolContributorsResponse {
    result: Result;
    fx_vol_contributors: FxVolContributor[];
}

export interface DeleteFxVolContributorRequest {
    removal: FxVolContributorRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolContributorResponse {
    result: Result;
}

export interface DeleteManyFxVolContributorsRequest {
    removals: FxVolContributorRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolContributorsResponse {
    result: Result;
}

export interface ListFxVolContributorVersionsRequest {
    key: FxVolContributorKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolContributorVersionsFilter | null;
}

export interface ListFxVolContributorVersionsResponse {
    result: Result;
    versions: FxVolContributor[];
    total: number;
}

export interface GetFxVolContributorVersionRequest {
    key: FxVolContributorVersionKey;
}

export interface GetFxVolContributorVersionResponse {
    result: Result;
    version: FxVolContributor | null;
}

export const subjects = {
    list_fx_vol_contributors_request: 'synthetic.v1.fx_vol_contributors.list',
    get_fx_vol_contributor_request: 'synthetic.v1.fx_vol_contributors.get',
    get_many_fx_vol_contributors_request: 'synthetic.v1.fx_vol_contributors.get_many',
    put_fx_vol_contributor_request: 'synthetic.v1.fx_vol_contributors.put',
    put_many_fx_vol_contributors_request: 'synthetic.v1.fx_vol_contributors.put_many',
    delete_fx_vol_contributor_request: 'synthetic.v1.fx_vol_contributors.delete',
    delete_many_fx_vol_contributors_request: 'synthetic.v1.fx_vol_contributors.delete_many',
    list_fx_vol_contributor_versions_request: 'synthetic.v1.fx_vol_contributors_versions.list',
    get_fx_vol_contributor_version_request: 'synthetic.v1.fx_vol_contributors_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_contributors_request: true,
    get_fx_vol_contributor_request: true,
    get_many_fx_vol_contributors_request: true,
    put_fx_vol_contributor_request: true,
    put_many_fx_vol_contributors_request: true,
    delete_fx_vol_contributor_request: true,
    delete_many_fx_vol_contributors_request: true,
    list_fx_vol_contributor_versions_request: true,
    get_fx_vol_contributor_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.fx_vol_contributors.created',
    updated: 'synthetic.v1.events.fx_vol_contributors.updated',
    deleted: 'synthetic.v1.events.fx_vol_contributors.deleted',
} as const;
