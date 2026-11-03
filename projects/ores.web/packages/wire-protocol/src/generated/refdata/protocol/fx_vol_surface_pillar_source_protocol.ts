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
import type { FxVolSurfacePillarSource } from '../domain/fx_vol_surface_pillar_source.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolSurfacePillarSourceKey {
    id: string;
}

export interface FxVolSurfacePillarSourceWrite {
    id: string;
    party_id: string;
    pillar_id: string;
    market_series_id: string;
    contributor_code: string;
    weight: number;
}

export interface FxVolSurfacePillarSourceChange {
    write: FxVolSurfacePillarSourceWrite;
    precondition: Precondition;
}

export interface FxVolSurfacePillarSourceRemoval {
    key: FxVolSurfacePillarSourceKey;
    precondition: Precondition;
}

export interface FxVolSurfacePillarSourceLookup {
    key: FxVolSurfacePillarSourceKey;
    fx_vol_surface_pillar_source: FxVolSurfacePillarSource | null;
}

export interface FxVolSurfacePillarSourcesFilter {
    pillar_id: string | null;
    id_one_of: string[] | null;
    pillar_id_one_of: string[] | null;
}

export interface FxVolSurfacePillarSourceEvent {
    event_id: string;
    key: FxVolSurfacePillarSourceKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolSurfacePillarSourceVersionKey {
    fx_vol_surface_pillar_source: FxVolSurfacePillarSourceKey;
    version: number;
}

export interface FxVolSurfacePillarSourceVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolSurfacePillarSourcesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfacePillarSourcesFilter | null;
    as_of: string | null;
}

export interface ListFxVolSurfacePillarSourcesResponse {
    result: Result;
    pillar_sources: FxVolSurfacePillarSource[];
    total: number;
}

export interface GetFxVolSurfacePillarSourceRequest {
    key: FxVolSurfacePillarSourceKey;
}

export interface GetFxVolSurfacePillarSourceResponse {
    result: Result;
    fx_vol_surface_pillar_source: FxVolSurfacePillarSource | null;
}

export interface GetManyFxVolSurfacePillarSourcesRequest {
    keys: FxVolSurfacePillarSourceKey[];
}

export interface GetManyFxVolSurfacePillarSourcesResponse {
    result: Result;
    entries: FxVolSurfacePillarSourceLookup[];
}

export interface PutFxVolSurfacePillarSourceRequest {
    change: FxVolSurfacePillarSourceChange;
    intent: ChangeIntent;
}

export interface PutFxVolSurfacePillarSourceResponse {
    result: Result;
    fx_vol_surface_pillar_source: FxVolSurfacePillarSource | null;
}

export interface PutManyFxVolSurfacePillarSourcesRequest {
    changes: FxVolSurfacePillarSourceChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolSurfacePillarSourcesResponse {
    result: Result;
    pillar_sources: FxVolSurfacePillarSource[];
}

export interface DeleteFxVolSurfacePillarSourceRequest {
    removal: FxVolSurfacePillarSourceRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolSurfacePillarSourceResponse {
    result: Result;
}

export interface DeleteManyFxVolSurfacePillarSourcesRequest {
    removals: FxVolSurfacePillarSourceRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolSurfacePillarSourcesResponse {
    result: Result;
}

export interface ListFxVolSurfacePillarSourceVersionsRequest {
    key: FxVolSurfacePillarSourceKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfacePillarSourceVersionsFilter | null;
}

export interface ListFxVolSurfacePillarSourceVersionsResponse {
    result: Result;
    versions: FxVolSurfacePillarSource[];
    total: number;
}

export interface GetFxVolSurfacePillarSourceVersionRequest {
    key: FxVolSurfacePillarSourceVersionKey;
}

export interface GetFxVolSurfacePillarSourceVersionResponse {
    result: Result;
    version: FxVolSurfacePillarSource | null;
}

export const subjects = {
    list_fx_vol_surface_pillar_sources_request: 'refdata.v1.fx_vol_surface_pillar_sources.list',
    get_fx_vol_surface_pillar_source_request: 'refdata.v1.fx_vol_surface_pillar_sources.get',
    get_many_fx_vol_surface_pillar_sources_request:
        'refdata.v1.fx_vol_surface_pillar_sources.get_many',
    put_fx_vol_surface_pillar_source_request: 'refdata.v1.fx_vol_surface_pillar_sources.put',
    put_many_fx_vol_surface_pillar_sources_request:
        'refdata.v1.fx_vol_surface_pillar_sources.put_many',
    delete_fx_vol_surface_pillar_source_request: 'refdata.v1.fx_vol_surface_pillar_sources.delete',
    delete_many_fx_vol_surface_pillar_sources_request:
        'refdata.v1.fx_vol_surface_pillar_sources.delete_many',
    list_fx_vol_surface_pillar_source_versions_request:
        'refdata.v1.fx_vol_surface_pillar_sources_versions.list',
    get_fx_vol_surface_pillar_source_version_request:
        'refdata.v1.fx_vol_surface_pillar_sources_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_surface_pillar_sources_request: true,
    get_fx_vol_surface_pillar_source_request: true,
    get_many_fx_vol_surface_pillar_sources_request: true,
    put_fx_vol_surface_pillar_source_request: true,
    put_many_fx_vol_surface_pillar_sources_request: true,
    delete_fx_vol_surface_pillar_source_request: true,
    delete_many_fx_vol_surface_pillar_sources_request: true,
    list_fx_vol_surface_pillar_source_versions_request: true,
    get_fx_vol_surface_pillar_source_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.fx_vol_surface_pillar_sources.created',
    updated: 'refdata.v1.events.fx_vol_surface_pillar_sources.updated',
    deleted: 'refdata.v1.events.fx_vol_surface_pillar_sources.deleted',
} as const;
