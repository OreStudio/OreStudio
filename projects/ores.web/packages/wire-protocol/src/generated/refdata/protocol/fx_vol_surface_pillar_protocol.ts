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
import type { FxVolSurfacePillar } from '../domain/fx_vol_surface_pillar.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolSurfacePillarKey {
    id: string;
}

export interface FxVolSurfacePillarWrite {
    id: string;
    party_id: string;
    blueprint_id: string;
    expiry_code: string;
    strike_label: string;
}

export interface FxVolSurfacePillarChange {
    write: FxVolSurfacePillarWrite;
    precondition: Precondition;
}

export interface FxVolSurfacePillarRemoval {
    key: FxVolSurfacePillarKey;
    precondition: Precondition;
}

export interface FxVolSurfacePillarLookup {
    key: FxVolSurfacePillarKey;
    fx_vol_surface_pillar: FxVolSurfacePillar | null;
}

export interface FxVolSurfacePillarsFilter {
    blueprint_id: string | null;
    id_one_of: string[] | null;
    blueprint_id_one_of: string[] | null;
}

export interface FxVolSurfacePillarEvent {
    event_id: string;
    key: FxVolSurfacePillarKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolSurfacePillarVersionKey {
    fx_vol_surface_pillar: FxVolSurfacePillarKey;
    version: number;
}

export interface FxVolSurfacePillarVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolSurfacePillarsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfacePillarsFilter | null;
    as_of: string | null;
}

export interface ListFxVolSurfacePillarsResponse {
    result: Result;
    pillars: FxVolSurfacePillar[];
    total: number;
}

export interface GetFxVolSurfacePillarRequest {
    key: FxVolSurfacePillarKey;
}

export interface GetFxVolSurfacePillarResponse {
    result: Result;
    fx_vol_surface_pillar: FxVolSurfacePillar | null;
}

export interface GetManyFxVolSurfacePillarsRequest {
    keys: FxVolSurfacePillarKey[];
}

export interface GetManyFxVolSurfacePillarsResponse {
    result: Result;
    entries: FxVolSurfacePillarLookup[];
}

export interface PutFxVolSurfacePillarRequest {
    change: FxVolSurfacePillarChange;
    intent: ChangeIntent;
}

export interface PutFxVolSurfacePillarResponse {
    result: Result;
    fx_vol_surface_pillar: FxVolSurfacePillar | null;
}

export interface PutManyFxVolSurfacePillarsRequest {
    changes: FxVolSurfacePillarChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolSurfacePillarsResponse {
    result: Result;
    pillars: FxVolSurfacePillar[];
}

export interface DeleteFxVolSurfacePillarRequest {
    removal: FxVolSurfacePillarRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolSurfacePillarResponse {
    result: Result;
}

export interface DeleteManyFxVolSurfacePillarsRequest {
    removals: FxVolSurfacePillarRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolSurfacePillarsResponse {
    result: Result;
}

export interface ListFxVolSurfacePillarVersionsRequest {
    key: FxVolSurfacePillarKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfacePillarVersionsFilter | null;
}

export interface ListFxVolSurfacePillarVersionsResponse {
    result: Result;
    versions: FxVolSurfacePillar[];
    total: number;
}

export interface GetFxVolSurfacePillarVersionRequest {
    key: FxVolSurfacePillarVersionKey;
}

export interface GetFxVolSurfacePillarVersionResponse {
    result: Result;
    version: FxVolSurfacePillar | null;
}

export const subjects = {
    list_fx_vol_surface_pillars_request: 'refdata.v1.fx_vol_surface_pillars.list',
    get_fx_vol_surface_pillar_request: 'refdata.v1.fx_vol_surface_pillars.get',
    get_many_fx_vol_surface_pillars_request: 'refdata.v1.fx_vol_surface_pillars.get_many',
    put_fx_vol_surface_pillar_request: 'refdata.v1.fx_vol_surface_pillars.put',
    put_many_fx_vol_surface_pillars_request: 'refdata.v1.fx_vol_surface_pillars.put_many',
    delete_fx_vol_surface_pillar_request: 'refdata.v1.fx_vol_surface_pillars.delete',
    delete_many_fx_vol_surface_pillars_request: 'refdata.v1.fx_vol_surface_pillars.delete_many',
    list_fx_vol_surface_pillar_versions_request: 'refdata.v1.fx_vol_surface_pillars_versions.list',
    get_fx_vol_surface_pillar_version_request: 'refdata.v1.fx_vol_surface_pillars_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_surface_pillars_request: true,
    get_fx_vol_surface_pillar_request: true,
    get_many_fx_vol_surface_pillars_request: true,
    put_fx_vol_surface_pillar_request: true,
    put_many_fx_vol_surface_pillars_request: true,
    delete_fx_vol_surface_pillar_request: true,
    delete_many_fx_vol_surface_pillars_request: true,
    list_fx_vol_surface_pillar_versions_request: true,
    get_fx_vol_surface_pillar_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.fx_vol_surface_pillars.created',
    updated: 'refdata.v1.events.fx_vol_surface_pillars.updated',
    deleted: 'refdata.v1.events.fx_vol_surface_pillars.deleted',
} as const;
