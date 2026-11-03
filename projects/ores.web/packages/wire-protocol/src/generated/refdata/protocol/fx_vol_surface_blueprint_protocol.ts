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
import type { FxVolSurfaceBlueprint } from '../domain/fx_vol_surface_blueprint.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolSurfaceBlueprintKey {
    id: string;
}

export interface FxVolSurfaceBlueprintWrite {
    id: string;
    output_series_id: string;
    name: string;
    base_currency_code: string;
    quote_currency_code: string;
    fx_option_convention_id: string;
    smile_model: string;
    expiry_interpolation: string;
    output_expiries: string;
    output_strikes: string;
}

export interface FxVolSurfaceBlueprintChange {
    write: FxVolSurfaceBlueprintWrite;
    precondition: Precondition;
}

export interface FxVolSurfaceBlueprintRemoval {
    key: FxVolSurfaceBlueprintKey;
    precondition: Precondition;
}

export interface FxVolSurfaceBlueprintLookup {
    key: FxVolSurfaceBlueprintKey;
    fx_vol_surface_blueprint: FxVolSurfaceBlueprint | null;
}

export interface FxVolSurfaceBlueprintsFilter {
    base_currency_code: string | null;
    quote_currency_code: string | null;
    id_one_of: string[] | null;
    base_currency_code_one_of: string[] | null;
    quote_currency_code_one_of: string[] | null;
    search: string | null;
}

export interface FxVolSurfaceBlueprintEvent {
    event_id: string;
    key: FxVolSurfaceBlueprintKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolSurfaceBlueprintVersionKey {
    fx_vol_surface_blueprint: FxVolSurfaceBlueprintKey;
    version: number;
}

export interface FxVolSurfaceBlueprintVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolSurfaceBlueprintsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceBlueprintsFilter | null;
    as_of: string | null;
}

export interface ListFxVolSurfaceBlueprintsResponse {
    result: Result;
    blueprints: FxVolSurfaceBlueprint[];
    total: number;
}

export interface GetFxVolSurfaceBlueprintRequest {
    key: FxVolSurfaceBlueprintKey;
}

export interface GetFxVolSurfaceBlueprintResponse {
    result: Result;
    fx_vol_surface_blueprint: FxVolSurfaceBlueprint | null;
}

export interface GetManyFxVolSurfaceBlueprintsRequest {
    keys: FxVolSurfaceBlueprintKey[];
}

export interface GetManyFxVolSurfaceBlueprintsResponse {
    result: Result;
    entries: FxVolSurfaceBlueprintLookup[];
}

export interface PutFxVolSurfaceBlueprintRequest {
    change: FxVolSurfaceBlueprintChange;
    intent: ChangeIntent;
}

export interface PutFxVolSurfaceBlueprintResponse {
    result: Result;
    fx_vol_surface_blueprint: FxVolSurfaceBlueprint | null;
}

export interface PutManyFxVolSurfaceBlueprintsRequest {
    changes: FxVolSurfaceBlueprintChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolSurfaceBlueprintsResponse {
    result: Result;
    blueprints: FxVolSurfaceBlueprint[];
}

export interface DeleteFxVolSurfaceBlueprintRequest {
    removal: FxVolSurfaceBlueprintRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolSurfaceBlueprintResponse {
    result: Result;
}

export interface DeleteManyFxVolSurfaceBlueprintsRequest {
    removals: FxVolSurfaceBlueprintRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolSurfaceBlueprintsResponse {
    result: Result;
}

export interface ListFxVolSurfaceBlueprintVersionsRequest {
    key: FxVolSurfaceBlueprintKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolSurfaceBlueprintVersionsFilter | null;
}

export interface ListFxVolSurfaceBlueprintVersionsResponse {
    result: Result;
    versions: FxVolSurfaceBlueprint[];
    total: number;
}

export interface GetFxVolSurfaceBlueprintVersionRequest {
    key: FxVolSurfaceBlueprintVersionKey;
}

export interface GetFxVolSurfaceBlueprintVersionResponse {
    result: Result;
    version: FxVolSurfaceBlueprint | null;
}

export const subjects = {
    list_fx_vol_surface_blueprints_request: 'refdata.v1.fx_vol_surface_blueprints.list',
    get_fx_vol_surface_blueprint_request: 'refdata.v1.fx_vol_surface_blueprints.get',
    get_many_fx_vol_surface_blueprints_request: 'refdata.v1.fx_vol_surface_blueprints.get_many',
    put_fx_vol_surface_blueprint_request: 'refdata.v1.fx_vol_surface_blueprints.put',
    put_many_fx_vol_surface_blueprints_request: 'refdata.v1.fx_vol_surface_blueprints.put_many',
    delete_fx_vol_surface_blueprint_request: 'refdata.v1.fx_vol_surface_blueprints.delete',
    delete_many_fx_vol_surface_blueprints_request:
        'refdata.v1.fx_vol_surface_blueprints.delete_many',
    list_fx_vol_surface_blueprint_versions_request:
        'refdata.v1.fx_vol_surface_blueprints_versions.list',
    get_fx_vol_surface_blueprint_version_request:
        'refdata.v1.fx_vol_surface_blueprints_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_vol_surface_blueprints_request: true,
    get_fx_vol_surface_blueprint_request: true,
    get_many_fx_vol_surface_blueprints_request: true,
    put_fx_vol_surface_blueprint_request: true,
    put_many_fx_vol_surface_blueprints_request: true,
    delete_fx_vol_surface_blueprint_request: true,
    delete_many_fx_vol_surface_blueprints_request: true,
    list_fx_vol_surface_blueprint_versions_request: true,
    get_fx_vol_surface_blueprint_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.fx_vol_surface_blueprints_events.created',
    updated: 'refdata.v1.fx_vol_surface_blueprints_events.updated',
    deleted: 'refdata.v1.fx_vol_surface_blueprints_events.deleted',
} as const;
