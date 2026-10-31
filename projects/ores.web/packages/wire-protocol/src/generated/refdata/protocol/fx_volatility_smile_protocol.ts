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
import type { FxVolatilitySmile } from '../domain/fx_volatility_smile.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxVolatilitySmileKey {
    id: string;
}

export interface FxVolatilitySmileWrite {
    id: string;
    fx_volatility_config_id: string;
    smile_type: string | null;
    smile_interpolation: string | null;
    smile_delta: string | null;
    smile_extrapolation: string | null;
    time_interpolation: string | null;
    time_weighting: string | null;
    butterfly_error_tolerance: number | null;
}

export interface FxVolatilitySmileChange {
    write: FxVolatilitySmileWrite;
    precondition: Precondition;
}

export interface FxVolatilitySmileRemoval {
    key: FxVolatilitySmileKey;
    precondition: Precondition;
}

export interface FxVolatilitySmileLookup {
    key: FxVolatilitySmileKey;
    fx_volatility_smile: FxVolatilitySmile | null;
}

export interface FxVolatilitySmilesFilter {
    id_one_of: string[] | null;
}

export interface FxVolatilitySmileEvent {
    event_id: string;
    key: FxVolatilitySmileKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxVolatilitySmileVersionKey {
    fx_volatility_smile: FxVolatilitySmileKey;
    version: number;
}

export interface FxVolatilitySmileVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxVolatilitySmilesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolatilitySmilesFilter | null;
    as_of: string | null;
}

export interface ListFxVolatilitySmilesResponse {
    result: Result;
    fx_volatility_smiles: FxVolatilitySmile[];
    total: number;
}

export interface GetFxVolatilitySmileRequest {
    key: FxVolatilitySmileKey;
}

export interface GetFxVolatilitySmileResponse {
    result: Result;
    fx_volatility_smile: FxVolatilitySmile | null;
}

export interface GetManyFxVolatilitySmilesRequest {
    keys: FxVolatilitySmileKey[];
}

export interface GetManyFxVolatilitySmilesResponse {
    result: Result;
    entries: FxVolatilitySmileLookup[];
}

export interface PutFxVolatilitySmileRequest {
    change: FxVolatilitySmileChange;
    intent: ChangeIntent;
}

export interface PutFxVolatilitySmileResponse {
    result: Result;
    fx_volatility_smile: FxVolatilitySmile | null;
}

export interface PutManyFxVolatilitySmilesRequest {
    changes: FxVolatilitySmileChange[];
    intent: ChangeIntent;
}

export interface PutManyFxVolatilitySmilesResponse {
    result: Result;
    fx_volatility_smiles: FxVolatilitySmile[];
}

export interface DeleteFxVolatilitySmileRequest {
    removal: FxVolatilitySmileRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxVolatilitySmileResponse {
    result: Result;
}

export interface DeleteManyFxVolatilitySmilesRequest {
    removals: FxVolatilitySmileRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxVolatilitySmilesResponse {
    result: Result;
}

export interface ListFxVolatilitySmileVersionsRequest {
    key: FxVolatilitySmileKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxVolatilitySmileVersionsFilter | null;
}

export interface ListFxVolatilitySmileVersionsResponse {
    result: Result;
    versions: FxVolatilitySmile[];
    total: number;
}

export interface GetFxVolatilitySmileVersionRequest {
    key: FxVolatilitySmileVersionKey;
}

export interface GetFxVolatilitySmileVersionResponse {
    result: Result;
    version: FxVolatilitySmile | null;
}

export const subjects = {
    list_fx_volatility_smiles_request: 'refdata.v1.fx_volatility_smiles.list',
    get_fx_volatility_smile_request: 'refdata.v1.fx_volatility_smiles.get',
    get_many_fx_volatility_smiles_request: 'refdata.v1.fx_volatility_smiles.get_many',
    put_fx_volatility_smile_request: 'refdata.v1.fx_volatility_smiles.put',
    put_many_fx_volatility_smiles_request: 'refdata.v1.fx_volatility_smiles.put_many',
    delete_fx_volatility_smile_request: 'refdata.v1.fx_volatility_smiles.delete',
    delete_many_fx_volatility_smiles_request: 'refdata.v1.fx_volatility_smiles.delete_many',
    list_fx_volatility_smile_versions_request: 'refdata.v1.fx_volatility_smiles_versions.list',
    get_fx_volatility_smile_version_request: 'refdata.v1.fx_volatility_smiles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_volatility_smiles_request: true,
    get_fx_volatility_smile_request: true,
    get_many_fx_volatility_smiles_request: true,
    put_fx_volatility_smile_request: true,
    put_many_fx_volatility_smiles_request: true,
    delete_fx_volatility_smile_request: true,
    delete_many_fx_volatility_smiles_request: true,
    list_fx_volatility_smile_versions_request: true,
    get_fx_volatility_smile_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.fx_volatility_smiles_events.created',
    updated: 'refdata.v1.fx_volatility_smiles_events.updated',
    deleted: 'refdata.v1.fx_volatility_smiles_events.deleted',
} as const;
