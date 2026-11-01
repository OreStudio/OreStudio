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
import type { CurveVolatilityParametricSmile } from '../domain/curve_volatility_parametric_smile.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilityParametricSmileKey {
    id: string;
}

export interface CurveVolatilityParametricSmileWrite {
    id: string;
    curve_volatility_kind_id: string;
    max_calibration_attempts: number;
    exit_early_error_threshold: number;
    max_acceptable_error: number;
    residual_correction_dimension: string | null;
}

export interface CurveVolatilityParametricSmileChange {
    write: CurveVolatilityParametricSmileWrite;
    precondition: Precondition;
}

export interface CurveVolatilityParametricSmileRemoval {
    key: CurveVolatilityParametricSmileKey;
    precondition: Precondition;
}

export interface CurveVolatilityParametricSmileLookup {
    key: CurveVolatilityParametricSmileKey;
    curve_volatility_parametric_smile: CurveVolatilityParametricSmile | null;
}

export interface CurveVolatilityParametricSmilesFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilityParametricSmileEvent {
    event_id: string;
    key: CurveVolatilityParametricSmileKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilityParametricSmileVersionKey {
    curve_volatility_parametric_smile: CurveVolatilityParametricSmileKey;
    version: number;
}

export interface CurveVolatilityParametricSmileVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilityParametricSmilesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityParametricSmilesFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilityParametricSmilesResponse {
    result: Result;
    curve_volatility_parametric_smiles: CurveVolatilityParametricSmile[];
    total: number;
}

export interface GetCurveVolatilityParametricSmileRequest {
    key: CurveVolatilityParametricSmileKey;
}

export interface GetCurveVolatilityParametricSmileResponse {
    result: Result;
    curve_volatility_parametric_smile: CurveVolatilityParametricSmile | null;
}

export interface GetManyCurveVolatilityParametricSmilesRequest {
    keys: CurveVolatilityParametricSmileKey[];
}

export interface GetManyCurveVolatilityParametricSmilesResponse {
    result: Result;
    entries: CurveVolatilityParametricSmileLookup[];
}

export interface PutCurveVolatilityParametricSmileRequest {
    change: CurveVolatilityParametricSmileChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilityParametricSmileResponse {
    result: Result;
    curve_volatility_parametric_smile: CurveVolatilityParametricSmile | null;
}

export interface PutManyCurveVolatilityParametricSmilesRequest {
    changes: CurveVolatilityParametricSmileChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilityParametricSmilesResponse {
    result: Result;
    curve_volatility_parametric_smiles: CurveVolatilityParametricSmile[];
}

export interface DeleteCurveVolatilityParametricSmileRequest {
    removal: CurveVolatilityParametricSmileRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilityParametricSmileResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilityParametricSmilesRequest {
    removals: CurveVolatilityParametricSmileRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilityParametricSmilesResponse {
    result: Result;
}

export interface ListCurveVolatilityParametricSmileVersionsRequest {
    key: CurveVolatilityParametricSmileKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilityParametricSmileVersionsFilter | null;
}

export interface ListCurveVolatilityParametricSmileVersionsResponse {
    result: Result;
    versions: CurveVolatilityParametricSmile[];
    total: number;
}

export interface GetCurveVolatilityParametricSmileVersionRequest {
    key: CurveVolatilityParametricSmileVersionKey;
}

export interface GetCurveVolatilityParametricSmileVersionResponse {
    result: Result;
    version: CurveVolatilityParametricSmile | null;
}

export const subjects = {
    list_curve_volatility_parametric_smiles_request:
        'refdata.v1.curve_volatility_parametric_smiles.list',
    get_curve_volatility_parametric_smile_request:
        'refdata.v1.curve_volatility_parametric_smiles.get',
    get_many_curve_volatility_parametric_smiles_request:
        'refdata.v1.curve_volatility_parametric_smiles.get_many',
    put_curve_volatility_parametric_smile_request:
        'refdata.v1.curve_volatility_parametric_smiles.put',
    put_many_curve_volatility_parametric_smiles_request:
        'refdata.v1.curve_volatility_parametric_smiles.put_many',
    delete_curve_volatility_parametric_smile_request:
        'refdata.v1.curve_volatility_parametric_smiles.delete',
    delete_many_curve_volatility_parametric_smiles_request:
        'refdata.v1.curve_volatility_parametric_smiles.delete_many',
    list_curve_volatility_parametric_smile_versions_request:
        'refdata.v1.curve_volatility_parametric_smiles_versions.list',
    get_curve_volatility_parametric_smile_version_request:
        'refdata.v1.curve_volatility_parametric_smiles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_parametric_smiles_request: true,
    get_curve_volatility_parametric_smile_request: true,
    get_many_curve_volatility_parametric_smiles_request: true,
    put_curve_volatility_parametric_smile_request: true,
    put_many_curve_volatility_parametric_smiles_request: true,
    delete_curve_volatility_parametric_smile_request: true,
    delete_many_curve_volatility_parametric_smiles_request: true,
    list_curve_volatility_parametric_smile_versions_request: true,
    get_curve_volatility_parametric_smile_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_parametric_smiles_events.created',
    updated: 'refdata.v1.curve_volatility_parametric_smiles_events.updated',
    deleted: 'refdata.v1.curve_volatility_parametric_smiles_events.deleted',
} as const;
