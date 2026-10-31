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
import type { CurveVolatilitySmile } from '../domain/curve_volatility_smile.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CurveVolatilitySmileKey {
    id: string;
}

export interface CurveVolatilitySmileWrite {
    id: string;
    curve_volatility_kind_id: string;
    max_calibration_attempts: number;
    exit_early_error_threshold: number;
    max_acceptable_error: number;
    residual_correction_dimension: string | null;
}

export interface CurveVolatilitySmileChange {
    write: CurveVolatilitySmileWrite;
    precondition: Precondition;
}

export interface CurveVolatilitySmileRemoval {
    key: CurveVolatilitySmileKey;
    precondition: Precondition;
}

export interface CurveVolatilitySmileLookup {
    key: CurveVolatilitySmileKey;
    curve_volatility_smile: CurveVolatilitySmile | null;
}

export interface CurveVolatilitySmilesFilter {
    id_one_of: string[] | null;
}

export interface CurveVolatilitySmileEvent {
    event_id: string;
    key: CurveVolatilitySmileKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CurveVolatilitySmileVersionKey {
    curve_volatility_smile: CurveVolatilitySmileKey;
    version: number;
}

export interface CurveVolatilitySmileVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCurveVolatilitySmilesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilitySmilesFilter | null;
    as_of: string | null;
}

export interface ListCurveVolatilitySmilesResponse {
    result: Result;
    curve_volatility_smiles: CurveVolatilitySmile[];
    total: number;
}

export interface GetCurveVolatilitySmileRequest {
    key: CurveVolatilitySmileKey;
}

export interface GetCurveVolatilitySmileResponse {
    result: Result;
    curve_volatility_smile: CurveVolatilitySmile | null;
}

export interface GetManyCurveVolatilitySmilesRequest {
    keys: CurveVolatilitySmileKey[];
}

export interface GetManyCurveVolatilitySmilesResponse {
    result: Result;
    entries: CurveVolatilitySmileLookup[];
}

export interface PutCurveVolatilitySmileRequest {
    change: CurveVolatilitySmileChange;
    intent: ChangeIntent;
}

export interface PutCurveVolatilitySmileResponse {
    result: Result;
    curve_volatility_smile: CurveVolatilitySmile | null;
}

export interface PutManyCurveVolatilitySmilesRequest {
    changes: CurveVolatilitySmileChange[];
    intent: ChangeIntent;
}

export interface PutManyCurveVolatilitySmilesResponse {
    result: Result;
    curve_volatility_smiles: CurveVolatilitySmile[];
}

export interface DeleteCurveVolatilitySmileRequest {
    removal: CurveVolatilitySmileRemoval;
    intent: ChangeIntent;
}

export interface DeleteCurveVolatilitySmileResponse {
    result: Result;
}

export interface DeleteManyCurveVolatilitySmilesRequest {
    removals: CurveVolatilitySmileRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCurveVolatilitySmilesResponse {
    result: Result;
}

export interface ListCurveVolatilitySmileVersionsRequest {
    key: CurveVolatilitySmileKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CurveVolatilitySmileVersionsFilter | null;
}

export interface ListCurveVolatilitySmileVersionsResponse {
    result: Result;
    versions: CurveVolatilitySmile[];
    total: number;
}

export interface GetCurveVolatilitySmileVersionRequest {
    key: CurveVolatilitySmileVersionKey;
}

export interface GetCurveVolatilitySmileVersionResponse {
    result: Result;
    version: CurveVolatilitySmile | null;
}

export const subjects = {
    list_curve_volatility_smiles_request: 'refdata.v1.curve_volatility_smiles.list',
    get_curve_volatility_smile_request: 'refdata.v1.curve_volatility_smiles.get',
    get_many_curve_volatility_smiles_request: 'refdata.v1.curve_volatility_smiles.get_many',
    put_curve_volatility_smile_request: 'refdata.v1.curve_volatility_smiles.put',
    put_many_curve_volatility_smiles_request: 'refdata.v1.curve_volatility_smiles.put_many',
    delete_curve_volatility_smile_request: 'refdata.v1.curve_volatility_smiles.delete',
    delete_many_curve_volatility_smiles_request: 'refdata.v1.curve_volatility_smiles.delete_many',
    list_curve_volatility_smile_versions_request:
        'refdata.v1.curve_volatility_smiles_versions.list',
    get_curve_volatility_smile_version_request: 'refdata.v1.curve_volatility_smiles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_curve_volatility_smiles_request: true,
    get_curve_volatility_smile_request: true,
    get_many_curve_volatility_smiles_request: true,
    put_curve_volatility_smile_request: true,
    put_many_curve_volatility_smiles_request: true,
    delete_curve_volatility_smile_request: true,
    delete_many_curve_volatility_smiles_request: true,
    list_curve_volatility_smile_versions_request: true,
    get_curve_volatility_smile_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.curve_volatility_smiles_events.created',
    updated: 'refdata.v1.curve_volatility_smiles_events.updated',
    deleted: 'refdata.v1.curve_volatility_smiles_events.deleted',
} as const;
