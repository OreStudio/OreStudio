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
import type { SwaptionVolatilitySmile } from '../domain/swaption_volatility_smile.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwaptionVolatilitySmileKey {
    id: string;
}

export interface SwaptionVolatilitySmileWrite {
    id: string;
    swaption_volatility_config_id: string;
    smile_option_tenors: string | null;
    smile_swap_tenors: string | null;
    smile_spreads: string | null;
}

export interface SwaptionVolatilitySmileChange {
    write: SwaptionVolatilitySmileWrite;
    precondition: Precondition;
}

export interface SwaptionVolatilitySmileRemoval {
    key: SwaptionVolatilitySmileKey;
    precondition: Precondition;
}

export interface SwaptionVolatilitySmileLookup {
    key: SwaptionVolatilitySmileKey;
    swaption_volatility_smile: SwaptionVolatilitySmile | null;
}

export interface SwaptionVolatilitySmilesFilter {
    id_one_of: string[] | null;
}

export interface SwaptionVolatilitySmileEvent {
    event_id: string;
    key: SwaptionVolatilitySmileKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwaptionVolatilitySmileVersionKey {
    swaption_volatility_smile: SwaptionVolatilitySmileKey;
    version: number;
}

export interface SwaptionVolatilitySmileVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwaptionVolatilitySmilesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SwaptionVolatilitySmilesFilter | null;
    as_of: string | null;
}

export interface ListSwaptionVolatilitySmilesResponse {
    result: Result;
    swaption_volatility_smiles: SwaptionVolatilitySmile[];
    total: number;
}

export interface GetSwaptionVolatilitySmileRequest {
    key: SwaptionVolatilitySmileKey;
}

export interface GetSwaptionVolatilitySmileResponse {
    result: Result;
    swaption_volatility_smile: SwaptionVolatilitySmile | null;
}

export interface GetManySwaptionVolatilitySmilesRequest {
    keys: SwaptionVolatilitySmileKey[];
}

export interface GetManySwaptionVolatilitySmilesResponse {
    result: Result;
    entries: SwaptionVolatilitySmileLookup[];
}

export interface PutSwaptionVolatilitySmileRequest {
    change: SwaptionVolatilitySmileChange;
    intent: ChangeIntent;
}

export interface PutSwaptionVolatilitySmileResponse {
    result: Result;
    swaption_volatility_smile: SwaptionVolatilitySmile | null;
}

export interface PutManySwaptionVolatilitySmilesRequest {
    changes: SwaptionVolatilitySmileChange[];
    intent: ChangeIntent;
}

export interface PutManySwaptionVolatilitySmilesResponse {
    result: Result;
    swaption_volatility_smiles: SwaptionVolatilitySmile[];
}

export interface DeleteSwaptionVolatilitySmileRequest {
    removal: SwaptionVolatilitySmileRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwaptionVolatilitySmileResponse {
    result: Result;
}

export interface DeleteManySwaptionVolatilitySmilesRequest {
    removals: SwaptionVolatilitySmileRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwaptionVolatilitySmilesResponse {
    result: Result;
}

export interface ListSwaptionVolatilitySmileVersionsRequest {
    key: SwaptionVolatilitySmileKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwaptionVolatilitySmileVersionsFilter | null;
}

export interface ListSwaptionVolatilitySmileVersionsResponse {
    result: Result;
    versions: SwaptionVolatilitySmile[];
    total: number;
}

export interface GetSwaptionVolatilitySmileVersionRequest {
    key: SwaptionVolatilitySmileVersionKey;
}

export interface GetSwaptionVolatilitySmileVersionResponse {
    result: Result;
    version: SwaptionVolatilitySmile | null;
}

export const subjects = {
    list_swaption_volatility_smiles_request: 'refdata.v1.swaption_volatility_smiles.list',
    get_swaption_volatility_smile_request: 'refdata.v1.swaption_volatility_smiles.get',
    get_many_swaption_volatility_smiles_request: 'refdata.v1.swaption_volatility_smiles.get_many',
    put_swaption_volatility_smile_request: 'refdata.v1.swaption_volatility_smiles.put',
    put_many_swaption_volatility_smiles_request: 'refdata.v1.swaption_volatility_smiles.put_many',
    delete_swaption_volatility_smile_request: 'refdata.v1.swaption_volatility_smiles.delete',
    delete_many_swaption_volatility_smiles_request:
        'refdata.v1.swaption_volatility_smiles.delete_many',
    list_swaption_volatility_smile_versions_request:
        'refdata.v1.swaption_volatility_smiles_versions.list',
    get_swaption_volatility_smile_version_request:
        'refdata.v1.swaption_volatility_smiles_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swaption_volatility_smiles_request: true,
    get_swaption_volatility_smile_request: true,
    get_many_swaption_volatility_smiles_request: true,
    put_swaption_volatility_smile_request: true,
    put_many_swaption_volatility_smiles_request: true,
    delete_swaption_volatility_smile_request: true,
    delete_many_swaption_volatility_smiles_request: true,
    list_swaption_volatility_smile_versions_request: true,
    get_swaption_volatility_smile_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.swaption_volatility_smiles_events.created',
    updated: 'refdata.v1.swaption_volatility_smiles_events.updated',
    deleted: 'refdata.v1.swaption_volatility_smiles_events.deleted',
} as const;
