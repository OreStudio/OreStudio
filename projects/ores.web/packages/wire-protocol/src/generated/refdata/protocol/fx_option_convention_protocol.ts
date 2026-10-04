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
import type { FxOptionConvention } from '../domain/fx_option_convention.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FxOptionConventionKey {
    id: string;
}

export interface FxOptionConventionWrite {
    id: string;
    fx_convention_id: string | null;
    atm_type: string;
    delta_type: string;
    switch_tenor: string | null;
    long_term_atm_type: string | null;
    long_term_delta_type: string | null;
    risk_reversal_in_favor_of: string | null;
    butterfly_style: string | null;
}

export interface FxOptionConventionChange {
    write: FxOptionConventionWrite;
    precondition: Precondition;
}

export interface FxOptionConventionRemoval {
    key: FxOptionConventionKey;
    precondition: Precondition;
}

export interface FxOptionConventionLookup {
    key: FxOptionConventionKey;
    fx_option_convention: FxOptionConvention | null;
}

export interface FxOptionConventionsFilter {
    id_one_of: string[] | null;
}

export interface FxOptionConventionEvent {
    event_id: string;
    key: FxOptionConventionKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FxOptionConventionVersionKey {
    fx_option_convention: FxOptionConventionKey;
    version: number;
}

export interface FxOptionConventionVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFxOptionConventionsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FxOptionConventionsFilter | null;
}

export interface ListFxOptionConventionsResponse {
    result: Result;
    fx_option_conventions: FxOptionConvention[];
    total: number;
}

export interface GetFxOptionConventionRequest {
    key: FxOptionConventionKey;
}

export interface GetFxOptionConventionResponse {
    result: Result;
    fx_option_convention: FxOptionConvention | null;
}

export interface GetManyFxOptionConventionsRequest {
    keys: FxOptionConventionKey[];
}

export interface GetManyFxOptionConventionsResponse {
    result: Result;
    entries: FxOptionConventionLookup[];
}

export interface PutFxOptionConventionRequest {
    change: FxOptionConventionChange;
    intent: ChangeIntent;
}

export interface PutFxOptionConventionResponse {
    result: Result;
    fx_option_convention: FxOptionConvention | null;
}

export interface PutManyFxOptionConventionsRequest {
    changes: FxOptionConventionChange[];
    intent: ChangeIntent;
}

export interface PutManyFxOptionConventionsResponse {
    result: Result;
    fx_option_conventions: FxOptionConvention[];
}

export interface DeleteFxOptionConventionRequest {
    removal: FxOptionConventionRemoval;
    intent: ChangeIntent;
}

export interface DeleteFxOptionConventionResponse {
    result: Result;
}

export interface DeleteManyFxOptionConventionsRequest {
    removals: FxOptionConventionRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFxOptionConventionsResponse {
    result: Result;
}

export interface ListFxOptionConventionVersionsRequest {
    key: FxOptionConventionKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FxOptionConventionVersionsFilter | null;
}

export interface ListFxOptionConventionVersionsResponse {
    result: Result;
    versions: FxOptionConvention[];
    total: number;
}

export interface GetFxOptionConventionVersionRequest {
    key: FxOptionConventionVersionKey;
}

export interface GetFxOptionConventionVersionResponse {
    result: Result;
    version: FxOptionConvention | null;
}

export const subjects = {
    list_fx_option_conventions_request: 'refdata.v1.fx_option_conventions.list',
    get_fx_option_convention_request: 'refdata.v1.fx_option_conventions.get',
    get_many_fx_option_conventions_request: 'refdata.v1.fx_option_conventions.get_many',
    put_fx_option_convention_request: 'refdata.v1.fx_option_conventions.put',
    put_many_fx_option_conventions_request: 'refdata.v1.fx_option_conventions.put_many',
    delete_fx_option_convention_request: 'refdata.v1.fx_option_conventions.delete',
    delete_many_fx_option_conventions_request: 'refdata.v1.fx_option_conventions.delete_many',
    list_fx_option_convention_versions_request: 'refdata.v1.fx_option_conventions_versions.list',
    get_fx_option_convention_version_request: 'refdata.v1.fx_option_conventions_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_fx_option_conventions_request: true,
    get_fx_option_convention_request: true,
    get_many_fx_option_conventions_request: true,
    put_fx_option_convention_request: true,
    put_many_fx_option_conventions_request: true,
    delete_fx_option_convention_request: true,
    delete_many_fx_option_conventions_request: true,
    list_fx_option_convention_versions_request: true,
    get_fx_option_convention_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.fx_option_conventions_events.created',
    updated: 'refdata.v1.fx_option_conventions_events.updated',
    deleted: 'refdata.v1.fx_option_conventions_events.deleted',
} as const;
