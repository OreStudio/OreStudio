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
import type { IrCurveIndexFamily } from '../domain/ir_curve_index_family.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface IrCurveIndexFamilyKey {
    code: string;
}

export interface IrCurveIndexFamilyWrite {
    code: string;
    name: string;
    description: string;
    admits_overnight: boolean;
    admits_term: boolean;
    dated_tenor: string;
    display_order: number;
}

export interface IrCurveIndexFamilyChange {
    write: IrCurveIndexFamilyWrite;
    precondition: Precondition;
}

export interface IrCurveIndexFamilyRemoval {
    key: IrCurveIndexFamilyKey;
    precondition: Precondition;
}

export interface IrCurveIndexFamilyLookup {
    key: IrCurveIndexFamilyKey;
    ir_curve_index_family: IrCurveIndexFamily | null;
}

export interface IrCurveIndexFamiliesFilter {
    code_one_of: string[] | null;
}

export interface IrCurveIndexFamilyEvent {
    event_id: string;
    key: IrCurveIndexFamilyKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface IrCurveIndexFamilyVersionKey {
    ir_curve_index_family: IrCurveIndexFamilyKey;
    version: number;
}

export interface IrCurveIndexFamilyVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListIrCurveIndexFamiliesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveIndexFamiliesFilter | null;
    as_of: string | null;
}

export interface ListIrCurveIndexFamiliesResponse {
    result: Result;
    index_families: IrCurveIndexFamily[];
    total: number;
}

export interface GetIrCurveIndexFamilyRequest {
    key: IrCurveIndexFamilyKey;
}

export interface GetIrCurveIndexFamilyResponse {
    result: Result;
    ir_curve_index_family: IrCurveIndexFamily | null;
}

export interface GetManyIrCurveIndexFamiliesRequest {
    keys: IrCurveIndexFamilyKey[];
}

export interface GetManyIrCurveIndexFamiliesResponse {
    result: Result;
    entries: IrCurveIndexFamilyLookup[];
}

export interface PutIrCurveIndexFamilyRequest {
    change: IrCurveIndexFamilyChange;
    intent: ChangeIntent;
}

export interface PutIrCurveIndexFamilyResponse {
    result: Result;
    ir_curve_index_family: IrCurveIndexFamily | null;
}

export interface PutManyIrCurveIndexFamiliesRequest {
    changes: IrCurveIndexFamilyChange[];
    intent: ChangeIntent;
}

export interface PutManyIrCurveIndexFamiliesResponse {
    result: Result;
    index_families: IrCurveIndexFamily[];
}

export interface DeleteIrCurveIndexFamilyRequest {
    removal: IrCurveIndexFamilyRemoval;
    intent: ChangeIntent;
}

export interface DeleteIrCurveIndexFamilyResponse {
    result: Result;
}

export interface DeleteManyIrCurveIndexFamiliesRequest {
    removals: IrCurveIndexFamilyRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyIrCurveIndexFamiliesResponse {
    result: Result;
}

export interface ListIrCurveIndexFamilyVersionsRequest {
    key: IrCurveIndexFamilyKey;
    offset: number;
    limit: number;
    order: Order;
    filter: IrCurveIndexFamilyVersionsFilter | null;
}

export interface ListIrCurveIndexFamilyVersionsResponse {
    result: Result;
    versions: IrCurveIndexFamily[];
    total: number;
}

export interface GetIrCurveIndexFamilyVersionRequest {
    key: IrCurveIndexFamilyVersionKey;
}

export interface GetIrCurveIndexFamilyVersionResponse {
    result: Result;
    version: IrCurveIndexFamily | null;
}

export const subjects = {
    list_ir_curve_index_families_request: 'synthetic.v1.ir_curve_index_families.list',
    get_ir_curve_index_family_request: 'synthetic.v1.ir_curve_index_families.get',
    get_many_ir_curve_index_families_request: 'synthetic.v1.ir_curve_index_families.get_many',
    put_ir_curve_index_family_request: 'synthetic.v1.ir_curve_index_families.put',
    put_many_ir_curve_index_families_request: 'synthetic.v1.ir_curve_index_families.put_many',
    delete_ir_curve_index_family_request: 'synthetic.v1.ir_curve_index_families.delete',
    delete_many_ir_curve_index_families_request: 'synthetic.v1.ir_curve_index_families.delete_many',
    list_ir_curve_index_family_versions_request:
        'synthetic.v1.ir_curve_index_families_versions.list',
    get_ir_curve_index_family_version_request: 'synthetic.v1.ir_curve_index_families_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ir_curve_index_families_request: true,
    get_ir_curve_index_family_request: true,
    get_many_ir_curve_index_families_request: true,
    put_ir_curve_index_family_request: true,
    put_many_ir_curve_index_families_request: true,
    delete_ir_curve_index_family_request: true,
    delete_many_ir_curve_index_families_request: true,
    list_ir_curve_index_family_versions_request: true,
    get_ir_curve_index_family_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'synthetic.v1.events.ir_curve_index_families.created',
    updated: 'synthetic.v1.events.ir_curve_index_families.updated',
    deleted: 'synthetic.v1.events.ir_curve_index_families.deleted',
} as const;
