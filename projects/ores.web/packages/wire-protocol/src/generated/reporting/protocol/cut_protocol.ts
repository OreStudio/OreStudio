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
import type { Cut } from '../domain/cut.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CutKey {
    id: string;
}

export interface CutWrite {
    id: string;
    kind_code: string;
    scope_code: string;
    timepoint: string;
    take_ordinal: number;
    business_centre_code: string;
    timepoint_timezone: string;
    cut_time_code: string | null;
    market_data_snapshot_id: string | null;
    deal_population_id: string | null;
    reference_data_version: string | null;
    engine_version: string | null;
    code_version: string | null;
    taken_by: string;
    taken_at: string | null;
    requires_authorisation: boolean;
    approval_request_id: string | null;
    approved_by: string;
    approved_at: string | null;
    refused_at: string | null;
    refusal_comment: string;
    retention_months: number;
    retention_destination_code: string;
    legal_hold: boolean;
}

export interface CutChange {
    write: CutWrite;
    precondition: Precondition;
}

export interface CutRemoval {
    key: CutKey;
    precondition: Precondition;
}

export interface CutLookup {
    key: CutKey;
    cut: Cut | null;
}

export interface CutsFilter {
    kind_code: string | null;
    party_id: string | null;
    business_centre_code: string | null;
    id_one_of: string[] | null;
    kind_code_one_of: string[] | null;
    party_id_one_of: string[] | null;
    business_centre_code_one_of: string[] | null;
}

export interface CutEvent {
    event_id: string;
    key: CutKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CutVersionKey {
    cut: CutKey;
    version: number;
}

export interface CutVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCutsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CutsFilter | null;
    as_of: string | null;
}

export interface ListCutsResponse {
    result: Result;
    cuts: Cut[];
    total: number;
}

export interface GetCutRequest {
    key: CutKey;
}

export interface GetCutResponse {
    result: Result;
    cut: Cut | null;
}

export interface GetManyCutsRequest {
    keys: CutKey[];
}

export interface GetManyCutsResponse {
    result: Result;
    entries: CutLookup[];
}

export interface PutCutRequest {
    change: CutChange;
    intent: ChangeIntent;
}

export interface PutCutResponse {
    result: Result;
    cut: Cut | null;
}

export interface PutManyCutsRequest {
    changes: CutChange[];
    intent: ChangeIntent;
}

export interface PutManyCutsResponse {
    result: Result;
    cuts: Cut[];
}

export interface DeleteCutRequest {
    removal: CutRemoval;
    intent: ChangeIntent;
}

export interface DeleteCutResponse {
    result: Result;
}

export interface DeleteManyCutsRequest {
    removals: CutRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCutsResponse {
    result: Result;
}

export interface ListByKindCodeCutsRequest {
    kind_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CutsFilter | null;
}

export interface ListByKindCodeCutsResponse {
    result: Result;
    cuts: Cut[];
    total: number;
}

export interface ListCutVersionsRequest {
    key: CutKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CutVersionsFilter | null;
}

export interface ListCutVersionsResponse {
    result: Result;
    versions: Cut[];
    total: number;
}

export interface GetCutVersionRequest {
    key: CutVersionKey;
}

export interface GetCutVersionResponse {
    result: Result;
    version: Cut | null;
}

export const subjects = {
    list_cuts_request: 'reporting.v1.cuts.list',
    get_cut_request: 'reporting.v1.cuts.get',
    get_many_cuts_request: 'reporting.v1.cuts.get_many',
    put_cut_request: 'reporting.v1.cuts.put',
    put_many_cuts_request: 'reporting.v1.cuts.put_many',
    delete_cut_request: 'reporting.v1.cuts.delete',
    delete_many_cuts_request: 'reporting.v1.cuts.delete_many',
    list_by_kind_code_cuts_request: 'reporting.v1.cuts.list_by_kind_code',
    list_cut_versions_request: 'reporting.v1.cuts_versions.list',
    get_cut_version_request: 'reporting.v1.cuts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cuts_request: true,
    get_cut_request: true,
    get_many_cuts_request: true,
    put_cut_request: true,
    put_many_cuts_request: true,
    delete_cut_request: true,
    delete_many_cuts_request: true,
    list_by_kind_code_cuts_request: true,
    list_cut_versions_request: true,
    get_cut_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.cuts_events.created',
    updated: 'reporting.v1.cuts_events.updated',
    deleted: 'reporting.v1.cuts_events.deleted',
} as const;
