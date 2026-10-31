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
import type { CutGap } from '../domain/cut_gap.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CutGapKey {
    id: string;
}

export interface CutGapWrite {
    id: string;
    cut_id: string;
    part_code: string;
    item: string;
    reason: string;
}

export interface CutGapChange {
    write: CutGapWrite;
    precondition: Precondition;
}

export interface CutGapRemoval {
    key: CutGapKey;
    precondition: Precondition;
}

export interface CutGapLookup {
    key: CutGapKey;
    cut_gap: CutGap | null;
}

export interface CutGapsFilter {
    cut_id: string | null;
    party_id: string | null;
    id_one_of: string[] | null;
    cut_id_one_of: string[] | null;
    party_id_one_of: string[] | null;
}

export interface CutGapEvent {
    event_id: string;
    key: CutGapKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CutGapVersionKey {
    cut_gap: CutGapKey;
    version: number;
}

export interface CutGapVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCutGapsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CutGapsFilter | null;
    as_of: string | null;
}

export interface ListCutGapsResponse {
    result: Result;
    gaps: CutGap[];
    total: number;
}

export interface GetCutGapRequest {
    key: CutGapKey;
}

export interface GetCutGapResponse {
    result: Result;
    cut_gap: CutGap | null;
}

export interface GetManyCutGapsRequest {
    keys: CutGapKey[];
}

export interface GetManyCutGapsResponse {
    result: Result;
    entries: CutGapLookup[];
}

export interface PutCutGapRequest {
    change: CutGapChange;
    intent: ChangeIntent;
}

export interface PutCutGapResponse {
    result: Result;
    cut_gap: CutGap | null;
}

export interface PutManyCutGapsRequest {
    changes: CutGapChange[];
    intent: ChangeIntent;
}

export interface PutManyCutGapsResponse {
    result: Result;
    gaps: CutGap[];
}

export interface DeleteCutGapRequest {
    removal: CutGapRemoval;
    intent: ChangeIntent;
}

export interface DeleteCutGapResponse {
    result: Result;
}

export interface DeleteManyCutGapsRequest {
    removals: CutGapRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCutGapsResponse {
    result: Result;
}

export interface ListByCutIdCutGapsRequest {
    cut_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CutGapsFilter | null;
}

export interface ListByCutIdCutGapsResponse {
    result: Result;
    gaps: CutGap[];
    total: number;
}

export interface ListCutGapVersionsRequest {
    key: CutGapKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CutGapVersionsFilter | null;
}

export interface ListCutGapVersionsResponse {
    result: Result;
    versions: CutGap[];
    total: number;
}

export interface GetCutGapVersionRequest {
    key: CutGapVersionKey;
}

export interface GetCutGapVersionResponse {
    result: Result;
    version: CutGap | null;
}

export const subjects = {
    list_cut_gaps_request: 'reporting.v1.cut_gaps.list',
    get_cut_gap_request: 'reporting.v1.cut_gaps.get',
    get_many_cut_gaps_request: 'reporting.v1.cut_gaps.get_many',
    put_cut_gap_request: 'reporting.v1.cut_gaps.put',
    put_many_cut_gaps_request: 'reporting.v1.cut_gaps.put_many',
    delete_cut_gap_request: 'reporting.v1.cut_gaps.delete',
    delete_many_cut_gaps_request: 'reporting.v1.cut_gaps.delete_many',
    list_by_cut_id_cut_gaps_request: 'reporting.v1.cut_gaps.list_by_cut_id',
    list_cut_gap_versions_request: 'reporting.v1.cut_gaps_versions.list',
    get_cut_gap_version_request: 'reporting.v1.cut_gaps_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cut_gaps_request: true,
    get_cut_gap_request: true,
    get_many_cut_gaps_request: true,
    put_cut_gap_request: true,
    put_many_cut_gaps_request: true,
    delete_cut_gap_request: true,
    delete_many_cut_gaps_request: true,
    list_by_cut_id_cut_gaps_request: true,
    list_cut_gap_versions_request: true,
    get_cut_gap_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.cut_gaps_events.created',
    updated: 'reporting.v1.cut_gaps_events.updated',
    deleted: 'reporting.v1.cut_gaps_events.deleted',
} as const;
