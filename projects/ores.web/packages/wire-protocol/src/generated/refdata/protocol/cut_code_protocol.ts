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
import type { CutCode } from '../domain/cut_code.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CutCodeKey {
    code: string;
}

export interface CutCodeWrite {
    code: string;
    name: string;
    description: string;
    cut_time: string;
    cut_time_zone: string;
    fixing_source_code: string;
    display_order: number;
}

export interface CutCodeChange {
    write: CutCodeWrite;
    precondition: Precondition;
}

export interface CutCodeRemoval {
    key: CutCodeKey;
    precondition: Precondition;
}

export interface CutCodeLookup {
    key: CutCodeKey;
    cut_code: CutCode | null;
}

export interface CutCodesFilter {
    code_one_of: string[] | null;
}

export interface CutCodeEvent {
    event_id: string;
    key: CutCodeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CutCodeVersionKey {
    cut_code: CutCodeKey;
    version: number;
}

export interface CutCodeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCutCodesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CutCodesFilter | null;
    as_of: string | null;
}

export interface ListCutCodesResponse {
    result: Result;
    cuts: CutCode[];
    total: number;
}

export interface GetCutCodeRequest {
    key: CutCodeKey;
}

export interface GetCutCodeResponse {
    result: Result;
    cut_code: CutCode | null;
}

export interface GetManyCutCodesRequest {
    keys: CutCodeKey[];
}

export interface GetManyCutCodesResponse {
    result: Result;
    entries: CutCodeLookup[];
}

export interface PutCutCodeRequest {
    change: CutCodeChange;
    intent: ChangeIntent;
}

export interface PutCutCodeResponse {
    result: Result;
    cut_code: CutCode | null;
}

export interface PutManyCutCodesRequest {
    changes: CutCodeChange[];
    intent: ChangeIntent;
}

export interface PutManyCutCodesResponse {
    result: Result;
    cuts: CutCode[];
}

export interface DeleteCutCodeRequest {
    removal: CutCodeRemoval;
    intent: ChangeIntent;
}

export interface DeleteCutCodeResponse {
    result: Result;
}

export interface DeleteManyCutCodesRequest {
    removals: CutCodeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCutCodesResponse {
    result: Result;
}

export interface ListCutCodeVersionsRequest {
    key: CutCodeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CutCodeVersionsFilter | null;
}

export interface ListCutCodeVersionsResponse {
    result: Result;
    versions: CutCode[];
    total: number;
}

export interface GetCutCodeVersionRequest {
    key: CutCodeVersionKey;
}

export interface GetCutCodeVersionResponse {
    result: Result;
    version: CutCode | null;
}

export const subjects = {
    list_cut_codes_request: 'refdata.v1.cut_codes.list',
    get_cut_code_request: 'refdata.v1.cut_codes.get',
    get_many_cut_codes_request: 'refdata.v1.cut_codes.get_many',
    put_cut_code_request: 'refdata.v1.cut_codes.put',
    put_many_cut_codes_request: 'refdata.v1.cut_codes.put_many',
    delete_cut_code_request: 'refdata.v1.cut_codes.delete',
    delete_many_cut_codes_request: 'refdata.v1.cut_codes.delete_many',
    list_cut_code_versions_request: 'refdata.v1.cut_codes_versions.list',
    get_cut_code_version_request: 'refdata.v1.cut_codes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cut_codes_request: true,
    get_cut_code_request: true,
    get_many_cut_codes_request: true,
    put_cut_code_request: true,
    put_many_cut_codes_request: true,
    delete_cut_code_request: true,
    delete_many_cut_codes_request: true,
    list_cut_code_versions_request: true,
    get_cut_code_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.events.cut_codes.created',
    updated: 'refdata.v1.events.cut_codes.updated',
    deleted: 'refdata.v1.events.cut_codes.deleted',
} as const;
