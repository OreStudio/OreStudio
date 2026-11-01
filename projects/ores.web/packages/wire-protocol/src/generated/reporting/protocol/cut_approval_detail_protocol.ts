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
import type { CutApprovalDetail } from '../domain/cut_approval_detail.js';
import type { Order } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface CutApprovalDetailKey {
    id: string;
}

export interface CutApprovalDetailLookup {
    key: CutApprovalDetailKey;
    cut_approval_detail: CutApprovalDetail | null;
}

export interface CutApprovalDetailsFilter {
    request_id: string | null;
    cut_id: string | null;
    party_id: string | null;
    id_one_of: string[] | null;
    request_id_one_of: string[] | null;
    cut_id_one_of: string[] | null;
    party_id_one_of: string[] | null;
}

export interface CutApprovalDetailEvent {
    event_id: string;
    key: CutApprovalDetailKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CutApprovalDetailVersionKey {
    cut_approval_detail: CutApprovalDetailKey;
    version: number;
}

export interface CutApprovalDetailVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCutApprovalDetailsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CutApprovalDetailsFilter | null;
    as_of: string | null;
}

export interface ListCutApprovalDetailsResponse {
    result: Result;
    details: CutApprovalDetail[];
    total: number;
}

export interface GetCutApprovalDetailRequest {
    key: CutApprovalDetailKey;
}

export interface GetCutApprovalDetailResponse {
    result: Result;
    cut_approval_detail: CutApprovalDetail | null;
}

export interface GetManyCutApprovalDetailsRequest {
    keys: CutApprovalDetailKey[];
}

export interface GetManyCutApprovalDetailsResponse {
    result: Result;
    entries: CutApprovalDetailLookup[];
}

export interface ListByRequestIdCutApprovalDetailsRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CutApprovalDetailsFilter | null;
}

export interface ListByRequestIdCutApprovalDetailsResponse {
    result: Result;
    details: CutApprovalDetail[];
    total: number;
}

export interface ListByCutIdCutApprovalDetailsRequest {
    cut_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: CutApprovalDetailsFilter | null;
}

export interface ListByCutIdCutApprovalDetailsResponse {
    result: Result;
    details: CutApprovalDetail[];
    total: number;
}

export interface ListCutApprovalDetailVersionsRequest {
    key: CutApprovalDetailKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CutApprovalDetailVersionsFilter | null;
}

export interface ListCutApprovalDetailVersionsResponse {
    result: Result;
    versions: CutApprovalDetail[];
    total: number;
}

export interface GetCutApprovalDetailVersionRequest {
    key: CutApprovalDetailVersionKey;
}

export interface GetCutApprovalDetailVersionResponse {
    result: Result;
    version: CutApprovalDetail | null;
}

export const subjects = {
    list_cut_approval_details_request: 'reporting.v1.cut_approval_details.list',
    get_cut_approval_detail_request: 'reporting.v1.cut_approval_details.get',
    get_many_cut_approval_details_request: 'reporting.v1.cut_approval_details.get_many',
    list_by_request_id_cut_approval_details_request:
        'reporting.v1.cut_approval_details.list_by_request_id',
    list_by_cut_id_cut_approval_details_request: 'reporting.v1.cut_approval_details.list_by_cut_id',
    list_cut_approval_detail_versions_request: 'reporting.v1.cut_approval_details_versions.list',
    get_cut_approval_detail_version_request: 'reporting.v1.cut_approval_details_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cut_approval_details_request: true,
    get_cut_approval_detail_request: true,
    get_many_cut_approval_details_request: true,
    list_by_request_id_cut_approval_details_request: true,
    list_by_cut_id_cut_approval_details_request: true,
    list_cut_approval_detail_versions_request: true,
    get_cut_approval_detail_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'reporting.v1.cut_approval_details_events.created',
    updated: 'reporting.v1.cut_approval_details_events.updated',
    deleted: 'reporting.v1.cut_approval_details_events.deleted',
} as const;
