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
import type { SubPeriodsCouponType } from '../domain/sub_periods_coupon_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SubPeriodsCouponTypeKey {
    code: string;
}

export interface SubPeriodsCouponTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface SubPeriodsCouponTypeChange {
    write: SubPeriodsCouponTypeWrite;
    precondition: Precondition;
}

export interface SubPeriodsCouponTypeRemoval {
    key: SubPeriodsCouponTypeKey;
    precondition: Precondition;
}

export interface SubPeriodsCouponTypeLookup {
    key: SubPeriodsCouponTypeKey;
    sub_periods_coupon_type: SubPeriodsCouponType | null;
}

export interface SubPeriodsCouponTypesFilter {
    code_one_of: string[] | null;
}

export interface SubPeriodsCouponTypeEvent {
    event_id: string;
    key: SubPeriodsCouponTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SubPeriodsCouponTypeVersionKey {
    sub_periods_coupon_type: SubPeriodsCouponTypeKey;
    version: number;
}

export interface SubPeriodsCouponTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSubPeriodsCouponTypesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SubPeriodsCouponTypesFilter | null;
    as_of: string | null;
}

export interface ListSubPeriodsCouponTypesResponse {
    result: Result;
    types: SubPeriodsCouponType[];
    total: number;
}

export interface GetSubPeriodsCouponTypeRequest {
    key: SubPeriodsCouponTypeKey;
}

export interface GetSubPeriodsCouponTypeResponse {
    result: Result;
    sub_periods_coupon_type: SubPeriodsCouponType | null;
}

export interface GetManySubPeriodsCouponTypesRequest {
    keys: SubPeriodsCouponTypeKey[];
}

export interface GetManySubPeriodsCouponTypesResponse {
    result: Result;
    entries: SubPeriodsCouponTypeLookup[];
}

export interface PutSubPeriodsCouponTypeRequest {
    change: SubPeriodsCouponTypeChange;
    intent: ChangeIntent;
}

export interface PutSubPeriodsCouponTypeResponse {
    result: Result;
    sub_periods_coupon_type: SubPeriodsCouponType | null;
}

export interface PutManySubPeriodsCouponTypesRequest {
    changes: SubPeriodsCouponTypeChange[];
    intent: ChangeIntent;
}

export interface PutManySubPeriodsCouponTypesResponse {
    result: Result;
    types: SubPeriodsCouponType[];
}

export interface DeleteSubPeriodsCouponTypeRequest {
    removal: SubPeriodsCouponTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteSubPeriodsCouponTypeResponse {
    result: Result;
}

export interface DeleteManySubPeriodsCouponTypesRequest {
    removals: SubPeriodsCouponTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySubPeriodsCouponTypesResponse {
    result: Result;
}

export interface ListSubPeriodsCouponTypeVersionsRequest {
    key: SubPeriodsCouponTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SubPeriodsCouponTypeVersionsFilter | null;
}

export interface ListSubPeriodsCouponTypeVersionsResponse {
    result: Result;
    versions: SubPeriodsCouponType[];
    total: number;
}

export interface GetSubPeriodsCouponTypeVersionRequest {
    key: SubPeriodsCouponTypeVersionKey;
}

export interface GetSubPeriodsCouponTypeVersionResponse {
    result: Result;
    version: SubPeriodsCouponType | null;
}

export const subjects = {
    list_sub_periods_coupon_types_request: 'refdata.v1.sub_periods_coupon_types.list',
    get_sub_periods_coupon_type_request: 'refdata.v1.sub_periods_coupon_types.get',
    get_many_sub_periods_coupon_types_request: 'refdata.v1.sub_periods_coupon_types.get_many',
    put_sub_periods_coupon_type_request: 'refdata.v1.sub_periods_coupon_types.put',
    put_many_sub_periods_coupon_types_request: 'refdata.v1.sub_periods_coupon_types.put_many',
    delete_sub_periods_coupon_type_request: 'refdata.v1.sub_periods_coupon_types.delete',
    delete_many_sub_periods_coupon_types_request: 'refdata.v1.sub_periods_coupon_types.delete_many',
    list_sub_periods_coupon_type_versions_request:
        'refdata.v1.sub_periods_coupon_types_versions.list',
    get_sub_periods_coupon_type_version_request: 'refdata.v1.sub_periods_coupon_types_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_sub_periods_coupon_types_request: true,
    get_sub_periods_coupon_type_request: true,
    get_many_sub_periods_coupon_types_request: true,
    put_sub_periods_coupon_type_request: true,
    put_many_sub_periods_coupon_types_request: true,
    delete_sub_periods_coupon_type_request: true,
    delete_many_sub_periods_coupon_types_request: true,
    list_sub_periods_coupon_type_versions_request: true,
    get_sub_periods_coupon_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'refdata.v1.sub_periods_coupon_types_events.created',
    updated: 'refdata.v1.sub_periods_coupon_types_events.updated',
    deleted: 'refdata.v1.sub_periods_coupon_types_events.deleted',
} as const;
