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
import type { DatasetBundleMember } from '../domain/dataset_bundle_member.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface DatasetBundleMemberKey {
    bundle_code: string;
    dataset_code: string;
}

export interface DatasetBundleMemberWrite {
    bundle_code: string;
    dataset_code: string;
    display_order: number;
    optional: boolean;
}

export interface DatasetBundleMemberChange {
    write: DatasetBundleMemberWrite;
    precondition: Precondition;
}

export interface DatasetBundleMemberRemoval {
    key: DatasetBundleMemberKey;
    precondition: Precondition;
}

export interface DatasetBundleMemberLookup {
    key: DatasetBundleMemberKey;
    dataset_bundle_member: DatasetBundleMember | null;
}

export interface DatasetBundleMembersFilter {
    bundle_code: string | null;
}

export interface DatasetBundleMemberEvent {
    event_id: string;
    key: DatasetBundleMemberKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ListDatasetBundleMembersRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DatasetBundleMembersFilter | null;
}

export interface ListDatasetBundleMembersResponse {
    result: Result;
    dataset_bundle_members: DatasetBundleMember[];
    total: number;
}

export interface GetDatasetBundleMemberRequest {
    key: DatasetBundleMemberKey;
}

export interface GetDatasetBundleMemberResponse {
    result: Result;
    dataset_bundle_member: DatasetBundleMember | null;
}

export interface GetManyDatasetBundleMembersRequest {
    keys: DatasetBundleMemberKey[];
}

export interface GetManyDatasetBundleMembersResponse {
    result: Result;
    entries: DatasetBundleMemberLookup[];
}

export interface PutDatasetBundleMemberRequest {
    change: DatasetBundleMemberChange;
    intent: ChangeIntent;
}

export interface PutDatasetBundleMemberResponse {
    result: Result;
    dataset_bundle_member: DatasetBundleMember;
}

export interface PutManyDatasetBundleMembersRequest {
    changes: DatasetBundleMemberChange[];
    intent: ChangeIntent;
}

export interface PutManyDatasetBundleMembersResponse {
    result: Result;
    dataset_bundle_members: DatasetBundleMember[];
}

export interface DeleteDatasetBundleMemberRequest {
    removal: DatasetBundleMemberRemoval;
    intent: ChangeIntent;
}

export interface DeleteDatasetBundleMemberResponse {
    result: Result;
}

export interface DeleteManyDatasetBundleMembersRequest {
    removals: DatasetBundleMemberRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDatasetBundleMembersResponse {
    result: Result;
}

export interface ListByBundleCodeDatasetBundleMembersRequest {
    bundle_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: DatasetBundleMembersFilter | null;
}

export interface ListByBundleCodeDatasetBundleMembersResponse {
    result: Result;
    dataset_bundle_members: DatasetBundleMember[];
    total: number;
}

export const subjects = {
    list_dataset_bundle_members_request: "dq.v1.dataset_bundle_members.list",
    get_dataset_bundle_member_request: "dq.v1.dataset_bundle_members.get",
    get_many_dataset_bundle_members_request: "dq.v1.dataset_bundle_members.get_many",
    put_dataset_bundle_member_request: "dq.v1.dataset_bundle_members.put",
    put_many_dataset_bundle_members_request: "dq.v1.dataset_bundle_members.put_many",
    delete_dataset_bundle_member_request: "dq.v1.dataset_bundle_members.delete",
    delete_many_dataset_bundle_members_request: "dq.v1.dataset_bundle_members.delete_many",
    list_by_bundle_code_dataset_bundle_members_request: "dq.v1.dataset_bundle_members.list_by_bundle_code",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_dataset_bundle_members_request: true,
    get_dataset_bundle_member_request: true,
    get_many_dataset_bundle_members_request: true,
    put_dataset_bundle_member_request: true,
    put_many_dataset_bundle_members_request: true,
    delete_dataset_bundle_member_request: true,
    delete_many_dataset_bundle_members_request: true,
    list_by_bundle_code_dataset_bundle_members_request: true,
} as const;
