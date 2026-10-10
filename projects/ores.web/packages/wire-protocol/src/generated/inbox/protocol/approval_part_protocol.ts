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
import type { ApprovalPart } from '../domain/approval_part.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ApprovalPartKey {
    code: string;
}

export interface ApprovalPartWrite {
    code: string;
    name: string;
    description: string;
    decide_permission_code: string;
    answer_order: number;
    display_order: number;
}

export interface ApprovalPartChange {
    write: ApprovalPartWrite;
    precondition: Precondition;
}

export interface ApprovalPartRemoval {
    key: ApprovalPartKey;
    precondition: Precondition;
}

export interface ApprovalPartLookup {
    key: ApprovalPartKey;
    approval_part: ApprovalPart | null;
}

export interface ApprovalPartsFilter {
    code_one_of: string[] | null;
}

export interface ApprovalPartEvent {
    event_id: string;
    key: ApprovalPartKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ApprovalPartVersionKey {
    approval_part: ApprovalPartKey;
    version: number;
}

export interface ApprovalPartVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListApprovalPartsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalPartsFilter | null;
    as_of: string | null;
}

export interface ListApprovalPartsResponse {
    result: Result;
    parts: ApprovalPart[];
    total: number;
}

export interface GetApprovalPartRequest {
    key: ApprovalPartKey;
}

export interface GetApprovalPartResponse {
    result: Result;
    approval_part: ApprovalPart | null;
}

export interface GetManyApprovalPartsRequest {
    keys: ApprovalPartKey[];
}

export interface GetManyApprovalPartsResponse {
    result: Result;
    entries: ApprovalPartLookup[];
}

export interface PutApprovalPartRequest {
    change: ApprovalPartChange;
    intent: ChangeIntent;
}

export interface PutApprovalPartResponse {
    result: Result;
    approval_part: ApprovalPart | null;
}

export interface PutManyApprovalPartsRequest {
    changes: ApprovalPartChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalPartsResponse {
    result: Result;
    parts: ApprovalPart[];
}

export interface DeleteApprovalPartRequest {
    removal: ApprovalPartRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalPartResponse {
    result: Result;
}

export interface DeleteManyApprovalPartsRequest {
    removals: ApprovalPartRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalPartsResponse {
    result: Result;
}

export interface ListApprovalPartVersionsRequest {
    key: ApprovalPartKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalPartVersionsFilter | null;
}

export interface ListApprovalPartVersionsResponse {
    result: Result;
    versions: ApprovalPart[];
    total: number;
}

export interface GetApprovalPartVersionRequest {
    key: ApprovalPartVersionKey;
}

export interface GetApprovalPartVersionResponse {
    result: Result;
    version: ApprovalPart | null;
}

export const subjects = {
    list_approval_parts_request: 'inbox.v1.approval_parts.list',
    get_approval_part_request: 'inbox.v1.approval_parts.get',
    get_many_approval_parts_request: 'inbox.v1.approval_parts.get_many',
    put_approval_part_request: 'inbox.v1.approval_parts.put',
    put_many_approval_parts_request: 'inbox.v1.approval_parts.put_many',
    delete_approval_part_request: 'inbox.v1.approval_parts.delete',
    delete_many_approval_parts_request: 'inbox.v1.approval_parts.delete_many',
    list_approval_part_versions_request: 'inbox.v1.approval_parts_versions.list',
    get_approval_part_version_request: 'inbox.v1.approval_parts_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_parts_request: true,
    get_approval_part_request: true,
    get_many_approval_parts_request: true,
    put_approval_part_request: true,
    put_many_approval_parts_request: true,
    delete_approval_part_request: true,
    delete_many_approval_parts_request: true,
    list_approval_part_versions_request: true,
    get_approval_part_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.approval_parts_events.created',
    updated: 'inbox.v1.approval_parts_events.updated',
    deleted: 'inbox.v1.approval_parts_events.deleted',
} as const;
