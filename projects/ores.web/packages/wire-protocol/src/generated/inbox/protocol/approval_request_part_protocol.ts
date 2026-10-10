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
import type { ApprovalRequestPart } from '../domain/approval_request_part.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ApprovalRequestPartKey {
    request_id: string;
    part_code: string;
}

export interface ApprovalRequestPartWrite {
    request_id: string;
    part_code: string;
}

export interface ApprovalRequestPartChange {
    write: ApprovalRequestPartWrite;
    precondition: Precondition;
}

export interface ApprovalRequestPartRemoval {
    key: ApprovalRequestPartKey;
    precondition: Precondition;
}

export interface ApprovalRequestPartLookup {
    key: ApprovalRequestPartKey;
    approval_request_part: ApprovalRequestPart | null;
}

export interface ApprovalRequestPartsFilter {
    request_id: string | null;
    request_id_one_of: string[] | null;
}

export interface ListApprovalRequestPartsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalRequestPartsFilter | null;
}

export interface ListApprovalRequestPartsResponse {
    result: Result;
    approval_request_parts: ApprovalRequestPart[];
    total: number;
}

export interface GetApprovalRequestPartRequest {
    key: ApprovalRequestPartKey;
}

export interface GetApprovalRequestPartResponse {
    result: Result;
    approval_request_part: ApprovalRequestPart | null;
}

export interface GetManyApprovalRequestPartsRequest {
    keys: ApprovalRequestPartKey[];
}

export interface GetManyApprovalRequestPartsResponse {
    result: Result;
    entries: ApprovalRequestPartLookup[];
}

export interface PutApprovalRequestPartRequest {
    change: ApprovalRequestPartChange;
    intent: ChangeIntent;
}

export interface PutApprovalRequestPartResponse {
    result: Result;
    approval_request_part: ApprovalRequestPart | null;
}

export interface PutManyApprovalRequestPartsRequest {
    changes: ApprovalRequestPartChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalRequestPartsResponse {
    result: Result;
    approval_request_parts: ApprovalRequestPart[];
}

export interface DeleteApprovalRequestPartRequest {
    removal: ApprovalRequestPartRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalRequestPartResponse {
    result: Result;
}

export interface DeleteManyApprovalRequestPartsRequest {
    removals: ApprovalRequestPartRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalRequestPartsResponse {
    result: Result;
}

export interface ListByRequestIdApprovalRequestPartsRequest {
    request_id: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalRequestPartsFilter | null;
}

export interface ListByRequestIdApprovalRequestPartsResponse {
    result: Result;
    approval_request_parts: ApprovalRequestPart[];
    total: number;
}

export const subjects = {
    list_approval_request_parts_request: 'inbox.v1.approval_request_parts.list',
    get_approval_request_part_request: 'inbox.v1.approval_request_parts.get',
    get_many_approval_request_parts_request: 'inbox.v1.approval_request_parts.get_many',
    put_approval_request_part_request: 'inbox.v1.approval_request_parts.put',
    put_many_approval_request_parts_request: 'inbox.v1.approval_request_parts.put_many',
    delete_approval_request_part_request: 'inbox.v1.approval_request_parts.delete',
    delete_many_approval_request_parts_request: 'inbox.v1.approval_request_parts.delete_many',
    list_by_request_id_approval_request_parts_request:
        'inbox.v1.approval_request_parts.list_by_request_id',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_request_parts_request: true,
    get_approval_request_part_request: true,
    get_many_approval_request_parts_request: true,
    put_approval_request_part_request: true,
    put_many_approval_request_parts_request: true,
    delete_approval_request_part_request: true,
    delete_many_approval_request_parts_request: true,
    list_by_request_id_approval_request_parts_request: true,
} as const;
