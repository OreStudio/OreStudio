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
import type { ApprovalKindPart } from '../domain/approval_kind_part.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';
import type { Scope } from '../../../utility/protocol.js';

export interface ApprovalKindPartKey {
    kind_code: string;
    part_code: string;
}

export interface ApprovalKindPartWrite {
    kind_code: string;
    part_code: string;
}

export interface ApprovalKindPartChange {
    write: ApprovalKindPartWrite;
    precondition: Precondition;
}

export interface ApprovalKindPartRemoval {
    key: ApprovalKindPartKey;
    precondition: Precondition;
}

export interface ApprovalKindPartLookup {
    key: ApprovalKindPartKey;
    approval_kind_part: ApprovalKindPart | null;
}

export interface ApprovalKindPartsFilter {
    kind_code: string | null;
    kind_code_one_of: string[] | null;
}

export interface ListApprovalKindPartsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalKindPartsFilter | null;
}

export interface ListApprovalKindPartsResponse {
    result: Result;
    approval_kind_parts: ApprovalKindPart[];
    total: number;
}

export interface GetApprovalKindPartRequest {
    key: ApprovalKindPartKey;
}

export interface GetApprovalKindPartResponse {
    result: Result;
    approval_kind_part: ApprovalKindPart | null;
}

export interface GetManyApprovalKindPartsRequest {
    keys: ApprovalKindPartKey[];
}

export interface GetManyApprovalKindPartsResponse {
    result: Result;
    entries: ApprovalKindPartLookup[];
}

export interface PutApprovalKindPartRequest {
    change: ApprovalKindPartChange;
    intent: ChangeIntent;
}

export interface PutApprovalKindPartResponse {
    result: Result;
    approval_kind_part: ApprovalKindPart | null;
}

export interface PutManyApprovalKindPartsRequest {
    changes: ApprovalKindPartChange[];
    intent: ChangeIntent;
}

export interface PutManyApprovalKindPartsResponse {
    result: Result;
    approval_kind_parts: ApprovalKindPart[];
}

export interface DeleteApprovalKindPartRequest {
    removal: ApprovalKindPartRemoval;
    intent: ChangeIntent;
}

export interface DeleteApprovalKindPartResponse {
    result: Result;
}

export interface DeleteManyApprovalKindPartsRequest {
    removals: ApprovalKindPartRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyApprovalKindPartsResponse {
    result: Result;
}

export interface ListByKindCodeApprovalKindPartsRequest {
    kind_code: string;
    scope: Scope;
    offset: number;
    limit: number;
    order: Order;
    filter: ApprovalKindPartsFilter | null;
}

export interface ListByKindCodeApprovalKindPartsResponse {
    result: Result;
    approval_kind_parts: ApprovalKindPart[];
    total: number;
}

export const subjects = {
    list_approval_kind_parts_request: 'inbox.v1.approval_kind_parts.list',
    get_approval_kind_part_request: 'inbox.v1.approval_kind_parts.get',
    get_many_approval_kind_parts_request: 'inbox.v1.approval_kind_parts.get_many',
    put_approval_kind_part_request: 'inbox.v1.approval_kind_parts.put',
    put_many_approval_kind_parts_request: 'inbox.v1.approval_kind_parts.put_many',
    delete_approval_kind_part_request: 'inbox.v1.approval_kind_parts.delete',
    delete_many_approval_kind_parts_request: 'inbox.v1.approval_kind_parts.delete_many',
    list_by_kind_code_approval_kind_parts_request: 'inbox.v1.approval_kind_parts.list_by_kind_code',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_approval_kind_parts_request: true,
    get_approval_kind_part_request: true,
    get_many_approval_kind_parts_request: true,
    put_approval_kind_part_request: true,
    put_many_approval_kind_parts_request: true,
    delete_approval_kind_part_request: true,
    delete_many_approval_kind_parts_request: true,
    list_by_kind_code_approval_kind_parts_request: true,
} as const;
