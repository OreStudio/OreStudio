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
import type { ChangeReason } from '../domain/change_reason.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface ChangeReasonKey {
    code: string;
}

export interface ChangeReasonWrite {
    code: string;
    description: string;
    category_code: string;
    applies_to_new: boolean;
    applies_to_amend: boolean;
    applies_to_delete: boolean;
    requires_commentary: boolean;
    display_order: number;
}

export interface ChangeReasonChange {
    write: ChangeReasonWrite;
    precondition: Precondition;
}

export interface ChangeReasonRemoval {
    key: ChangeReasonKey;
    precondition: Precondition;
}

export interface ChangeReasonLookup {
    key: ChangeReasonKey;
    change_reason: ChangeReason | null;
}

export interface ChangeReasonEvent {
    event_id: string;
    key: ChangeReasonKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface ChangeReasonVersionKey {
    change_reason: ChangeReasonKey;
    version: number;
}

export interface ChangeReasonVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListChangeReasonsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListChangeReasonsResponse {
    result: Result;
    reasons: ChangeReason[];
    total: number;
}

export interface GetChangeReasonRequest {
    key: ChangeReasonKey;
}

export interface GetChangeReasonResponse {
    result: Result;
    change_reason: ChangeReason | null;
}

export interface GetManyChangeReasonsRequest {
    keys: ChangeReasonKey[];
}

export interface GetManyChangeReasonsResponse {
    result: Result;
    entries: ChangeReasonLookup[];
}

export interface PutChangeReasonRequest {
    change: ChangeReasonChange;
    intent: ChangeIntent;
}

export interface PutChangeReasonResponse {
    result: Result;
    change_reason: ChangeReason;
}

export interface PutManyChangeReasonsRequest {
    changes: ChangeReasonChange[];
    intent: ChangeIntent;
}

export interface PutManyChangeReasonsResponse {
    result: Result;
    reasons: ChangeReason[];
}

export interface DeleteChangeReasonRequest {
    removal: ChangeReasonRemoval;
    intent: ChangeIntent;
}

export interface DeleteChangeReasonResponse {
    result: Result;
}

export interface DeleteManyChangeReasonsRequest {
    removals: ChangeReasonRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyChangeReasonsResponse {
    result: Result;
}

export interface ListChangeReasonVersionsRequest {
    key: ChangeReasonKey;
    offset: number;
    limit: number;
    order: Order;
    filter: ChangeReasonVersionsFilter | null;
}

export interface ListChangeReasonVersionsResponse {
    result: Result;
    versions: ChangeReason[];
    total: number;
}

export interface GetChangeReasonVersionRequest {
    key: ChangeReasonVersionKey;
}

export interface GetChangeReasonVersionResponse {
    result: Result;
    version: ChangeReason;
}

export const subjects = {
    list_change_reasons_request: "dq.v1.change_reasons.list",
    get_change_reason_request: "dq.v1.change_reasons.get",
    get_many_change_reasons_request: "dq.v1.change_reasons.get_many",
    put_change_reason_request: "dq.v1.change_reasons.put",
    put_many_change_reasons_request: "dq.v1.change_reasons.put_many",
    delete_change_reason_request: "dq.v1.change_reasons.delete",
    delete_many_change_reasons_request: "dq.v1.change_reasons.delete_many",
    list_change_reason_versions_request: "dq.v1.change_reasons_versions.list",
    get_change_reason_version_request: "dq.v1.change_reasons_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_change_reasons_request: true,
    get_change_reason_request: true,
    get_many_change_reasons_request: true,
    put_change_reason_request: true,
    put_many_change_reasons_request: true,
    delete_change_reason_request: true,
    delete_many_change_reasons_request: true,
    list_change_reason_versions_request: true,
    get_change_reason_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "dq.v1.change_reasons_events.created",
    updated: "dq.v1.change_reasons_events.updated",
    deleted: "dq.v1.change_reasons_events.deleted",
} as const;
