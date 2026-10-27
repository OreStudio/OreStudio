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
import type { HoldReason } from '../domain/hold_reason.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface HoldReasonKey {
    code: string;
}

export interface HoldReasonWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface HoldReasonChange {
    write: HoldReasonWrite;
    precondition: Precondition;
}

export interface HoldReasonRemoval {
    key: HoldReasonKey;
    precondition: Precondition;
}

export interface HoldReasonLookup {
    key: HoldReasonKey;
    hold_reason: HoldReason | null;
}

export interface HoldReasonsFilter {
    code_one_of: string[] | null;
}

export interface HoldReasonEvent {
    event_id: string;
    key: HoldReasonKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface HoldReasonVersionKey {
    hold_reason: HoldReasonKey;
    version: number;
}

export interface HoldReasonVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListHoldReasonsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: HoldReasonsFilter | null;
    as_of: string | null;
}

export interface ListHoldReasonsResponse {
    result: Result;
    reasons: HoldReason[];
    total: number;
}

export interface GetHoldReasonRequest {
    key: HoldReasonKey;
}

export interface GetHoldReasonResponse {
    result: Result;
    hold_reason: HoldReason | null;
}

export interface GetManyHoldReasonsRequest {
    keys: HoldReasonKey[];
}

export interface GetManyHoldReasonsResponse {
    result: Result;
    entries: HoldReasonLookup[];
}

export interface PutHoldReasonRequest {
    change: HoldReasonChange;
    intent: ChangeIntent;
}

export interface PutHoldReasonResponse {
    result: Result;
    hold_reason: HoldReason | null;
}

export interface PutManyHoldReasonsRequest {
    changes: HoldReasonChange[];
    intent: ChangeIntent;
}

export interface PutManyHoldReasonsResponse {
    result: Result;
    reasons: HoldReason[];
}

export interface DeleteHoldReasonRequest {
    removal: HoldReasonRemoval;
    intent: ChangeIntent;
}

export interface DeleteHoldReasonResponse {
    result: Result;
}

export interface DeleteManyHoldReasonsRequest {
    removals: HoldReasonRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyHoldReasonsResponse {
    result: Result;
}

export interface ListHoldReasonVersionsRequest {
    key: HoldReasonKey;
    offset: number;
    limit: number;
    order: Order;
    filter: HoldReasonVersionsFilter | null;
}

export interface ListHoldReasonVersionsResponse {
    result: Result;
    versions: HoldReason[];
    total: number;
}

export interface GetHoldReasonVersionRequest {
    key: HoldReasonVersionKey;
}

export interface GetHoldReasonVersionResponse {
    result: Result;
    version: HoldReason | null;
}

export const subjects = {
    list_hold_reasons_request: 'inbox.v1.hold_reasons.list',
    get_hold_reason_request: 'inbox.v1.hold_reasons.get',
    get_many_hold_reasons_request: 'inbox.v1.hold_reasons.get_many',
    put_hold_reason_request: 'inbox.v1.hold_reasons.put',
    put_many_hold_reasons_request: 'inbox.v1.hold_reasons.put_many',
    delete_hold_reason_request: 'inbox.v1.hold_reasons.delete',
    delete_many_hold_reasons_request: 'inbox.v1.hold_reasons.delete_many',
    list_hold_reason_versions_request: 'inbox.v1.hold_reasons_versions.list',
    get_hold_reason_version_request: 'inbox.v1.hold_reasons_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_hold_reasons_request: true,
    get_hold_reason_request: true,
    get_many_hold_reasons_request: true,
    put_hold_reason_request: true,
    put_many_hold_reasons_request: true,
    delete_hold_reason_request: true,
    delete_many_hold_reasons_request: true,
    list_hold_reason_versions_request: true,
    get_hold_reason_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'inbox.v1.hold_reasons_events.created',
    updated: 'inbox.v1.hold_reasons_events.updated',
    deleted: 'inbox.v1.hold_reasons_events.deleted',
} as const;
