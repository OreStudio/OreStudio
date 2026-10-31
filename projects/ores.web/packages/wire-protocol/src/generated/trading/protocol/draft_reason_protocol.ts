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
import type { DraftReason } from '../domain/draft_reason.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface DraftReasonKey {
    code: string;
}

export interface DraftReasonWrite {
    code: string;
    description: string;
    responsible_role: string;
}

export interface DraftReasonChange {
    write: DraftReasonWrite;
    precondition: Precondition;
}

export interface DraftReasonRemoval {
    key: DraftReasonKey;
    precondition: Precondition;
}

export interface DraftReasonLookup {
    key: DraftReasonKey;
    draft_reason: DraftReason | null;
}

export interface DraftReasonsFilter {
    code_one_of: string[] | null;
}

export interface DraftReasonEvent {
    event_id: string;
    key: DraftReasonKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface DraftReasonVersionKey {
    draft_reason: DraftReasonKey;
    version: number;
}

export interface DraftReasonVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListDraftReasonsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: DraftReasonsFilter | null;
    as_of: string | null;
}

export interface ListDraftReasonsResponse {
    result: Result;
    draft_reasons: DraftReason[];
    total: number;
}

export interface GetDraftReasonRequest {
    key: DraftReasonKey;
}

export interface GetDraftReasonResponse {
    result: Result;
    draft_reason: DraftReason | null;
}

export interface GetManyDraftReasonsRequest {
    keys: DraftReasonKey[];
}

export interface GetManyDraftReasonsResponse {
    result: Result;
    entries: DraftReasonLookup[];
}

export interface PutDraftReasonRequest {
    change: DraftReasonChange;
    intent: ChangeIntent;
}

export interface PutDraftReasonResponse {
    result: Result;
    draft_reason: DraftReason | null;
}

export interface PutManyDraftReasonsRequest {
    changes: DraftReasonChange[];
    intent: ChangeIntent;
}

export interface PutManyDraftReasonsResponse {
    result: Result;
    draft_reasons: DraftReason[];
}

export interface DeleteDraftReasonRequest {
    removal: DraftReasonRemoval;
    intent: ChangeIntent;
}

export interface DeleteDraftReasonResponse {
    result: Result;
}

export interface DeleteManyDraftReasonsRequest {
    removals: DraftReasonRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyDraftReasonsResponse {
    result: Result;
}

export interface ListDraftReasonVersionsRequest {
    key: DraftReasonKey;
    offset: number;
    limit: number;
    order: Order;
    filter: DraftReasonVersionsFilter | null;
}

export interface ListDraftReasonVersionsResponse {
    result: Result;
    versions: DraftReason[];
    total: number;
}

export interface GetDraftReasonVersionRequest {
    key: DraftReasonVersionKey;
}

export interface GetDraftReasonVersionResponse {
    result: Result;
    version: DraftReason | null;
}

export const subjects = {
    list_draft_reasons_request: 'trading.v1.draft_reasons.list',
    get_draft_reason_request: 'trading.v1.draft_reasons.get',
    get_many_draft_reasons_request: 'trading.v1.draft_reasons.get_many',
    put_draft_reason_request: 'trading.v1.draft_reasons.put',
    put_many_draft_reasons_request: 'trading.v1.draft_reasons.put_many',
    delete_draft_reason_request: 'trading.v1.draft_reasons.delete',
    delete_many_draft_reasons_request: 'trading.v1.draft_reasons.delete_many',
    list_draft_reason_versions_request: 'trading.v1.draft_reasons_versions.list',
    get_draft_reason_version_request: 'trading.v1.draft_reasons_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_draft_reasons_request: true,
    get_draft_reason_request: true,
    get_many_draft_reasons_request: true,
    put_draft_reason_request: true,
    put_many_draft_reasons_request: true,
    delete_draft_reason_request: true,
    delete_many_draft_reasons_request: true,
    list_draft_reason_versions_request: true,
    get_draft_reason_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.draft_reasons_events.created',
    updated: 'trading.v1.draft_reasons_events.updated',
    deleted: 'trading.v1.draft_reasons_events.deleted',
} as const;
