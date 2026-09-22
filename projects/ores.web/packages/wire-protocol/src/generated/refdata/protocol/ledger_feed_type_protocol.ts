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
import type { LedgerFeedType } from '../domain/ledger_feed_type.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface LedgerFeedTypeKey {
    code: string;
}

export interface LedgerFeedTypeWrite {
    code: string;
    name: string;
    description: string;
    display_order: number;
}

export interface LedgerFeedTypeChange {
    write: LedgerFeedTypeWrite;
    precondition: Precondition;
}

export interface LedgerFeedTypeRemoval {
    key: LedgerFeedTypeKey;
    precondition: Precondition;
}

export interface LedgerFeedTypeLookup {
    key: LedgerFeedTypeKey;
    ledger_feed_type: LedgerFeedType | null;
}

export interface LedgerFeedTypeEvent {
    event_id: string;
    key: LedgerFeedTypeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface LedgerFeedTypeVersionKey {
    ledger_feed_type: LedgerFeedTypeKey;
    version: number;
}

export interface LedgerFeedTypeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListLedgerFeedTypesRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListLedgerFeedTypesResponse {
    result: Result;
    types: LedgerFeedType[];
    total: number;
}

export interface GetLedgerFeedTypeRequest {
    key: LedgerFeedTypeKey;
}

export interface GetLedgerFeedTypeResponse {
    result: Result;
    ledger_feed_type: LedgerFeedType | null;
}

export interface GetManyLedgerFeedTypesRequest {
    keys: LedgerFeedTypeKey[];
}

export interface GetManyLedgerFeedTypesResponse {
    result: Result;
    entries: LedgerFeedTypeLookup[];
}

export interface PutLedgerFeedTypeRequest {
    change: LedgerFeedTypeChange;
    intent: ChangeIntent;
}

export interface PutLedgerFeedTypeResponse {
    result: Result;
    ledger_feed_type: LedgerFeedType;
}

export interface PutManyLedgerFeedTypesRequest {
    changes: LedgerFeedTypeChange[];
    intent: ChangeIntent;
}

export interface PutManyLedgerFeedTypesResponse {
    result: Result;
    types: LedgerFeedType[];
}

export interface DeleteLedgerFeedTypeRequest {
    removal: LedgerFeedTypeRemoval;
    intent: ChangeIntent;
}

export interface DeleteLedgerFeedTypeResponse {
    result: Result;
}

export interface DeleteManyLedgerFeedTypesRequest {
    removals: LedgerFeedTypeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyLedgerFeedTypesResponse {
    result: Result;
}

export interface ListLedgerFeedTypeVersionsRequest {
    key: LedgerFeedTypeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: LedgerFeedTypeVersionsFilter | null;
}

export interface ListLedgerFeedTypeVersionsResponse {
    result: Result;
    versions: LedgerFeedType[];
    total: number;
}

export interface GetLedgerFeedTypeVersionRequest {
    key: LedgerFeedTypeVersionKey;
}

export interface GetLedgerFeedTypeVersionResponse {
    result: Result;
    version: LedgerFeedType;
}

export const subjects = {
    list_ledger_feed_types_request: "refdata.v1.ledger_feed_types.list",
    get_ledger_feed_type_request: "refdata.v1.ledger_feed_types.get",
    get_many_ledger_feed_types_request: "refdata.v1.ledger_feed_types.get_many",
    put_ledger_feed_type_request: "refdata.v1.ledger_feed_types.put",
    put_many_ledger_feed_types_request: "refdata.v1.ledger_feed_types.put_many",
    delete_ledger_feed_type_request: "refdata.v1.ledger_feed_types.delete",
    delete_many_ledger_feed_types_request: "refdata.v1.ledger_feed_types.delete_many",
    list_ledger_feed_type_versions_request: "refdata.v1.ledger_feed_types_versions.list",
    get_ledger_feed_type_version_request: "refdata.v1.ledger_feed_types_versions.get",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ledger_feed_types_request: true,
    get_ledger_feed_type_request: true,
    get_many_ledger_feed_types_request: true,
    put_ledger_feed_type_request: true,
    put_many_ledger_feed_types_request: true,
    delete_ledger_feed_type_request: true,
    delete_many_ledger_feed_types_request: true,
    list_ledger_feed_type_versions_request: true,
    get_ledger_feed_type_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: "refdata.v1.ledger_feed_types_events.created",
    updated: "refdata.v1.ledger_feed_types_events.updated",
    deleted: "refdata.v1.ledger_feed_types_events.deleted",
} as const;
