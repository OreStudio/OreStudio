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
import type { Ascot } from '../domain/ascot.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface AscotKey {
    trade_id: string;
}

export interface AscotWrite {
    trade_id: string;
    ascot_option_type: string;
}

export interface AscotChange {
    write: AscotWrite;
    precondition: Precondition;
}

export interface AscotRemoval {
    key: AscotKey;
    precondition: Precondition;
}

export interface AscotLookup {
    key: AscotKey;
    ascot: Ascot | null;
}

export interface AscotsFilter {
    trade_id_one_of: string[] | null;
}

export interface AscotEvent {
    event_id: string;
    key: AscotKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface AscotVersionKey {
    ascot: AscotKey;
    version: number;
}

export interface AscotVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListAscotsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: AscotsFilter | null;
    as_of: string | null;
}

export interface ListAscotsResponse {
    result: Result;
    ascots: Ascot[];
    total: number;
}

export interface GetAscotRequest {
    key: AscotKey;
}

export interface GetAscotResponse {
    result: Result;
    ascot: Ascot | null;
}

export interface GetManyAscotsRequest {
    keys: AscotKey[];
}

export interface GetManyAscotsResponse {
    result: Result;
    entries: AscotLookup[];
}

export interface PutAscotRequest {
    change: AscotChange;
    intent: ChangeIntent;
}

export interface PutAscotResponse {
    result: Result;
    ascot: Ascot | null;
}

export interface PutManyAscotsRequest {
    changes: AscotChange[];
    intent: ChangeIntent;
}

export interface PutManyAscotsResponse {
    result: Result;
    ascots: Ascot[];
}

export interface DeleteAscotRequest {
    removal: AscotRemoval;
    intent: ChangeIntent;
}

export interface DeleteAscotResponse {
    result: Result;
}

export interface DeleteManyAscotsRequest {
    removals: AscotRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyAscotsResponse {
    result: Result;
}

export interface ListAscotVersionsRequest {
    key: AscotKey;
    offset: number;
    limit: number;
    order: Order;
    filter: AscotVersionsFilter | null;
}

export interface ListAscotVersionsResponse {
    result: Result;
    versions: Ascot[];
    total: number;
}

export interface GetAscotVersionRequest {
    key: AscotVersionKey;
}

export interface GetAscotVersionResponse {
    result: Result;
    version: Ascot | null;
}

export const subjects = {
    list_ascots_request: 'trading.v1.ascots.list',
    get_ascot_request: 'trading.v1.ascots.get',
    get_many_ascots_request: 'trading.v1.ascots.get_many',
    put_ascot_request: 'trading.v1.ascots.put',
    put_many_ascots_request: 'trading.v1.ascots.put_many',
    delete_ascot_request: 'trading.v1.ascots.delete',
    delete_many_ascots_request: 'trading.v1.ascots.delete_many',
    list_ascot_versions_request: 'trading.v1.ascots_versions.list',
    get_ascot_version_request: 'trading.v1.ascots_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_ascots_request: true,
    get_ascot_request: true,
    get_many_ascots_request: true,
    put_ascot_request: true,
    put_many_ascots_request: true,
    delete_ascot_request: true,
    delete_many_ascots_request: true,
    list_ascot_versions_request: true,
    get_ascot_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.ascots_events.created',
    updated: 'trading.v1.ascots_events.updated',
    deleted: 'trading.v1.ascots_events.deleted',
} as const;
