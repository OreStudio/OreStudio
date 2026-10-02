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
import type { CapFloorInstrument } from '../domain/cap_floor_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CapFloorInstrumentKey {
    trade_id: string;
}

export interface CapFloorInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    start_date: string;
    maturity_date: string;
    description: string;
}

export interface CapFloorInstrumentChange {
    write: CapFloorInstrumentWrite;
    precondition: Precondition;
}

export interface CapFloorInstrumentRemoval {
    key: CapFloorInstrumentKey;
    precondition: Precondition;
}

export interface CapFloorInstrumentLookup {
    key: CapFloorInstrumentKey;
    cap_floor_instrument: CapFloorInstrument | null;
}

export interface CapFloorInstrumentEvent {
    event_id: string;
    key: CapFloorInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CapFloorInstrumentVersionKey {
    cap_floor_instrument: CapFloorInstrumentKey;
    version: number;
}

export interface CapFloorInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCapFloorInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListCapFloorInstrumentsResponse {
    result: Result;
    cap_floor_instruments: CapFloorInstrument[];
    total: number;
}

export interface GetCapFloorInstrumentRequest {
    key: CapFloorInstrumentKey;
}

export interface GetCapFloorInstrumentResponse {
    result: Result;
    cap_floor_instrument: CapFloorInstrument | null;
}

export interface GetManyCapFloorInstrumentsRequest {
    keys: CapFloorInstrumentKey[];
}

export interface GetManyCapFloorInstrumentsResponse {
    result: Result;
    entries: CapFloorInstrumentLookup[];
}

export interface PutCapFloorInstrumentRequest {
    change: CapFloorInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCapFloorInstrumentResponse {
    result: Result;
    cap_floor_instrument: CapFloorInstrument | null;
}

export interface PutManyCapFloorInstrumentsRequest {
    changes: CapFloorInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCapFloorInstrumentsResponse {
    result: Result;
    cap_floor_instruments: CapFloorInstrument[];
}

export interface DeleteCapFloorInstrumentRequest {
    removal: CapFloorInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCapFloorInstrumentResponse {
    result: Result;
}

export interface DeleteManyCapFloorInstrumentsRequest {
    removals: CapFloorInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCapFloorInstrumentsResponse {
    result: Result;
}

export interface ListCapFloorInstrumentVersionsRequest {
    key: CapFloorInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CapFloorInstrumentVersionsFilter | null;
}

export interface ListCapFloorInstrumentVersionsResponse {
    result: Result;
    versions: CapFloorInstrument[];
    total: number;
}

export interface GetCapFloorInstrumentVersionRequest {
    key: CapFloorInstrumentVersionKey;
}

export interface GetCapFloorInstrumentVersionResponse {
    result: Result;
    version: CapFloorInstrument | null;
}

export const subjects = {
    list_cap_floor_instruments_request: 'trading.v1.cap_floor_instruments.list',
    get_cap_floor_instrument_request: 'trading.v1.cap_floor_instruments.get',
    get_many_cap_floor_instruments_request: 'trading.v1.cap_floor_instruments.get_many',
    put_cap_floor_instrument_request: 'trading.v1.cap_floor_instruments.put',
    put_many_cap_floor_instruments_request: 'trading.v1.cap_floor_instruments.put_many',
    delete_cap_floor_instrument_request: 'trading.v1.cap_floor_instruments.delete',
    delete_many_cap_floor_instruments_request: 'trading.v1.cap_floor_instruments.delete_many',
    list_cap_floor_instrument_versions_request: 'trading.v1.cap_floor_instruments_versions.list',
    get_cap_floor_instrument_version_request: 'trading.v1.cap_floor_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cap_floor_instruments_request: true,
    get_cap_floor_instrument_request: true,
    get_many_cap_floor_instruments_request: true,
    put_cap_floor_instrument_request: true,
    put_many_cap_floor_instruments_request: true,
    delete_cap_floor_instrument_request: true,
    delete_many_cap_floor_instruments_request: true,
    list_cap_floor_instrument_versions_request: true,
    get_cap_floor_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.cap_floor_instruments_events.created',
    updated: 'trading.v1.cap_floor_instruments_events.updated',
    deleted: 'trading.v1.cap_floor_instruments_events.deleted',
} as const;
