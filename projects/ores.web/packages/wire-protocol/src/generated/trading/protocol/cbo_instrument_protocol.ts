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
import type { CboInstrument } from '../domain/cbo_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CboInstrumentKey {
    trade_id: string;
}

export interface CboInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    tranche_name: string;
    notional: string;
    structure_id: string;
}

export interface CboInstrumentChange {
    write: CboInstrumentWrite;
    precondition: Precondition;
}

export interface CboInstrumentRemoval {
    key: CboInstrumentKey;
    precondition: Precondition;
}

export interface CboInstrumentLookup {
    key: CboInstrumentKey;
    cbo_instrument: CboInstrument | null;
}

export interface CboInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface CboInstrumentEvent {
    event_id: string;
    key: CboInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CboInstrumentVersionKey {
    cbo_instrument: CboInstrumentKey;
    version: number;
}

export interface CboInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCboInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CboInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListCboInstrumentsResponse {
    result: Result;
    cbo_instruments: CboInstrument[];
    total: number;
}

export interface GetCboInstrumentRequest {
    key: CboInstrumentKey;
}

export interface GetCboInstrumentResponse {
    result: Result;
    cbo_instrument: CboInstrument | null;
}

export interface GetManyCboInstrumentsRequest {
    keys: CboInstrumentKey[];
}

export interface GetManyCboInstrumentsResponse {
    result: Result;
    entries: CboInstrumentLookup[];
}

export interface PutCboInstrumentRequest {
    change: CboInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCboInstrumentResponse {
    result: Result;
    cbo_instrument: CboInstrument | null;
}

export interface PutManyCboInstrumentsRequest {
    changes: CboInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCboInstrumentsResponse {
    result: Result;
    cbo_instruments: CboInstrument[];
}

export interface DeleteCboInstrumentRequest {
    removal: CboInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCboInstrumentResponse {
    result: Result;
}

export interface DeleteManyCboInstrumentsRequest {
    removals: CboInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCboInstrumentsResponse {
    result: Result;
}

export interface ListCboInstrumentVersionsRequest {
    key: CboInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CboInstrumentVersionsFilter | null;
}

export interface ListCboInstrumentVersionsResponse {
    result: Result;
    versions: CboInstrument[];
    total: number;
}

export interface GetCboInstrumentVersionRequest {
    key: CboInstrumentVersionKey;
}

export interface GetCboInstrumentVersionResponse {
    result: Result;
    version: CboInstrument | null;
}

export const subjects = {
    list_cbo_instruments_request: 'trading.v1.cbo_instruments.list',
    get_cbo_instrument_request: 'trading.v1.cbo_instruments.get',
    get_many_cbo_instruments_request: 'trading.v1.cbo_instruments.get_many',
    put_cbo_instrument_request: 'trading.v1.cbo_instruments.put',
    put_many_cbo_instruments_request: 'trading.v1.cbo_instruments.put_many',
    delete_cbo_instrument_request: 'trading.v1.cbo_instruments.delete',
    delete_many_cbo_instruments_request: 'trading.v1.cbo_instruments.delete_many',
    list_cbo_instrument_versions_request: 'trading.v1.cbo_instruments_versions.list',
    get_cbo_instrument_version_request: 'trading.v1.cbo_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_cbo_instruments_request: true,
    get_cbo_instrument_request: true,
    get_many_cbo_instruments_request: true,
    put_cbo_instrument_request: true,
    put_many_cbo_instruments_request: true,
    delete_cbo_instrument_request: true,
    delete_many_cbo_instruments_request: true,
    list_cbo_instrument_versions_request: true,
    get_cbo_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.events.cbo_instruments.created',
    updated: 'trading.v1.events.cbo_instruments.updated',
    deleted: 'trading.v1.events.cbo_instruments.deleted',
} as const;
