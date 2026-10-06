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
import type { RpaInstrument } from '../domain/rpa_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface RpaInstrumentKey {
    trade_id: string;
}

export interface RpaInstrumentWrite {
    trade_id: string;
    start_date: string;
    maturity_date: string;
    reference_counterparty: string;
    participation_rate: number;
    protection_fee: number | null;
    description: string;
}

export interface RpaInstrumentChange {
    write: RpaInstrumentWrite;
    precondition: Precondition;
}

export interface RpaInstrumentRemoval {
    key: RpaInstrumentKey;
    precondition: Precondition;
}

export interface RpaInstrumentLookup {
    key: RpaInstrumentKey;
    rpa_instrument: RpaInstrument | null;
}

export interface RpaInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface RpaInstrumentEvent {
    event_id: string;
    key: RpaInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface RpaInstrumentVersionKey {
    rpa_instrument: RpaInstrumentKey;
    version: number;
}

export interface RpaInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListRpaInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: RpaInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListRpaInstrumentsResponse {
    result: Result;
    rpa_instruments: RpaInstrument[];
    total: number;
}

export interface GetRpaInstrumentRequest {
    key: RpaInstrumentKey;
}

export interface GetRpaInstrumentResponse {
    result: Result;
    rpa_instrument: RpaInstrument | null;
}

export interface GetManyRpaInstrumentsRequest {
    keys: RpaInstrumentKey[];
}

export interface GetManyRpaInstrumentsResponse {
    result: Result;
    entries: RpaInstrumentLookup[];
}

export interface PutRpaInstrumentRequest {
    change: RpaInstrumentChange;
    intent: ChangeIntent;
}

export interface PutRpaInstrumentResponse {
    result: Result;
    rpa_instrument: RpaInstrument | null;
}

export interface PutManyRpaInstrumentsRequest {
    changes: RpaInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyRpaInstrumentsResponse {
    result: Result;
    rpa_instruments: RpaInstrument[];
}

export interface DeleteRpaInstrumentRequest {
    removal: RpaInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteRpaInstrumentResponse {
    result: Result;
}

export interface DeleteManyRpaInstrumentsRequest {
    removals: RpaInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyRpaInstrumentsResponse {
    result: Result;
}

export interface ListRpaInstrumentVersionsRequest {
    key: RpaInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: RpaInstrumentVersionsFilter | null;
}

export interface ListRpaInstrumentVersionsResponse {
    result: Result;
    versions: RpaInstrument[];
    total: number;
}

export interface GetRpaInstrumentVersionRequest {
    key: RpaInstrumentVersionKey;
}

export interface GetRpaInstrumentVersionResponse {
    result: Result;
    version: RpaInstrument | null;
}

export const subjects = {
    list_rpa_instruments_request: 'trading.v1.rpa_instruments.list',
    get_rpa_instrument_request: 'trading.v1.rpa_instruments.get',
    get_many_rpa_instruments_request: 'trading.v1.rpa_instruments.get_many',
    put_rpa_instrument_request: 'trading.v1.rpa_instruments.put',
    put_many_rpa_instruments_request: 'trading.v1.rpa_instruments.put_many',
    delete_rpa_instrument_request: 'trading.v1.rpa_instruments.delete',
    delete_many_rpa_instruments_request: 'trading.v1.rpa_instruments.delete_many',
    list_rpa_instrument_versions_request: 'trading.v1.rpa_instruments_versions.list',
    get_rpa_instrument_version_request: 'trading.v1.rpa_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_rpa_instruments_request: true,
    get_rpa_instrument_request: true,
    get_many_rpa_instruments_request: true,
    put_rpa_instrument_request: true,
    put_many_rpa_instruments_request: true,
    delete_rpa_instrument_request: true,
    delete_many_rpa_instruments_request: true,
    list_rpa_instrument_versions_request: true,
    get_rpa_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.rpa_instruments_events.created',
    updated: 'trading.v1.rpa_instruments_events.updated',
    deleted: 'trading.v1.rpa_instruments_events.deleted',
} as const;
