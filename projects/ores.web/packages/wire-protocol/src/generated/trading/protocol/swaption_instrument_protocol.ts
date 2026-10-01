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
import type { SwaptionInstrument } from '../domain/swaption_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SwaptionInstrumentKey {
    trade_id: string;
}

export interface SwaptionInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    expiry_date: string;
    exercise_type: string;
    settlement_type: string;
    long_short: string;
    start_date: string | null;
    maturity_date: string | null;
    description: string;
}

export interface SwaptionInstrumentChange {
    write: SwaptionInstrumentWrite;
    precondition: Precondition;
}

export interface SwaptionInstrumentRemoval {
    key: SwaptionInstrumentKey;
    precondition: Precondition;
}

export interface SwaptionInstrumentLookup {
    key: SwaptionInstrumentKey;
    swaption_instrument: SwaptionInstrument | null;
}

export interface SwaptionInstrumentEvent {
    event_id: string;
    key: SwaptionInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SwaptionInstrumentVersionKey {
    swaption_instrument: SwaptionInstrumentKey;
    version: number;
}

export interface SwaptionInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSwaptionInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
}

export interface ListSwaptionInstrumentsResponse {
    result: Result;
    swaption_instruments: SwaptionInstrument[];
    total: number;
}

export interface GetSwaptionInstrumentRequest {
    key: SwaptionInstrumentKey;
}

export interface GetSwaptionInstrumentResponse {
    result: Result;
    swaption_instrument: SwaptionInstrument | null;
}

export interface GetManySwaptionInstrumentsRequest {
    keys: SwaptionInstrumentKey[];
}

export interface GetManySwaptionInstrumentsResponse {
    result: Result;
    entries: SwaptionInstrumentLookup[];
}

export interface PutSwaptionInstrumentRequest {
    change: SwaptionInstrumentChange;
    intent: ChangeIntent;
}

export interface PutSwaptionInstrumentResponse {
    result: Result;
    swaption_instrument: SwaptionInstrument | null;
}

export interface PutManySwaptionInstrumentsRequest {
    changes: SwaptionInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManySwaptionInstrumentsResponse {
    result: Result;
    swaption_instruments: SwaptionInstrument[];
}

export interface DeleteSwaptionInstrumentRequest {
    removal: SwaptionInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteSwaptionInstrumentResponse {
    result: Result;
}

export interface DeleteManySwaptionInstrumentsRequest {
    removals: SwaptionInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySwaptionInstrumentsResponse {
    result: Result;
}

export interface ListSwaptionInstrumentVersionsRequest {
    key: SwaptionInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SwaptionInstrumentVersionsFilter | null;
}

export interface ListSwaptionInstrumentVersionsResponse {
    result: Result;
    versions: SwaptionInstrument[];
    total: number;
}

export interface GetSwaptionInstrumentVersionRequest {
    key: SwaptionInstrumentVersionKey;
}

export interface GetSwaptionInstrumentVersionResponse {
    result: Result;
    version: SwaptionInstrument | null;
}

export const subjects = {
    list_swaption_instruments_request: 'trading.v1.swaption_instruments.list',
    get_swaption_instrument_request: 'trading.v1.swaption_instruments.get',
    get_many_swaption_instruments_request: 'trading.v1.swaption_instruments.get_many',
    put_swaption_instrument_request: 'trading.v1.swaption_instruments.put',
    put_many_swaption_instruments_request: 'trading.v1.swaption_instruments.put_many',
    delete_swaption_instrument_request: 'trading.v1.swaption_instruments.delete',
    delete_many_swaption_instruments_request: 'trading.v1.swaption_instruments.delete_many',
    list_swaption_instrument_versions_request: 'trading.v1.swaption_instruments_versions.list',
    get_swaption_instrument_version_request: 'trading.v1.swaption_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_swaption_instruments_request: true,
    get_swaption_instrument_request: true,
    get_many_swaption_instruments_request: true,
    put_swaption_instrument_request: true,
    put_many_swaption_instruments_request: true,
    delete_swaption_instrument_request: true,
    delete_many_swaption_instruments_request: true,
    list_swaption_instrument_versions_request: true,
    get_swaption_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.swaption_instruments_events.created',
    updated: 'trading.v1.swaption_instruments_events.updated',
    deleted: 'trading.v1.swaption_instruments_events.deleted',
} as const;
