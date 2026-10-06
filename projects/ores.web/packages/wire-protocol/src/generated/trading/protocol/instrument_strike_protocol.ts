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
import type { InstrumentStrike } from '../domain/instrument_strike.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface InstrumentStrikeKey {
    trade_id: string;
}

export interface InstrumentStrikeWrite {
    trade_id: string;
    trade_activity_id: string;
    price_value: string | null;
    price_currency: string | null;
    yield_value: number | null;
    yield_compounding: string | null;
    bare_value: string | null;
    bare_currency: string | null;
}

export interface InstrumentStrikeChange {
    write: InstrumentStrikeWrite;
    precondition: Precondition;
}

export interface InstrumentStrikeRemoval {
    key: InstrumentStrikeKey;
    precondition: Precondition;
}

export interface InstrumentStrikeLookup {
    key: InstrumentStrikeKey;
    instrument_strike: InstrumentStrike | null;
}

export interface InstrumentStrikesFilter {
    trade_id_one_of: string[] | null;
}

export interface InstrumentStrikeEvent {
    event_id: string;
    key: InstrumentStrikeKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface InstrumentStrikeVersionKey {
    instrument_strike: InstrumentStrikeKey;
    version: number;
}

export interface InstrumentStrikeVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListInstrumentStrikesRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentStrikesFilter | null;
    as_of: string | null;
}

export interface ListInstrumentStrikesResponse {
    result: Result;
    instrument_strikes: InstrumentStrike[];
    total: number;
}

export interface GetInstrumentStrikeRequest {
    key: InstrumentStrikeKey;
}

export interface GetInstrumentStrikeResponse {
    result: Result;
    instrument_strike: InstrumentStrike | null;
}

export interface GetManyInstrumentStrikesRequest {
    keys: InstrumentStrikeKey[];
}

export interface GetManyInstrumentStrikesResponse {
    result: Result;
    entries: InstrumentStrikeLookup[];
}

export interface PutInstrumentStrikeRequest {
    change: InstrumentStrikeChange;
    intent: ChangeIntent;
}

export interface PutInstrumentStrikeResponse {
    result: Result;
    instrument_strike: InstrumentStrike | null;
}

export interface PutManyInstrumentStrikesRequest {
    changes: InstrumentStrikeChange[];
    intent: ChangeIntent;
}

export interface PutManyInstrumentStrikesResponse {
    result: Result;
    instrument_strikes: InstrumentStrike[];
}

export interface DeleteInstrumentStrikeRequest {
    removal: InstrumentStrikeRemoval;
    intent: ChangeIntent;
}

export interface DeleteInstrumentStrikeResponse {
    result: Result;
}

export interface DeleteManyInstrumentStrikesRequest {
    removals: InstrumentStrikeRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyInstrumentStrikesResponse {
    result: Result;
}

export interface ListInstrumentStrikeVersionsRequest {
    key: InstrumentStrikeKey;
    offset: number;
    limit: number;
    order: Order;
    filter: InstrumentStrikeVersionsFilter | null;
}

export interface ListInstrumentStrikeVersionsResponse {
    result: Result;
    versions: InstrumentStrike[];
    total: number;
}

export interface GetInstrumentStrikeVersionRequest {
    key: InstrumentStrikeVersionKey;
}

export interface GetInstrumentStrikeVersionResponse {
    result: Result;
    version: InstrumentStrike | null;
}

export const subjects = {
    list_instrument_strikes_request: 'trading.v1.instrument_strikes.list',
    get_instrument_strike_request: 'trading.v1.instrument_strikes.get',
    get_many_instrument_strikes_request: 'trading.v1.instrument_strikes.get_many',
    put_instrument_strike_request: 'trading.v1.instrument_strikes.put',
    put_many_instrument_strikes_request: 'trading.v1.instrument_strikes.put_many',
    delete_instrument_strike_request: 'trading.v1.instrument_strikes.delete',
    delete_many_instrument_strikes_request: 'trading.v1.instrument_strikes.delete_many',
    list_instrument_strike_versions_request: 'trading.v1.instrument_strikes_versions.list',
    get_instrument_strike_version_request: 'trading.v1.instrument_strikes_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_instrument_strikes_request: true,
    get_instrument_strike_request: true,
    get_many_instrument_strikes_request: true,
    put_instrument_strike_request: true,
    put_many_instrument_strikes_request: true,
    delete_instrument_strike_request: true,
    delete_many_instrument_strikes_request: true,
    list_instrument_strike_versions_request: true,
    get_instrument_strike_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.instrument_strikes_events.created',
    updated: 'trading.v1.instrument_strikes_events.updated',
    deleted: 'trading.v1.instrument_strikes_events.deleted',
} as const;
