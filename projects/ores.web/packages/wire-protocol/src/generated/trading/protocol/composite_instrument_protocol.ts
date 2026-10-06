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
import type { CompositeInstrument } from '../domain/composite_instrument.js';
import type { CompositeLeg } from '../domain/composite_leg.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface CompositeInstrumentKey {
    trade_id: string;
}

export interface CompositeInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    description: string;
}

export interface CompositeInstrumentChange {
    write: CompositeInstrumentWrite;
    precondition: Precondition;
}

export interface CompositeInstrumentRemoval {
    key: CompositeInstrumentKey;
    precondition: Precondition;
}

export interface CompositeInstrumentLookup {
    key: CompositeInstrumentKey;
    composite_instrument: CompositeInstrument | null;
}

export interface CompositeInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface CompositeInstrumentEvent {
    event_id: string;
    key: CompositeInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface CompositeInstrumentVersionKey {
    composite_instrument: CompositeInstrumentKey;
    version: number;
}

export interface CompositeInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListCompositeInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: CompositeInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListCompositeInstrumentsResponse {
    result: Result;
    composite_instruments: CompositeInstrument[];
    total: number;
}

export interface GetCompositeInstrumentRequest {
    key: CompositeInstrumentKey;
}

export interface GetCompositeInstrumentResponse {
    result: Result;
    composite_instrument: CompositeInstrument | null;
}

export interface GetManyCompositeInstrumentsRequest {
    keys: CompositeInstrumentKey[];
}

export interface GetManyCompositeInstrumentsResponse {
    result: Result;
    entries: CompositeInstrumentLookup[];
}

export interface PutCompositeInstrumentRequest {
    change: CompositeInstrumentChange;
    intent: ChangeIntent;
}

export interface PutCompositeInstrumentResponse {
    result: Result;
    composite_instrument: CompositeInstrument | null;
}

export interface PutManyCompositeInstrumentsRequest {
    changes: CompositeInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyCompositeInstrumentsResponse {
    result: Result;
    composite_instruments: CompositeInstrument[];
}

export interface DeleteCompositeInstrumentRequest {
    removal: CompositeInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteCompositeInstrumentResponse {
    result: Result;
}

export interface DeleteManyCompositeInstrumentsRequest {
    removals: CompositeInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyCompositeInstrumentsResponse {
    result: Result;
}

export interface ListCompositeInstrumentVersionsRequest {
    key: CompositeInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: CompositeInstrumentVersionsFilter | null;
}

export interface ListCompositeInstrumentVersionsResponse {
    result: Result;
    versions: CompositeInstrument[];
    total: number;
}

export interface GetCompositeInstrumentVersionRequest {
    key: CompositeInstrumentVersionKey;
}

export interface GetCompositeInstrumentVersionResponse {
    result: Result;
    version: CompositeInstrument | null;
}

/**
 * @brief Writes a composite instrument and replaces its whole leg set.
 *
 * One request rather than two, because the store refuses a leg whose
 * parent is not current: the instrument row and its legs have to reach
 * one handler in order. The leg set the request states replaces whatever
 * the instrument carried before.
 */
export interface PutCompositeInstrumentWithLegsRequest {
    instrument: CompositeInstrument;
    legs: CompositeLeg[];
}

export interface PutCompositeInstrumentWithLegsResponse {
    result: Result;
    instrument: CompositeInstrument;
}

export interface GetCompositeInstrumentLegsRequest {
    trade_id: string;
}

export interface GetCompositeInstrumentLegsResponse {
    result: Result;
    legs: CompositeLeg[];
}

export const subjects = {
    list_composite_instruments_request: 'trading.v1.composite_instruments.list',
    get_composite_instrument_request: 'trading.v1.composite_instruments.get',
    get_many_composite_instruments_request: 'trading.v1.composite_instruments.get_many',
    put_composite_instrument_request: 'trading.v1.composite_instruments.put',
    put_many_composite_instruments_request: 'trading.v1.composite_instruments.put_many',
    delete_composite_instrument_request: 'trading.v1.composite_instruments.delete',
    delete_many_composite_instruments_request: 'trading.v1.composite_instruments.delete_many',
    list_composite_instrument_versions_request: 'trading.v1.composite_instruments_versions.list',
    get_composite_instrument_version_request: 'trading.v1.composite_instruments_versions.get',
    put_composite_instrument_with_legs_request: 'trading.v1.composite_instruments.put_with_legs',
    get_composite_instrument_legs_request: 'trading.v1.composite_instruments.legs',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_composite_instruments_request: true,
    get_composite_instrument_request: true,
    get_many_composite_instruments_request: true,
    put_composite_instrument_request: true,
    put_many_composite_instruments_request: true,
    delete_composite_instrument_request: true,
    delete_many_composite_instruments_request: true,
    list_composite_instrument_versions_request: true,
    get_composite_instrument_version_request: true,
    put_composite_instrument_with_legs_request: true,
    get_composite_instrument_legs_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.composite_instruments_events.created',
    updated: 'trading.v1.composite_instruments_events.updated',
    deleted: 'trading.v1.composite_instruments_events.deleted',
} as const;
