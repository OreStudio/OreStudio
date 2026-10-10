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
import type { FlexiSwapInstrument } from '../domain/flexi_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface FlexiSwapInstrumentKey {
    trade_id: string;
}

export interface FlexiSwapInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    option_long_short: string;
}

export interface FlexiSwapInstrumentChange {
    write: FlexiSwapInstrumentWrite;
    precondition: Precondition;
}

export interface FlexiSwapInstrumentRemoval {
    key: FlexiSwapInstrumentKey;
    precondition: Precondition;
}

export interface FlexiSwapInstrumentLookup {
    key: FlexiSwapInstrumentKey;
    flexi_swap_instrument: FlexiSwapInstrument | null;
}

export interface FlexiSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface FlexiSwapInstrumentEvent {
    event_id: string;
    key: FlexiSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface FlexiSwapInstrumentVersionKey {
    flexi_swap_instrument: FlexiSwapInstrumentKey;
    version: number;
}

export interface FlexiSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListFlexiSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: FlexiSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListFlexiSwapInstrumentsResponse {
    result: Result;
    flexi_swap_instruments: FlexiSwapInstrument[];
    total: number;
}

export interface GetFlexiSwapInstrumentRequest {
    key: FlexiSwapInstrumentKey;
}

export interface GetFlexiSwapInstrumentResponse {
    result: Result;
    flexi_swap_instrument: FlexiSwapInstrument | null;
}

export interface GetManyFlexiSwapInstrumentsRequest {
    keys: FlexiSwapInstrumentKey[];
}

export interface GetManyFlexiSwapInstrumentsResponse {
    result: Result;
    entries: FlexiSwapInstrumentLookup[];
}

export interface PutFlexiSwapInstrumentRequest {
    change: FlexiSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutFlexiSwapInstrumentResponse {
    result: Result;
    flexi_swap_instrument: FlexiSwapInstrument | null;
}

export interface PutManyFlexiSwapInstrumentsRequest {
    changes: FlexiSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyFlexiSwapInstrumentsResponse {
    result: Result;
    flexi_swap_instruments: FlexiSwapInstrument[];
}

export interface DeleteFlexiSwapInstrumentRequest {
    removal: FlexiSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteFlexiSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyFlexiSwapInstrumentsRequest {
    removals: FlexiSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyFlexiSwapInstrumentsResponse {
    result: Result;
}

export interface ListFlexiSwapInstrumentVersionsRequest {
    key: FlexiSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: FlexiSwapInstrumentVersionsFilter | null;
}

export interface ListFlexiSwapInstrumentVersionsResponse {
    result: Result;
    versions: FlexiSwapInstrument[];
    total: number;
}

export interface GetFlexiSwapInstrumentVersionRequest {
    key: FlexiSwapInstrumentVersionKey;
}

export interface GetFlexiSwapInstrumentVersionResponse {
    result: Result;
    version: FlexiSwapInstrument | null;
}

export const subjects = {
    list_flexi_swap_instruments_request: 'trading.v1.flexi_swap_instruments.list',
    get_flexi_swap_instrument_request: 'trading.v1.flexi_swap_instruments.get',
    get_many_flexi_swap_instruments_request: 'trading.v1.flexi_swap_instruments.get_many',
    put_flexi_swap_instrument_request: 'trading.v1.flexi_swap_instruments.put',
    put_many_flexi_swap_instruments_request: 'trading.v1.flexi_swap_instruments.put_many',
    delete_flexi_swap_instrument_request: 'trading.v1.flexi_swap_instruments.delete',
    delete_many_flexi_swap_instruments_request: 'trading.v1.flexi_swap_instruments.delete_many',
    list_flexi_swap_instrument_versions_request: 'trading.v1.flexi_swap_instruments_versions.list',
    get_flexi_swap_instrument_version_request: 'trading.v1.flexi_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_flexi_swap_instruments_request: true,
    get_flexi_swap_instrument_request: true,
    get_many_flexi_swap_instruments_request: true,
    put_flexi_swap_instrument_request: true,
    put_many_flexi_swap_instruments_request: true,
    delete_flexi_swap_instrument_request: true,
    delete_many_flexi_swap_instruments_request: true,
    list_flexi_swap_instrument_versions_request: true,
    get_flexi_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.flexi_swap_instruments_events.created',
    updated: 'trading.v1.flexi_swap_instruments_events.updated',
    deleted: 'trading.v1.flexi_swap_instruments_events.deleted',
} as const;
