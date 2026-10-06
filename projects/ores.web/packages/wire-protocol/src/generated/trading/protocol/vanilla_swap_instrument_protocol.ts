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
import type { VanillaSwapInstrument } from '../domain/vanilla_swap_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface VanillaSwapInstrumentKey {
    trade_id: string;
}

export interface VanillaSwapInstrumentWrite {
    trade_id: string;
    trade_type_code: string;
    trade_activity_id: string;
    start_date: string;
    maturity_date: string;
    settlement_lag: number | null;
    netting_set_id: string;
    description: string;
}

export interface VanillaSwapInstrumentChange {
    write: VanillaSwapInstrumentWrite;
    precondition: Precondition;
}

export interface VanillaSwapInstrumentRemoval {
    key: VanillaSwapInstrumentKey;
    precondition: Precondition;
}

export interface VanillaSwapInstrumentLookup {
    key: VanillaSwapInstrumentKey;
    vanilla_swap_instrument: VanillaSwapInstrument | null;
}

export interface VanillaSwapInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface VanillaSwapInstrumentEvent {
    event_id: string;
    key: VanillaSwapInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface VanillaSwapInstrumentVersionKey {
    vanilla_swap_instrument: VanillaSwapInstrumentKey;
    version: number;
}

export interface VanillaSwapInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListVanillaSwapInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: VanillaSwapInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListVanillaSwapInstrumentsResponse {
    result: Result;
    vanilla_swap_instruments: VanillaSwapInstrument[];
    total: number;
}

export interface GetVanillaSwapInstrumentRequest {
    key: VanillaSwapInstrumentKey;
}

export interface GetVanillaSwapInstrumentResponse {
    result: Result;
    vanilla_swap_instrument: VanillaSwapInstrument | null;
}

export interface GetManyVanillaSwapInstrumentsRequest {
    keys: VanillaSwapInstrumentKey[];
}

export interface GetManyVanillaSwapInstrumentsResponse {
    result: Result;
    entries: VanillaSwapInstrumentLookup[];
}

export interface PutVanillaSwapInstrumentRequest {
    change: VanillaSwapInstrumentChange;
    intent: ChangeIntent;
}

export interface PutVanillaSwapInstrumentResponse {
    result: Result;
    vanilla_swap_instrument: VanillaSwapInstrument | null;
}

export interface PutManyVanillaSwapInstrumentsRequest {
    changes: VanillaSwapInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManyVanillaSwapInstrumentsResponse {
    result: Result;
    vanilla_swap_instruments: VanillaSwapInstrument[];
}

export interface DeleteVanillaSwapInstrumentRequest {
    removal: VanillaSwapInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteVanillaSwapInstrumentResponse {
    result: Result;
}

export interface DeleteManyVanillaSwapInstrumentsRequest {
    removals: VanillaSwapInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManyVanillaSwapInstrumentsResponse {
    result: Result;
}

export interface ListVanillaSwapInstrumentVersionsRequest {
    key: VanillaSwapInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: VanillaSwapInstrumentVersionsFilter | null;
}

export interface ListVanillaSwapInstrumentVersionsResponse {
    result: Result;
    versions: VanillaSwapInstrument[];
    total: number;
}

export interface GetVanillaSwapInstrumentVersionRequest {
    key: VanillaSwapInstrumentVersionKey;
}

export interface GetVanillaSwapInstrumentVersionResponse {
    result: Result;
    version: VanillaSwapInstrument | null;
}

export const subjects = {
    list_vanilla_swap_instruments_request: 'trading.v1.vanilla_swap_instruments.list',
    get_vanilla_swap_instrument_request: 'trading.v1.vanilla_swap_instruments.get',
    get_many_vanilla_swap_instruments_request: 'trading.v1.vanilla_swap_instruments.get_many',
    put_vanilla_swap_instrument_request: 'trading.v1.vanilla_swap_instruments.put',
    put_many_vanilla_swap_instruments_request: 'trading.v1.vanilla_swap_instruments.put_many',
    delete_vanilla_swap_instrument_request: 'trading.v1.vanilla_swap_instruments.delete',
    delete_many_vanilla_swap_instruments_request: 'trading.v1.vanilla_swap_instruments.delete_many',
    list_vanilla_swap_instrument_versions_request:
        'trading.v1.vanilla_swap_instruments_versions.list',
    get_vanilla_swap_instrument_version_request: 'trading.v1.vanilla_swap_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_vanilla_swap_instruments_request: true,
    get_vanilla_swap_instrument_request: true,
    get_many_vanilla_swap_instruments_request: true,
    put_vanilla_swap_instrument_request: true,
    put_many_vanilla_swap_instruments_request: true,
    delete_vanilla_swap_instrument_request: true,
    delete_many_vanilla_swap_instruments_request: true,
    list_vanilla_swap_instrument_versions_request: true,
    get_vanilla_swap_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.vanilla_swap_instruments_events.created',
    updated: 'trading.v1.vanilla_swap_instruments_events.updated',
    deleted: 'trading.v1.vanilla_swap_instruments_events.deleted',
} as const;
