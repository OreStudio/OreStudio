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
import type { SyntheticCdoInstrument } from '../domain/synthetic_cdo_instrument.js';
import type { ChangeIntent } from '../../../utility/protocol.js';
import type { Order } from '../../../utility/protocol.js';
import type { Precondition } from '../../../utility/protocol.js';
import type { Result } from '../../../utility/protocol.js';

export interface SyntheticCdoInstrumentKey {
    trade_id: string;
}

export interface SyntheticCdoInstrumentWrite {
    trade_id: string;
    trade_activity_id: string;
    qualifier: string;
    protection_start_date: string;
    attachment_point: number;
    detachment_point: number;
    recovery_rate: number | null;
}

export interface SyntheticCdoInstrumentChange {
    write: SyntheticCdoInstrumentWrite;
    precondition: Precondition;
}

export interface SyntheticCdoInstrumentRemoval {
    key: SyntheticCdoInstrumentKey;
    precondition: Precondition;
}

export interface SyntheticCdoInstrumentLookup {
    key: SyntheticCdoInstrumentKey;
    synthetic_cdo_instrument: SyntheticCdoInstrument | null;
}

export interface SyntheticCdoInstrumentsFilter {
    trade_id_one_of: string[] | null;
}

export interface SyntheticCdoInstrumentEvent {
    event_id: string;
    key: SyntheticCdoInstrumentKey;
    action: string;
    version: number;
    occurred_at: string;
    correlation_id: string | null;
}

export interface SyntheticCdoInstrumentVersionKey {
    synthetic_cdo_instrument: SyntheticCdoInstrumentKey;
    version: number;
}

export interface SyntheticCdoInstrumentVersionsFilter {
    version: number | null;
    from_version: number | null;
    to_version: number | null;
}

export interface ListSyntheticCdoInstrumentsRequest {
    offset: number;
    limit: number;
    order: Order;
    filter: SyntheticCdoInstrumentsFilter | null;
    as_of: string | null;
}

export interface ListSyntheticCdoInstrumentsResponse {
    result: Result;
    synthetic_cdo_instruments: SyntheticCdoInstrument[];
    total: number;
}

export interface GetSyntheticCdoInstrumentRequest {
    key: SyntheticCdoInstrumentKey;
}

export interface GetSyntheticCdoInstrumentResponse {
    result: Result;
    synthetic_cdo_instrument: SyntheticCdoInstrument | null;
}

export interface GetManySyntheticCdoInstrumentsRequest {
    keys: SyntheticCdoInstrumentKey[];
}

export interface GetManySyntheticCdoInstrumentsResponse {
    result: Result;
    entries: SyntheticCdoInstrumentLookup[];
}

export interface PutSyntheticCdoInstrumentRequest {
    change: SyntheticCdoInstrumentChange;
    intent: ChangeIntent;
}

export interface PutSyntheticCdoInstrumentResponse {
    result: Result;
    synthetic_cdo_instrument: SyntheticCdoInstrument | null;
}

export interface PutManySyntheticCdoInstrumentsRequest {
    changes: SyntheticCdoInstrumentChange[];
    intent: ChangeIntent;
}

export interface PutManySyntheticCdoInstrumentsResponse {
    result: Result;
    synthetic_cdo_instruments: SyntheticCdoInstrument[];
}

export interface DeleteSyntheticCdoInstrumentRequest {
    removal: SyntheticCdoInstrumentRemoval;
    intent: ChangeIntent;
}

export interface DeleteSyntheticCdoInstrumentResponse {
    result: Result;
}

export interface DeleteManySyntheticCdoInstrumentsRequest {
    removals: SyntheticCdoInstrumentRemoval[];
    intent: ChangeIntent;
}

export interface DeleteManySyntheticCdoInstrumentsResponse {
    result: Result;
}

export interface ListSyntheticCdoInstrumentVersionsRequest {
    key: SyntheticCdoInstrumentKey;
    offset: number;
    limit: number;
    order: Order;
    filter: SyntheticCdoInstrumentVersionsFilter | null;
}

export interface ListSyntheticCdoInstrumentVersionsResponse {
    result: Result;
    versions: SyntheticCdoInstrument[];
    total: number;
}

export interface GetSyntheticCdoInstrumentVersionRequest {
    key: SyntheticCdoInstrumentVersionKey;
}

export interface GetSyntheticCdoInstrumentVersionResponse {
    result: Result;
    version: SyntheticCdoInstrument | null;
}

export const subjects = {
    list_synthetic_cdo_instruments_request: 'trading.v1.synthetic_cdo_instruments.list',
    get_synthetic_cdo_instrument_request: 'trading.v1.synthetic_cdo_instruments.get',
    get_many_synthetic_cdo_instruments_request: 'trading.v1.synthetic_cdo_instruments.get_many',
    put_synthetic_cdo_instrument_request: 'trading.v1.synthetic_cdo_instruments.put',
    put_many_synthetic_cdo_instruments_request: 'trading.v1.synthetic_cdo_instruments.put_many',
    delete_synthetic_cdo_instrument_request: 'trading.v1.synthetic_cdo_instruments.delete',
    delete_many_synthetic_cdo_instruments_request:
        'trading.v1.synthetic_cdo_instruments.delete_many',
    list_synthetic_cdo_instrument_versions_request:
        'trading.v1.synthetic_cdo_instruments_versions.list',
    get_synthetic_cdo_instrument_version_request:
        'trading.v1.synthetic_cdo_instruments_versions.get',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    list_synthetic_cdo_instruments_request: true,
    get_synthetic_cdo_instrument_request: true,
    get_many_synthetic_cdo_instruments_request: true,
    put_synthetic_cdo_instrument_request: true,
    put_many_synthetic_cdo_instruments_request: true,
    delete_synthetic_cdo_instrument_request: true,
    delete_many_synthetic_cdo_instruments_request: true,
    list_synthetic_cdo_instrument_versions_request: true,
    get_synthetic_cdo_instrument_version_request: true,
} as const;

/**
 * The subjects this resource's changes are announced on. One payload is
 * addressed by three subjects, because the last segment is the action the
 * payload reports.
 */
export const eventSubjects = {
    created: 'trading.v1.synthetic_cdo_instruments_events.created',
    updated: 'trading.v1.synthetic_cdo_instruments_events.updated',
    deleted: 'trading.v1.synthetic_cdo_instruments_events.deleted',
} as const;
